/*
 * Copyright © 2026 Gornskew Enterprises
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.  Distributed WITHOUT
 * ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.
 */

/**
 * server.js
 *
 * HTTP client and MCP server implementation.
 * No container management - just protocol handling.
 *
 * Resilience notes (2026-07-26):
 * - All backend requests go through a dedicated keep-alive Agent
 *   (backendAgent).  On ANY request failure (socket error, idle
 *   timeout, or watchdog) the agent is destroyed and re-created, so
 *   dead pooled sockets are never reused after a backend restart.
 * - Every request carries a wall-clock watchdog independent of
 *   socket events, guaranteeing a JSON-RPC response reaches the
 *   client even when socket-level timeouts fail to fire (observed
 *   after container restarts: client waits its full timeout, e.g.
 *   240s on Claude Desktop).  Hard-capped at HARD_DEADLINE_MS.
 * - A once-guard prevents double callbacks (previously possible:
 *   req.destroy() after timeout also emitted 'error').
 */

const http = require('http');
const readline = require('readline');

/** Absolute per-request wall-clock ceiling (fail-fast bound). */
const HARD_DEADLINE_MS = 60000;

/** Dedicated agent for backend connections (resettable). */
let backendAgent = createBackendAgent();

/** Optional admin auth header injected on every backend request.
 *  Set once from config in startMcpWrapper; null means "no header". */
let adminHeaderName = null;
let adminSecret = null;

function createBackendAgent() {
  return new http.Agent({ keepAlive: true, maxSockets: 8 });
}

/**
 * Tear down all pooled/keep-alive backend sockets and start fresh.
 * Called on any request failure so a restarted backend gets a
 * brand-new connection instead of stale dead state.
 */
function resetBackendAgent(logger, reason) {
  try { backendAgent.destroy(); } catch (e) { /* ignore */ }
  backendAgent = createBackendAgent();
  if (logger) {
    logger.warn(`Backend HTTP agent reset (${reason}); next request opens a fresh connection`);
  }
}

/**
 * Determine the hostname and port for backend connection
 */
function getBackendConnectionInfo(config, logger) {
  const hostname = config.BACKEND_HOST;

  // If connecting to localhost, use host port; otherwise use internal port
  const isLocalhost = hostname === 'localhost' || hostname === '127.0.0.1';
  const port = isLocalhost ? config.HTTP_HOST_PORT : config.HTTP_PORT;

  logger.debug(`Backend connection: ${hostname}:${port} (localhost=${isLocalhost})`);
  return { hostname, port };
}

/**
 * Check if backend service is available via HTTP ping
 */
function checkBackendAvailability(config, logger) {
  return new Promise((resolve) => {
    const { hostname, port } = getBackendConnectionInfo(config, logger);

    logger.info(`Checking backend at ${hostname}:${port}${config.PING_ENDPOINT}`);

    const options = {
      hostname,
      port,
      path: config.PING_ENDPOINT,
      method: 'GET',
      timeout: 5000
    };

    const req = http.request(options, (res) => {
      let body = '';
      res.on('data', chunk => body += chunk);
      res.on('end', () => {
        if (res.statusCode === 200 && body.length > 0) {
          logger.info(`Backend available. Response: ${body.trim()}`);
          resolve(true);
        } else {
          logger.warn(`Backend ping failed. Status: ${res.statusCode}`);
          resolve(false);
        }
      });
    });

    req.on('timeout', () => {
      logger.warn('Backend ping timed out');
      req.destroy();
      resolve(false);
    });

    req.on('error', (error) => {
      logger.warn(`Backend ping error: ${error.message}`);
      resolve(false);
    });

    req.end();
  });
}

/**
 * Make HTTP request to backend.
 *
 * Guarantees exactly one callback invocation, within
 * min(max(timeoutMs + 5000, 15000), HARD_DEADLINE_MS) wall-clock
 * time, regardless of socket behavior.  On any failure the backend
 * agent is reset so the next request re-establishes the connection.
 */
function makeHttpRequest(options, body, callback, logPrefix, logger) {
  const prefix = logPrefix ? `[${logPrefix}] ` : '';
  const timeoutMs = Number.isFinite(options.timeoutMs) ? options.timeoutMs : 10000;
  const hardMs = Math.min(Math.max(timeoutMs + 5000, 15000), HARD_DEADLINE_MS);

  logger.debug(`${prefix}${options.method} http://${options.hostname}:${options.port}${options.path} (idle ${timeoutMs}ms, hard ${hardMs}ms)`);
  if (body) {
    logger.debug(`${prefix}Body: ${body.substring(0, 200)}${body.length > 200 ? '...' : ''}`);
  }

  let finished = false;
  let watchdog = null;

  function finish(error, result) {
    if (finished) return;
    finished = true;
    if (watchdog) { clearTimeout(watchdog); watchdog = null; }
    callback(error, result);
  }

  // Inject the admin auth header (e.g. for a gated Cyclops) on every
  // backend request, including redirect hand-offs, without each handler
  // needing to know about it.
  if (adminSecret) {
    options = { ...options,
                headers: { ...(options.headers || {}), [adminHeaderName]: adminSecret } };
  }
  const req = http.request({ ...options, agent: backendAgent, timeout: timeoutMs }, (res) => {
    let data = '';

    res.on('data', chunk => data += chunk);

    res.on('end', () => {
      logger.debug(`${prefix}Response: ${res.statusCode}, ${data.substring(0, 200)}${data.length > 200 ? '...' : ''}`);

      // Follow redirects (hand off to a fresh request with its own watchdog)
      if ([301, 302, 303, 307, 308].includes(res.statusCode) && res.headers.location) {
        if (finished) return;
        finished = true;
        if (watchdog) { clearTimeout(watchdog); watchdog = null; }
        logger.info(`${prefix}Following redirect to: ${res.headers.location}`);
        const redirectOptions = { ...options, path: res.headers.location };
        if (res.statusCode === 303) redirectOptions.method = 'GET';
        return makeHttpRequest(redirectOptions, null, callback, logPrefix, logger);
      }

      finish(null, {
        content: data,
        statusCode: res.statusCode,
        headers: res.headers,
        finalUrl: options.path
      });
    });

    res.on('error', (error) => {
      logger.error(`${prefix}Response error: ${error.message}`);
      resetBackendAgent(logger, `response error: ${error.message}`);
      finish(new Error(`Backend response failed (${error.message}); if the backend just restarted, retry shortly`));
    });
  });

  req.on('error', (error) => {
    const msg = error.message || error.code || String(error);
    logger.error(`${prefix}Request error: ${msg}`);
    resetBackendAgent(logger, `request error: ${msg}`);
    finish(new Error(`Backend request failed (${msg}); if the backend just restarted, retry shortly`));
  });

  req.setTimeout(timeoutMs, () => {
    logger.error(`${prefix}Request idle timeout after ${timeoutMs}ms`);
    resetBackendAgent(logger, `idle timeout after ${timeoutMs}ms`);
    try { req.destroy(); } catch (e) { /* ignore */ }
    finish(new Error(`Backend unresponsive (no socket activity for ${timeoutMs}ms); it may be restarting - retry shortly`));
  });

  watchdog = setTimeout(() => {
    logger.error(`${prefix}Hard watchdog fired after ${hardMs}ms - forcing failure response`);
    resetBackendAgent(logger, `hard watchdog after ${hardMs}ms`);
    try { req.destroy(); } catch (e) { /* ignore */ }
    finish(new Error(`Backend unresponsive (no reply within ${hardMs}ms hard deadline); it may be restarting - retry shortly`));
  }, hardMs);

  if (body && ['POST', 'PUT', 'PATCH'].includes(options.method)) {
    req.write(body);
  }

  req.end();
}

/**
 * Start MCP wrapper - stdio JSON-RPC server
 */
function startMcpWrapper(config, logger, handlers) {
  logger.info('Starting MCP stdio server');

  if (config.ADMIN_SECRET) {
    adminHeaderName = config.ADMIN_SECRET_HEADER;
    adminSecret = config.ADMIN_SECRET;
    logger.info(`Admin auth header enabled: ${adminHeaderName} (value hidden)`);
  }

  process.stdin.setEncoding('utf8');

  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    terminal: false
  });

  rl.on('line', (line) => {
    logger.debug(`Received: ${line}`);

    try {
      const request = JSON.parse(line);

      // Handle notifications (no response needed)
      if (request.method && request.method.startsWith('notifications/')) {
        logger.debug(`Notification: ${request.method}`);
        return;
      }

      // Handle methods
      switch (request.method) {
        case 'initialize':
          handlers.handleInitialize(request, config, logger);
          break;
        case 'tools/call':
          handlers.handleToolCall(request, config, logger);
          break;
        case 'tools/list':
          handlers.handleToolsList(request, config, logger);
          break;
        case 'resources/list':
          handlers.sendStandardResponse(request.id, { resources: [] }, logger);
          break;
        case 'prompts/list':
          handlers.sendStandardResponse(request.id, { prompts: [] }, logger);
          break;
        default:
          logger.warn(`Unsupported method: ${request.method}`);
          handlers.sendErrorResponse(request.id, -32601, `Method not supported: ${request.method}`, logger);
      }
    } catch (error) {
      logger.error(`Error processing request: ${error.message}`);
      try {
        const id = JSON.parse(line).id;
        handlers.sendErrorResponse(id, -32603, `Internal error: ${error.message}`, logger);
      } catch (e) {
        logger.error(`Could not send error response: ${e.message}`);
      }
    }
  });

  rl.on('close', () => {
    logger.info('stdin closed - exiting');
    process.exit(0);
  });

  return rl;
}

module.exports = {
  getBackendConnectionInfo,
  checkBackendAvailability,
  makeHttpRequest,
  resetBackendAgent,
  startMcpWrapper
};
