/**
 * server.js 
 * 
 * HTTP client and MCP server implementation.
 * No container management - just protocol handling.
 */

const http = require('http');
const readline = require('readline');

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
 * Make HTTP request to backend
 */
function makeHttpRequest(options, body, callback, logPrefix, logger) {
  const prefix = logPrefix ? `[${logPrefix}] ` : '';
  const timeoutMs = Number.isFinite(options.timeoutMs) ? options.timeoutMs : 10000;
  
  logger.debug(`${prefix}${options.method} http://${options.hostname}:${options.port}${options.path}`);
  if (body) {
    logger.debug(`${prefix}Body: ${body.substring(0, 200)}${body.length > 200 ? '...' : ''}`);
  }
  
  const req = http.request(options, (res) => {
    let data = '';
    
    res.on('data', chunk => data += chunk);
    
    res.on('end', () => {
      logger.debug(`${prefix}Response: ${res.statusCode}, ${data.substring(0, 200)}${data.length > 200 ? '...' : ''}`);
      
      // Follow redirects
      if ([301, 302, 303, 307, 308].includes(res.statusCode) && res.headers.location) {
        logger.info(`${prefix}Following redirect to: ${res.headers.location}`);
        const redirectOptions = { ...options, path: res.headers.location };
        if (res.statusCode === 303) redirectOptions.method = 'GET';
        return makeHttpRequest(redirectOptions, null, callback, logPrefix, logger);
      }
      
      callback(null, {
        content: data,
        statusCode: res.statusCode,
        headers: res.headers,
        finalUrl: options.path
      });
    });
  });
  
  req.on('error', (error) => {
    logger.error(`${prefix}Request error: ${error.message}`);
    callback(error);
  });
  
  req.setTimeout(timeoutMs, () => {
    logger.error(`${prefix}Request timed out`);
    req.destroy();
    callback(new Error('Request timed out'));
  });
  
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
  startMcpWrapper
};
