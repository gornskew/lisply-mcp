#!/usr/bin/env node

/*
 * Copyright (C) 2026 Gornskew Enterprises
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.  Distributed WITHOUT
 * ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.
 */

const path = require('path');
const fs = require('fs');

// Auto-install dependencies if needed
function ensureDependencies() {
  try {
    require.resolve('commander');
    return true;
  } catch (e) {
    console.error('Missing dependencies. Running npm install...');
    const { execSync } = require('child_process');
    try {
      execSync('npm install', { cwd: __dirname, stdio: ['ignore', 'pipe', 'pipe'] });
      console.error('Dependencies installed. Please restart.');
      process.exit(0);
    } catch (err) {
      console.error('Failed to install dependencies:', err.message);
      console.error('Please run: cd ' + __dirname + ' && npm install');
      process.exit(1);
    }
  }
}

ensureDependencies();

const { program } = require('commander');
const { createLogger } = require('./lib/logger');
const { loadConfig } = require('./lib/config');
const { checkBackendAvailability, startMcpWrapper } = require('./lib/server');
const handlers = require('./handlers');

// Parse command line arguments
program
    .option('-H, --backend-host <host>', 'Backend server host (default: 127.0.0.1)')
    .option('--http-host-port <port>', 'HTTP port on host system (default: 9081)')
    .option('--http-port <port>', 'HTTP port inside container (default: 9080)')
    .option('--swank-host-port <port>', 'SWANK port on host system (for documentation)')
    .option('--swank-port <port>', 'SWANK port inside container (for documentation)')
    .option('--log-file <path>', 'Path to log file (default: /tmp/lisply-mcp-wrapper.log)')
    .option('--debug', 'Enable debug logging')
    .option('--endpoint-prefix <prefix>', 'Prefix for all endpoints (default: lisply)')
    .option('--lisp-eval-endpoint <n>', 'Endpoint name for Lisp evaluation (default: lisp-eval)')
    .option('--http-request-endpoint <n>', 'Endpoint name for HTTP requests (default: http-request)')
    .option('--ping-endpoint <n>', 'Endpoint name for ping (default: ping-lisp)')
    .option('--server-name <n>', 'MCP server name for tool prefixing (default: lisply-mcp)')
    .option('--eval-timeout <ms>', 'Timeout for Lisp evaluation in milliseconds (default: 30000)')
    .option('--request-timeout-ms <ms>', 'Timeout for backend HTTP requests in milliseconds (default: 10000)')
    .option('--admin-secret <secret>', 'Secret sent as an admin auth header on every backend request (e.g. a gated Cyclops); overridable via LISPLY_ADMIN_SECRET / ADMIN_SECRET env')
    .option('--admin-secret-header <name>', 'Header name for --admin-secret (default: X-Cyclops-Admin-Secret)')
    .parse(process.argv);

// Load configuration
const config = loadConfig(program);

// Create logger
const logger = createLogger(config);

// Log startup info
logger.info('='.repeat(60));
logger.info('Lisply MCP Wrapper starting');
logger.info('='.repeat(60));
logger.info(`Server name: ${config.SERVER_NAME}`);
logger.info(`Backend: ${config.BACKEND_HOST}:${config.HTTP_HOST_PORT} (host) / :${config.HTTP_PORT} (container)`);
logger.info(`Endpoints: ${config.EVAL_ENDPOINT}, ${config.HTTP_REQUEST_ENDPOINT}, ${config.PING_ENDPOINT}`);

// Main: check backend availability and start MCP server
checkBackendAvailability(config, logger)
  .then(available => {
    if (available) {
      logger.info('Backend service is available - starting MCP server');
      startMcpWrapper(config, logger, handlers);
    } else {
      logger.error('Backend service is NOT available');
      logger.error(`Expected backend at: ${config.BACKEND_HOST}:${config.HTTP_HOST_PORT}`);
      logger.error('');
      logger.error('Please ensure the docker-compose services are running:');
      logger.error('  cd ~/projects/skewed-emacs && ./compose-dev up');
      logger.error('');
      process.exit(1);
    }
  })
  .catch(error => {
    logger.error(`Fatal error: ${error.message}`);
    if (error.stack) {
      logger.error(error.stack);
    }
    process.exit(1);
  });

// Handle signals gracefully
process.on('SIGINT', () => {
  logger.info('Received SIGINT - exiting');
  process.exit(0);
});

process.on('SIGTERM', () => {
  logger.info('Received SIGTERM - exiting');
  process.exit(0);
});

process.on('unhandledRejection', (reason, promise) => {
  logger.error(`Unhandled promise rejection: ${reason}`);
});

// A dead wrapper is the worst failure mode (client sees a permanently
// unresponsive MCP server).  Individual request state is self-contained,
// so log and keep serving rather than crashing.
process.on('uncaughtException', (error) => {
  logger.error(`Uncaught exception: ${error.message}`);
  if (error.stack) {
    logger.error(error.stack);
  }
});
