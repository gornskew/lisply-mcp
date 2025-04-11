#!/usr/bin/env node

// Check for required dependencies before importing any modules
const path = require('path');
const fs = require('fs');

// Helper function to install dependencies and restart the script
function installDependenciesAndRestart(contextMessage = 'Missing dependency detected') {
  console.error(`\n\x1b[33m${contextMessage}\x1b[0m`);
  console.error('\nAttempting to install dependencies automatically...\n');
  
  try {
    // Execute npm install synchronously
    const { execSync } = require('child_process');
    console.error('\x1b[36mRunning: npm install\x1b[0m');
    execSync('npm install', { cwd: __dirname, encoding: 'utf8' });
    
    console.error('\x1b[32mDependencies installed successfully!\x1b[0m');
    console.error('\nRestarting script with newly installed dependencies...\n');
    
    // Restart the script
    const { spawnSync } = require('child_process');
    const result = spawnSync(process.argv[0], process.argv.slice(1), { 
      stdio: 'inherit',
      cwd: process.cwd()
    });
    
    // Exit with the same code as the child process
    process.exit(result.status);
  } catch (installError) {
    console.error('\n\x1b[31mError: Failed to automatically install dependencies\x1b[0m');
    console.error(`\n${installError.message}\n`);
    console.error('Please install dependencies manually by running:\n');
    console.error('\x1b[33m  cd ' + __dirname + ' && npm install\x1b[0m\n');
    console.error('Then try running this script again.\n');
    return false;
  }
}

function checkDependencies() {
  // Check for external dependencies listed in package.json
  try {
    // Node built-in modules don't need to be checked
    // External modules needed for the package
    require.resolve('commander');
    
    // Try to read package.json to give more helpful error messages
    try {
      const packageJsonPath = path.join(__dirname, 'package.json');
      const packageData = JSON.parse(fs.readFileSync(packageJsonPath, 'utf8'));
      
      return true;
    } catch (fsError) {
      // If we can't read package.json, continue since we'll already
      // check for the critical dependencies
      return true;
    }
  } catch (e) {
    const missingModule = e.requireStack ? e.requireStack[0] : e.message.split("\"")[1];
    return installDependenciesAndRestart(`Missing dependency: ${missingModule}`);
  }
}

// Run dependency check
checkDependencies();

/**
 * mcp-wrapper.js
 * 
 * Copyright (C) 2025 Genworks
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 * 
 * This file is part of the Lisply Model Context Protocol integration.
 * 
 * Wrapper script for Lisply MCP integration with:
 * - Configurable backend host and port via environment variables or CLI arguments
 * - Docker container management for local deployments (using -i mode for automatic cleanup)
 * - Support for running inside a container or directly on the host
 * - Support for mounting volumes into the backend container
 */

// Now we can safely require the modules
try {
  const { program } = require('commander');
  const { createLogger } = require('./lib/logger');
  const { loadConfig, checkPortConflicts } = require('./lib/config');
  const { isDockerAvailable, cleanup, startBackendContainer, tryAttachToContainer } = require('./lib/docker');
  const { checkBackendAvailability, startMcpWrapper } = require('./lib/server');
  const handlers = require('./handlers');

  // Parse command line arguments
  program
      .option('-H, --backend-host <host>', 'Backend server host (default: 127.0.0.1)')
      .option('--swank-host-port <port>', 'SWANK port on host system (default: 4201)')
      .option('--http-host-port <port>', 'HTTP port on host system (default: 9081)')
      .option('--https-host-port <port>', 'HTTPS port on host system (default: 9444)')
      .option('--telnet-host-port <port>', 'TELNET port on host system (default: 4024)')
      .option('--http-port <port>', 'HTTP port inside container (default: 9080)')
      .option('--https-port <port>', 'HTTPS port inside container (default: 9443)')
      .option('--swank-port <port>', 'SWANK port inside container (default: 4200)')
      .option('--telnet-port <port>', 'TELNET port inside container (default: 4023)')
      .option('--image-base-name <n>', 'Base name for Docker image (default: dcooper8/gendl)')
      .option('--image-branch <branch>', 'Branch to use for Docker image (default: auto-detected)')
      .option('--docker-image <image>', 'Full Docker image for backend (overrides base name and branch)')
      .option('--lisp-impl <impl>', 'Lisp implementation to use, ccl or sbcl (default: ccl)')
      .option('--no-auto-start', 'Do not auto-start backend docker container if not running')
      .option('--docker-socket <path>', 'Path to docker socket (default: /var/run/docker.sock)')
      .option('--log-file <path>', 'Path to log file (default: /tmp/lisply-mcp-wrapper.log)')
      .option('--debug', 'Enable debug logging')
      .option('--mount <mounts...>', 'Mount volumes in format "src:dst" (can specify multiple times)', (value, previous) => previous.concat([value]), [])
      .option('--start-http', 'Start HTTP service in backend container (default: true)')
      .option('--start-https', 'Start HTTPS service in backend container (default: false)')
      .option('--start-swank', 'Start SWANK service in backend container (default: true)') // for CL-based servers
      .option('--start-telnet', 'Start TELNET service in backend container (default: false)')
      .option('--no-use-stdio', 'Disable stdio capability for local containers. When disabled, HTTP mode will always be used even when stdio mode is requested.')
      .option('--repl-prompt <pattern>', 'REPL prompt pattern to detect Lisp evaluation completion (default: ?)')
      .option('--eval-timeout <ms>', 'Timeout for Lisp evaluation in milliseconds (default: 30000)')
      .option('--endpoint-prefix <prefix>', 'Prefix for all endpoints (default: lisply)')
      .option('--lisp-eval-endpoint <n>', 'Endpoint name for Lisp evaluation (default: lisp-eval)')
      .option('--http-request-endpoint <n>', 'Endpoint name for HTTP requests (default: http-request)')
      .option('--ping-endpoint <n>', 'Endpoint name for ping (default: ping-lisp)')
      .parse(process.argv);

  // Load configuration
  const config = loadConfig(program);

  // Create logger
  const logger = createLogger(config);

  // Check port conflicts
  checkPortConflicts(config);

  // Initialize globals
  global.isMcpWrapperStarted = false;
  global.backendContainerName = null;
  global.dockerProcess = null;

  // Log configuration
  logger.info(`Starting MCP wrapper with backend host: ${config.BACKEND_HOST}, SWANK port: ${config.SWANK_HOST_PORT}, HTTP port: ${config.HTTP_HOST_PORT}`);
  logger.info(`Auto-start is ${config.AUTO_START ? 'enabled' : 'disabled'}, Docker image: ${config.options.dockerImage || 'auto-detected'}`);
  logger.info(`Stdio capability for local containers: ${config.NO_USE_STDIO ? 'disabled' : 'enabled'} (default: enabled), HTTP is default mode, Prompt: '${config.REPL_PROMPT}', Timeout: ${config.EVAL_TIMEOUT}ms`);
  logger.info(`Endpoint prefix: ${config.ENDPOINT_PREFIX}, Endpoints - Eval: ${config.EVAL_ENDPOINT}, HTTP: ${config.HTTP_REQUEST_ENDPOINT}, Ping: ${config.PING_ENDPOINT}`);
  if (config.ALL_MOUNTS.length > 0) {
    logger.info(`Configured mounts: ${config.ALL_MOUNTS.join(', ')}`);
  }

  // Check if Lisp implementation is supported
  if (!config.SUPPORTED_IMPLS.includes(config.LISP_IMPL)) {
    logger.warn(`Unsupported Lisp implementation: ${config.LISP_IMPL}, defaulting to ${config.DEFAULT_IMPL}`);
  }

  // Main promise chain
  try {
    checkBackendAvailability(config, logger)
      .then(available => {
        if (available) {
          logger.info(`Backend service already available at ${config.BACKEND_HOST}:${config.HTTP_HOST_PORT}`);
          startMcpWrapper(config, logger, handlers);
          return Promise.resolve(true);
        } else {
          const isLocalHost = (config.BACKEND_HOST === '127.0.0.1' || config.BACKEND_HOST === 'localhost');
          const dockerAvailable = isDockerAvailable(logger, config.DOCKER_SOCKET);
          
          if (config.AUTO_START && isLocalHost && dockerAvailable) {
            logger.info(`Backend server not available, attempting to start container`);
            return startBackendContainer(config, logger, checkBackendAvailability);
          } else {
            logger.error(`Cannot start backend container. Conditions not met:
    - Auto-start: ${config.AUTO_START}
    - Local host: ${isLocalHost}
    - Docker available: ${dockerAvailable}`);
            process.exit(1);
          }
        }
      })
      .then((containerStarted) => {
        if (!global.isMcpWrapperStarted) {
          startMcpWrapper(config, logger, handlers);
        }
      })
      .catch(error => {
        logger.error(`Failed to start MCP wrapper: ${error.message}`);
        if (error.stack) {
          logger.error(`Error stack: ${error.stack}`);
        }
        process.exit(1);
      });
  } catch (error) {
    // If we get here, it means an error occurred before the promises could be set up
    // This is likely due to a module not being available
    console.error('\n\x1b[31mError: Failed to start MCP wrapper\x1b[0m');
    console.error(`\n${error.message}\n`);
    
    // Check if this is a module not found error
    if (error.code === 'MODULE_NOT_FOUND') {
      installDependenciesAndRestart('Missing dependency detected during execution');
    } else {
      console.error('For troubleshooting, try:\n');
      console.error('1. Check that all dependencies are installed:\n');
      console.error('\x1b[33m  cd /projects/lisply-mcp/scripts && npm install\x1b[0m\n');
      console.error('2. Check the log file for more details on the error.\n');
    }
    
    // If we can't use the logger yet, at least print the stack trace to console
    if (error.stack) {
      console.error('\nError stack trace:');
      console.error(error.stack);
    }
    
    process.exit(1);
  }

  // Set up process events for cleanup
  process.on('SIGINT', async () => {
    logger.info('Received SIGINT signal - cleaning up and exiting');
    await cleanup(logger);
    process.exit(0);
  });

  process.on('SIGTERM', async () => {
    logger.info('Received SIGTERM signal - cleaning up and exiting');
    await cleanup(logger);
    process.exit(0);
  });

  // Catch unhandled promise rejections
  process.on('unhandledRejection', (reason, promise) => {
    try {
      logger.error(`Unhandled promise rejection: ${reason}`);
      if (reason instanceof Error && reason.stack) {
        logger.error(`Stack trace: ${reason.stack}`);
      }
      
      // If this looks like a module error, give specific advice
      if (reason instanceof Error && reason.code === 'MODULE_NOT_FOUND') {
        installDependenciesAndRestart('Missing dependency detected in promise');
      }
    } catch (e) {
      // If logger is not available, use console directly
      console.error(`Unhandled promise rejection: ${reason}`);
      if (reason instanceof Error && reason.stack) {
        console.error(`Stack trace: ${reason.stack}`);
      }
    }
  });
} catch (error) {
  // If we get here, it means an error occurred when requiring the modules
  console.error('\n\x1b[31mError: Failed to load required modules\x1b[0m');
  console.error(`\n${error.message}\n`);
  
  // Check if this is a module not found error
  if (error.code === 'MODULE_NOT_FOUND') {
    installDependenciesAndRestart('Missing module: ' + error.message.split('"')[1]);
  } else {
    console.error('For troubleshooting, try:\n');
    console.error('1. Check that all dependencies are installed:\n');
    console.error('\x1b[33m  cd ' + __dirname + ' && npm install\x1b[0m\n');
    console.error('2. Check if any module paths are incorrect.\n');
    
    // Print the stack trace to console
    if (error.stack) {
      console.error('\nError stack trace:');
      console.error(error.stack);
    }
    
    process.exit(1);
  }
}
