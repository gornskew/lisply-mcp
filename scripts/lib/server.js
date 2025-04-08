/**
 * server.js
 * 
 * HTTP server and MCP wrapper implementation
 */

const http = require('http');
const readline = require('readline');

/**
 * Check if the backend server is available
 * @param {Object} config - Configuration object
 * @param {Object} logger - Logger instance
 * @returns {Promise<boolean>} - Whether the backend is available
 */
function checkBackendAvailability(config, logger) {
  return new Promise((resolve) => {
    logger.info(`Checking backend HTTP service availability at ${config.BACKEND_HOST}:${config.HTTP_HOST_PORT}`);
    
    const options = {
      hostname: config.BACKEND_HOST,
      port: config.HTTP_HOST_PORT,
      path: config.PING_ENDPOINT,
      method: 'GET',
      timeout: 5000
    };

    const req = http.request(options, (res) => {
      let responseBody = '';

      res.on('data', (chunk) => {
        responseBody += chunk;
      });

      res.on('end', () => {
        if (res.statusCode === 200 && responseBody.length > 0) {
          logger.info(`Lisply service is available. Ping response: ${responseBody}`);
          resolve(true);
        } else {
          logger.warn(`Lisply service ping failed. Status: ${res.statusCode}, Response length: ${responseBody.length}`);
          resolve(false);
        }
      });
    });

    req.on('timeout', () => {
      logger.warn('Lisply service ping request timed out');
      req.destroy();
      resolve(false);
    });

    req.on('error', (error) => {
      logger.warn(`Lisply service ping request error: ${error.message}`);
      resolve(false);
    });

    req.end();
  });
}

/**
 * Helper function to make HTTP requests with unified logging and error handling
 * @param {Object} options - HTTP request options
 * @param {string|null} body - Request body
 * @param {Function} callback - Callback function for the response
 * @param {string} logPrefix - Optional prefix for log messages
 * @param {Object} logger - Logger instance
 */
function makeHttpRequest(options, body, callback, logPrefix = '', logger) {
  const prefix = logPrefix ? `[${logPrefix}] ` : '';
  logger.debug(`${prefix}Making ${options.method} request to http://${options.hostname}:${options.port}${options.path}`);
  logger.debug(`${prefix}Request headers: ${JSON.stringify(options.headers)}`);
  if (body) {
    logger.debug(`${prefix}Request body: ${body.substring(0, 500)}${body.length > 500 ? '...' : ''}`);
  }
  
  const req = http.request(options, (res) => {
    let data = '';
    
    res.on('data', (chunk) => {
      data += chunk;
      if (logPrefix) {
        logger.debug(`${prefix}Received chunk: ${chunk.toString().substring(0, 100)}...`);
      }
    });
    
    res.on('end', () => {
      logger.debug(`${prefix}Response status: ${res.statusCode}, Content-Type: ${res.headers['content-type']}`);
      logger.debug(`${prefix}Response data: ${data.substring(0, 500)}${data.length > 500 ? '...' : ''}`);
      
      // Follow redirects
      if ([301, 302, 303, 307, 308].includes(res.statusCode)) {
        const location = res.headers.location;
        
        if (location) {
          logger.info(`${prefix}Following redirect to: ${location}`);
          
          // Create new options for the redirect
          const redirectOptions = {
            ...options,
            path: location
          };
          
          // For 303 redirects, use GET
          if (res.statusCode === 303) {
            redirectOptions.method = 'GET';
          }
          
          // Follow the redirect
          return makeHttpRequest(redirectOptions, null, callback, logPrefix, logger);
        }
      }
      
      // Return a comprehensive response object
      callback(null, {
        content: data,
        statusCode: res.statusCode,
        headers: res.headers,
        finalUrl: options.path
      });
    });
  });
  
  req.on('error', (error) => {
    logger.error(`${prefix}HTTP request error: ${error.message}`);
    callback(error);
  });
  
  // Set timeout to prevent hanging
  req.setTimeout(10000, () => {
    logger.error(`${prefix}Request timed out after 10 seconds`);
    req.destroy();
    callback(new Error("Request timed out"));
  });
  
  // Send the request body if present
  if (body && ['POST', 'PUT', 'PATCH'].includes(options.method)) {
    req.write(body);
  }
  
  req.end();
  if (logPrefix) {
    logger.debug(`${prefix}Request sent`);
  }
}

/**
 * Set up event handlers for the process
 * @param {Object} rl - Readline interface
 * @param {Object} logger - Logger instance
 */
function setupProcessEvents(rl, logger) {
  // Function to clean up resources on exit
  const cleanup = async () => {
    logger.info('Cleanup started'); // Log when cleanup begins
    
    if (rl) rl.close();
    
    logger.info('Cleanup completed'); // Log when cleanup finishes
  };

  // Add event handlers to prevent unexpected termination
  process.on('SIGINT', async () => {
    logger.info('Received SIGINT signal - cleaning up and exiting');
    await cleanup();
    process.exit(0);
  });
  
  process.on('SIGTERM', async () => {
    logger.info('Received SIGTERM signal - cleaning up and exiting');
    await cleanup();
    process.exit(0);
  });

  // Add handler for normal exit (uncaught process exit)
  process.on('exit', (code) => {
    logger.info(`Process exiting with code ${code} - cleanup should have run`);
  });

  // Add handler for unhandled promise rejections to catch async issues
  process.on('unhandledRejection', (reason, promise) => {
    logger.error(`Unhandled promise rejection: ${reason}`);
  });
}

/**
 * Start the MCP wrapper server
 * @param {Object} config - Configuration object
 * @param {Object} logger - Logger instance
 * @param {Object} handlers - Object containing handler functions
 */
function startMcpWrapper(config, logger, handlers) {
  // Set flag to prevent double-starting
  global.isMcpWrapperStarted = true;
  
  logger.info('Starting MCP wrapper');
  
  // Create readline interface for stdin/stdout communication
  let rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    terminal: false
  });
  
  // Handle MCP requests
  rl.on('line', (line) => {
    logger.info(`Received: ${line}`);
    
    try {
      const request = JSON.parse(line);
      
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
          handlers.sendStandardResponse(request, { resources: [] });
          break;
        case 'prompts/list':
          handlers.sendStandardResponse(request, { prompts: [] });
          break;
        case 'notifications/initialized':
          // Just acknowledge notification, no response needed
          logger.info('Received initialization notification');
          break;
        default:
          // Send method not supported error for any other method
          logger.warn(`Unsupported method: ${request.method}`);
          handlers.sendErrorResponse(request, -32601, `Method not supported: ${request.method}`);
      }
    } catch (error) {
      logger.error(`Error processing request: ${error.message}`);
      if (line && line.includes('"id"')) {
        try {
          const id = JSON.parse(line).id;
          handlers.sendErrorResponse({ id }, -32603, `Internal error: ${error.message}`);
        } catch (e) {
          logger.error(`Could not extract request ID: ${e.message}`);
        }
      }
    }
  });
  
  // Handle process events
  setupProcessEvents(rl, logger);
  
  return rl;
}

module.exports = {
  checkBackendAvailability,
  makeHttpRequest,
  setupProcessEvents,
  startMcpWrapper
};