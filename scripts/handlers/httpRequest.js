/**
 * httpRequest.js
 * 
 * Handler for HTTP request tool
 */

const { makeHttpRequest } = require('../lib/server');
const { sendTextResponse, sendErrorResponse } = require('../lib/utils');

/**
 * Handle HTTP request with support for all methods and bodies
 * @param {Object} request - MCP request
 * @param {Object} args - Tool arguments
 * @param {Object} config - Configuration object
 * @param {Object} logger - Logger instance
 */
async function handleHttpRequest(request, args, config, logger) {
  logger.info(`Handling http_request: ${JSON.stringify(args)}`);
  
  try {
    // Check for required path parameter
    if (!args.path) {
      sendErrorResponse(request, -32602, "Missing required parameter: path", logger);
      return;
    }
    
    // Prepare the request options
    const options = {
      hostname: config.BACKEND_HOST,
      port: config.HTTP_HOST_PORT,
      path: args.path,
      method: args.method || 'GET',
      headers: {
        'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8'
      }
    };
    
    // Add custom headers if provided
    if (args.headers && typeof args.headers === 'object') {
      options.headers = { ...options.headers, ...args.headers };
    }
    
    // If Content-Type is not specified for POST/PUT and we have a body, default to application/json
    if (['POST', 'PUT'].includes(options.method) && args.body && 
        !options.headers['Content-Type'] && !options.headers['content-type']) {
      options.headers['Content-Type'] = 'application/json';
    }
    
    makeHttpRequest(options, args.body, (error, response) => {
      if (error) {
        logger.error(`HTTP request error: ${error.message}`);
        sendErrorResponse(request, -32603, `Error making HTTP request: ${error.message}`, logger);
        return;
      }
      
      // If rawResponse is set, return the full response object
      if (args.rawResponse) {
        sendTextResponse(request, JSON.stringify(response, null, 2), logger);
      } else {
        // Otherwise, return just the content
        sendTextResponse(request, response.content, logger);
      }
    }, null, logger);
  } catch (error) {
    logger.error(`Error in http_request: ${error.message}`);
    sendErrorResponse(request, -32603, `Error making HTTP request: ${error.message}`, logger);
  }
}

module.exports = {
  handleHttpRequest
};