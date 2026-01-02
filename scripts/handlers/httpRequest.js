/**
 * httpRequest.js 
 * 
 * Handler for HTTP request tool
 */

const { getBackendConnectionInfo, makeHttpRequest } = require('../lib/server');
const { sendTextResponse, sendErrorResponse } = require('./index');

/**
 * Handle HTTP request tool
 */
function handleHttpRequest(request, args, config, logger) {
  logger.info(`Handling http_request: ${JSON.stringify(args)}`);
  
  if (!args.path) {
    sendErrorResponse(request, -32602, 'Missing required parameter: path', logger);
    return;
  }
  
  const { hostname, port } = getBackendConnectionInfo(config, logger);
  
  const options = {
    hostname,
    port,
    path: args.path,
    method: args.method || 'GET',
    timeoutMs: config.REQUEST_TIMEOUT_MS,
    headers: {
      'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8'
    }
  };
  
  // Support both 'body' and 'content' parameters
  const requestBody = args.content !== undefined ? args.content : args.body;
  
  // Add custom headers
  if (args.headers && typeof args.headers === 'object') {
    options.headers = { ...options.headers, ...args.headers };
  }
  
  // Default Content-Type for POST/PUT with body
  if (['POST', 'PUT'].includes(options.method) && requestBody && 
      !options.headers['Content-Type'] && !options.headers['content-type']) {
    options.headers['Content-Type'] = 'application/json';
  }
  
  if (requestBody && !options.headers['Content-Length'] && !options.headers['content-length']) {
    options.headers['Content-Length'] = Buffer.byteLength(requestBody);
  }
  
  makeHttpRequest(options, requestBody, (error, response) => {
    if (error) {
      logger.error(`HTTP request error: ${error.message}`);
      sendErrorResponse(request, -32603, `Error making HTTP request: ${error.message}`, logger);
      return;
    }
    
    if (args.rawResponse) {
      sendTextResponse(request, JSON.stringify(response, null, 2), logger);
    } else {
      sendTextResponse(request, response.content, logger);
    }
  }, 'HTTP-REQUEST', logger);
}

module.exports = { handleHttpRequest };
