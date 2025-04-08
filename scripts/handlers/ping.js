/**
 * ping.js
 * 
 * Handler for ping_lisp tool
 */

const { makeHttpRequest } = require('../lib/server');
const { sendTextResponse, sendErrorResponse } = require('../lib/utils');

/**
 * Handle simple ping_lisp tool
 * @param {Object} request - MCP request
 * @param {Object} config - Configuration object
 * @param {Object} logger - Logger instance
 */
function handlePingLisp(request, config, logger) {
  logger.info('Handling ping_lisp request');
  
  const options = {
    hostname: config.BACKEND_HOST,
    port: config.HTTP_HOST_PORT,
    path: config.PING_ENDPOINT,
    method: 'GET'
  };
  
  makeHttpRequest(options, null, (error, response) => {
    if (error) {
      logger.error(`Error pinging backend server: ${error.message}`);
      sendErrorResponse(request, -32603, `Error pinging backend server: ${error.message}`, logger);
      return;
    }
    
    // Just return the raw text response
    sendTextResponse(request, response.content, logger);
  }, 'PING', logger);
}

module.exports = {
  handlePingLisp
};