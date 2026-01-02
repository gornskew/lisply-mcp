/**
 * ping.js 
 * 
 * Handler for ping_lisp tool
 */

const { getBackendConnectionInfo, makeHttpRequest } = require('../lib/server');
const { sendTextResponse, sendErrorResponse } = require('./index');

/**
 * Handle ping_lisp tool
 */
function handlePingLisp(request, config, logger) {
  logger.info('Handling ping_lisp request');
  
  const { hostname, port } = getBackendConnectionInfo(config, logger);
  
  const options = {
    hostname,
    port,
    path: config.PING_ENDPOINT,
    method: 'GET'
  };
  
  makeHttpRequest(options, null, (error, response) => {
    if (error) {
      logger.error(`Ping error: ${error.message}`);
      sendErrorResponse(request, -32603, `Error pinging backend: ${error.message}`, logger);
      return;
    }
    
    sendTextResponse(request, response.content, logger);
  }, 'PING', logger);
}

module.exports = { handlePingLisp };
