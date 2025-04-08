/**
 * toolCall.js
 * 
 * Handler for tools/call requests
 */

const { sendErrorResponse } = require('../lib/utils');
const { handleHttpRequest } = require('./httpRequest');
const { handlePingLisp } = require('./ping');
const { handleLispEval } = require('./lispEval');

/**
 * Handle tool calls
 * @param {Object} request - MCP request
 * @param {Object} config - Configuration object
 * @param {Object} logger - Logger instance
 */
async function handleToolCall(request, config, logger) {
  const toolName = request.params?.name;
  const args = request.params?.arguments || {};
  
  logger.info(`Handling tool call: ${toolName}`);
  
  try {
    // Special handling for each tool
    if (toolName === 'http_request') {
      return handleHttpRequest(request, args, config, logger);
    } else if (toolName === 'ping_lisp') {
      return handlePingLisp(request, config, logger);
    } else if (toolName === 'lisp_eval') {
      return handleLispEval(request, args, config, logger);
    // Removed KB query handler
    } else {
      // Unknown tool
      sendErrorResponse(request, -32601, `Unknown tool: ${toolName}`, logger);
    }
  } catch (error) {
    logger.error(`Error in tool call: ${error.message}`);
    sendErrorResponse(request, -32603, `Error calling tool: ${error.message}`, logger);
  }
}

module.exports = {
  handleToolCall
};