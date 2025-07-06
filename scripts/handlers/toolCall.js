/**
 * toolCall.js
 * 
 * Handler for tools/call requests
 */

const { sendErrorResponse } = require('../lib/utils');
const { handleHttpRequest } = require('./httpRequest');
const { handlePingLisp } = require('./ping');
const { handleLispEval } = require('./lispEval');
const { extractOriginalToolName } = require('../lib/config');
const { getBackendConnectionInfo, makeHttpRequest } = require('../lib/server');
const { sendStandardResponse } = require('../lib/utils');

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
  
  // Extract the original tool name by removing server prefix
  const originalToolName = extractOriginalToolName(toolName);
  logger.debug(`Extracted original tool name: ${originalToolName} from: ${toolName}`);
  
  try {
    // Special handling for each tool (using original tool name for routing)
    if (originalToolName === 'http_request') {
      return handleHttpRequest(request, args, config, logger);
    } else if (originalToolName === 'ping_lisp') {
      return handlePingLisp(request, config, logger);
    } else if (originalToolName === 'lisp_eval') {
      return handleLispEval(request, args, config, logger);
    } else if (originalToolName === 'get_docs_list') {
      return handleGetDocsList(request, config, logger);
    } else if (originalToolName === 'get_docs') {
      return handleGetDocs(request, args, config, logger);
    // Removed KB query handler
    } else {
      // Unknown tool
      sendErrorResponse(request, -32601, `Unknown tool: ${toolName} (original: ${originalToolName})`, logger);
    }
  } catch (error) {
    logger.error(`Error in tool call: ${error.message}`);
    sendErrorResponse(request, -32603, `Error calling tool: ${error.message}`, logger);
  }
}

/**
 * Handle get_docs_list tool calls
 * @param {Object} request - MCP request
 * @param {Object} config - Configuration object
 * @param {Object} logger - Logger instance
 */
async function handleGetDocsList(request, config, logger) {
  logger.info('Handling get_docs_list request');
  
  const { hostname, port } = getBackendConnectionInfo(config, logger);
  
  const options = {
    hostname,
    port,
    path: `${config.BASE_PATH}/docs/list`,
    method: 'GET'
  };
  
  makeHttpRequest(options, null, (error, response) => {
    if (error) {
      logger.error(`Error fetching docs list: ${error.message}`);
      sendErrorResponse(request, -32603, `Error fetching docs list: ${error.message}`, logger);
      return;
    }
    
    try {
      // Try to parse as JSON first, but fall back to plain text
      let content;
      try {
        content = JSON.parse(response.content);
      } catch (parseError) {
        content = response.content;
      }
      
      sendStandardResponse(request, {
        content: [{
          type: 'text',
          text: typeof content === 'string' ? content : JSON.stringify(content, null, 2)
        }]
      }, logger);
    } catch (responseError) {
      logger.error(`Error processing docs list response: ${responseError.message}`);
      sendErrorResponse(request, -32603, `Error processing docs list: ${responseError.message}`, logger);
    }
  }, 'GET-DOCS-LIST', logger);
}

/**
 * Handle get_docs tool calls
 * @param {Object} request - MCP request
 * @param {Object} args - Tool arguments
 * @param {Object} config - Configuration object
 * @param {Object} logger - Logger instance
 */
async function handleGetDocs(request, args, config, logger) {
  const { doc_id } = args;
  
  logger.info(`Handling get_docs request for doc_id: ${doc_id}`);
  
  if (!doc_id) {
    sendErrorResponse(request, -32602, 'Missing required parameter: doc_id', logger);
    return;
  }
  
  const { hostname, port } = getBackendConnectionInfo(config, logger);
  
  const options = {
    hostname,
    port,
    path: `${config.BASE_PATH}/docs/${encodeURIComponent(doc_id)}`,
    method: 'GET'
  };
  
  makeHttpRequest(options, null, (error, response) => {
    if (error) {
      logger.error(`Error fetching docs for ${doc_id}: ${error.message}`);
      sendErrorResponse(request, -32603, `Error fetching docs: ${error.message}`, logger);
      return;
    }
    
    try {
      sendStandardResponse(request, {
        content: [{
          type: 'text',
          text: response.content
        }]
      }, logger);
    } catch (responseError) {
      logger.error(`Error processing docs response: ${responseError.message}`);
      sendErrorResponse(request, -32603, `Error processing docs: ${responseError.message}`, logger);
    }
  }, 'GET-DOCS', logger);
}

module.exports = {
  handleToolCall
};