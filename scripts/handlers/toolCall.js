/**
 * toolCall.js (v2)
 * 
 * Handler for tools/call requests
 */

const { extractOriginalToolName } = require('../lib/config');
const { getBackendConnectionInfo, makeHttpRequest } = require('../lib/server');
const { sendErrorResponse, sendStandardResponse } = require('./index');
const { handleHttpRequest } = require('./httpRequest');
const { handleSkewedSearch } = require('./skewedSearch');
const { handlePingLisp } = require('./ping');
const { handleLispEval } = require('./lispEval');

/**
 * Handle tool calls
 */
function handleToolCall(request, config, logger) {
  const toolName = request.params?.name;
  const args = request.params?.arguments || {};
  
  logger.info(`Handling tool call: ${toolName}`);
  
  // Extract original tool name (remove server prefix)
  const originalToolName = extractOriginalToolName(toolName);
  logger.debug(`Original tool name: ${originalToolName}`);
  
  try {
    switch (originalToolName) {
      case 'lisp_eval':
        return handleLispEval(request, args, config, logger);
      case 'http_request':
        return handleHttpRequest(request, args, config, logger);
      case 'ping_lisp':
        return handlePingLisp(request, config, logger);
      case 'get_docs_list':
        return handleGetDocsList(request, config, logger);
      case 'get_docs':
        return handleGetDocs(request, args, config, logger);
      case 'skewed_search':
        return handleSkewedSearch(request, args, config, logger);
      default:
        // Tools beyond the wrapper's native set may be advertised by
        // the backend in its /tools/list; forward such calls to the
        // backend's generic tools/call endpoint.
        return handleBackendTool(request, originalToolName, args, config, logger);
    }
  } catch (error) {
    logger.error(`Tool call error: ${error.message}`);
    sendErrorResponse(request, -32603, `Error calling tool: ${error.message}`, logger);
  }
}

/**
 * Forward a tool call to the backend's generic tools/call endpoint.
 *
 * Backends may advertise additional tools (beyond the lisply
 * baseline) in their /tools/list response. Calls to such tools are
 * forwarded as POST {BASE_PATH}/tools/call with body
 * {"name": <tool>, "arguments": {...}}. The backend responds with an
 * MCP-style result object: {"content": [...], ("isError": true)} --
 * passed through to the client unchanged, so backends can return any
 * MCP content type including image blocks.
 */
function handleBackendTool(request, toolName, args, config, logger) {
  logger.info(`Forwarding tool call to backend: ${toolName}`);

  const { hostname, port } = getBackendConnectionInfo(config, logger);
  const body = JSON.stringify({ name: toolName, arguments: args });

  const options = {
    hostname,
    port,
    path: `${config.BASE_PATH}/tools/call`,
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'Content-Length': Buffer.byteLength(body)
    }
  };

  makeHttpRequest(options, body, (error, response) => {
    if (error) {
      sendErrorResponse(request, -32603,
        `Error calling backend tool ${toolName}: ${error.message}`, logger);
      return;
    }
    try {
      const result = JSON.parse(response.content);
      if (!result.content || !Array.isArray(result.content)) {
        throw new Error('Backend tool result missing content array');
      }
      sendStandardResponse(request, result, logger);
    } catch (parseError) {
      sendErrorResponse(request, -32603,
        `Error parsing backend tool result for ${toolName}: ${parseError.message}`, logger);
    }
  }, 'BACKEND-TOOL', logger);
}

/**
 * Handle get_docs_list tool
 */
function handleGetDocsList(request, config, logger) {
  logger.info('Handling get_docs_list');
  
  const { hostname, port } = getBackendConnectionInfo(config, logger);
  
  const options = {
    hostname,
    port,
    path: `${config.BASE_PATH}/docs/list`,
    method: 'GET'
  };
  
  makeHttpRequest(options, null, (error, response) => {
    if (error) {
      sendErrorResponse(request, -32603, `Error fetching docs list: ${error.message}`, logger);
      return;
    }
    
    let content;
    try {
      content = JSON.parse(response.content);
    } catch (e) {
      content = response.content;
    }
    
    sendStandardResponse(request, {
      content: [{
        type: 'text',
        text: typeof content === 'string' ? content : JSON.stringify(content, null, 2)
      }]
    }, logger);
  }, 'GET-DOCS-LIST', logger);
}

/**
 * Handle get_docs tool
 */
function handleGetDocs(request, args, config, logger) {
  const { id } = args;
  
  logger.info(`Handling get_docs for: ${id}`);
  
  if (!id) {
    sendErrorResponse(request, -32602, 'Missing required parameter: id', logger);
    return;
  }
  
  const { hostname, port } = getBackendConnectionInfo(config, logger);
  
  const options = {
    hostname,
    port,
    path: `${config.BASE_PATH}/docs/${encodeURIComponent(id)}`,
    method: 'GET'
  };
  
  makeHttpRequest(options, null, (error, response) => {
    if (error) {
      sendErrorResponse(request, -32603, `Error fetching docs: ${error.message}`, logger);
      return;
    }
    
    sendStandardResponse(request, {
      content: [{
        type: 'text',
        text: response.content
      }]
    }, logger);
  }, 'GET-DOCS', logger);
}

module.exports = { handleToolCall };
