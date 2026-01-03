/**
 * index.js
 * 
 * Handler exports and response utilities
 */

// Response utilities (inline to avoid circular deps)

function sendResponse(response, logger) {
  const json = JSON.stringify(response);
  logger.debug(`Sending: ${json.substring(0, 300)}${json.length > 300 ? '...' : ''}`);
  process.stdout.write(json + '\n');
}

function sendTextResponse(requestOrId, text, logger) {
  const id = typeof requestOrId === 'object' ? requestOrId.id : requestOrId;
  
  if (typeof text === 'object' && text !== null) {
    text = JSON.stringify(text, null, 2);
  }
  
  sendResponse({
    jsonrpc: '2.0',
    id,
    result: {
      content: [{ type: 'text', text: String(text || '') }]
    }
  }, logger);
}

function sendStandardResponse(requestOrId, data, logger) {
  const id = typeof requestOrId === 'object' ? requestOrId.id : requestOrId;
  
  sendResponse({
    jsonrpc: '2.0',
    id,
    result: data
  }, logger);
}

function sendErrorResponse(requestOrId, code, message, logger) {
  const id = typeof requestOrId === 'object' ? requestOrId.id : requestOrId;
  
  sendResponse({
    jsonrpc: '2.0',
    id,
    error: { code, message }
  }, logger);
}

// Export response utilities
module.exports = {
  sendResponse,
  sendTextResponse,
  sendStandardResponse,
  sendErrorResponse
};

// Import and re-export handlers (after module.exports to break circular deps)
const { handleInitialize } = require('./initialize');
const { handleToolsList } = require('./toolsList');
const { handleToolCall } = require('./toolCall');
const { handleHttpRequest } = require('./httpRequest');
const { handleSkewedSearch } = require('./skewedSearch');
const { handlePingLisp } = require('./ping');
const { handleLispEval } = require('./lispEval');

module.exports.handleInitialize = handleInitialize;
module.exports.handleToolsList = handleToolsList;
module.exports.handleToolCall = handleToolCall;
module.exports.handleHttpRequest = handleHttpRequest;
module.exports.handleSkewedSearch = handleSkewedSearch;
module.exports.handlePingLisp = handlePingLisp;
module.exports.handleLispEval = handleLispEval;
