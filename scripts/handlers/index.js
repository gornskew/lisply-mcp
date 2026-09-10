/*
 * Copyright © 2026 Gornskew Enterprises
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.  Distributed WITHOUT
 * ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.
 */

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

// Tool-level error: a SUCCESSFUL JSON-RPC response whose result carries
// isError:true + visible text (MCP tool-error convention).  Use this for
// eval failures, unreachable backends, etc.  JSON-RPC protocol errors
// (sendErrorResponse) get flattened to a bare "Tool execution failed" by
// the claude.ai relay, hiding the actual message; isError results render.
function sendToolErrorResponse(requestOrId, text, logger) {
  const id = typeof requestOrId === 'object' ? requestOrId.id : requestOrId;

  sendResponse({
    jsonrpc: '2.0',
    id,
    result: {
      content: [{ type: 'text', text: String(text || 'Unknown error') }],
      isError: true
    }
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
  sendToolErrorResponse,
  sendErrorResponse
};

// Import and re-export handlers (after module.exports to break circular deps)
const { handleInitialize } = require('./initialize');
const { handleToolsList } = require('./toolsList');
const { handleToolCall } = require('./toolCall');
const { handleHttpRequest } = require('./httpRequest');
const { handleLisplySearch, handleSkewedSearch } = require('./lisplySearch');
const { handlePingLisp } = require('./ping');
const { handleLispEval } = require('./lispEval');

module.exports.handleInitialize = handleInitialize;
module.exports.handleToolsList = handleToolsList;
module.exports.handleToolCall = handleToolCall;
module.exports.handleHttpRequest = handleHttpRequest;
module.exports.handleLisplySearch = handleLisplySearch;
module.exports.handleSkewedSearch = handleSkewedSearch; // deprecated alias
module.exports.handlePingLisp = handlePingLisp;
module.exports.handleLispEval = handleLispEval;
