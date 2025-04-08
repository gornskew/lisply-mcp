/**
 * utils.js
 * 
 * Utility functions for the MCP wrapper
 */

// Helper to send MCP responses
function sendResponse(response, logger) {
  const jsonResponse = JSON.stringify(response);
  logger.info(`Sending response: ${jsonResponse.substring(0, 500)}${jsonResponse.length > 500 ? '...' : ''}`);
  // Use process.stdout.write to ensure only JSON is output to stdout
  process.stdout.write(jsonResponse + '\n');
}

// Standard text response format
function sendTextResponse(requestOrId, text, logger) {
  logger.debug(`sendTextResponse called with: ${JSON.stringify(text)}`);
  // Check if text is an object and not a string
  if (typeof text === 'object' && text !== null) {
    logger.debug(`Text is an object, converting to string: ${JSON.stringify(text)}`);
    text = JSON.stringify(text, null, 2);
  }
  
  // Get ID either from request object or directly
  const id = typeof requestOrId === 'object' ? requestOrId.id : requestOrId;
  
  const response = {
    jsonrpc: '2.0',
    id: id,
    result: {
      content: [
        {
          type: 'text',
          text: String(text || '')
        }
      ]
    }
  };
  sendResponse(response, logger);
}

// Standard response for simple data
function sendStandardResponse(requestOrId, data, logger) {
  // Get ID either from request object or directly
  const id = typeof requestOrId === 'object' ? requestOrId.id : requestOrId;
  
  const response = {
    jsonrpc: '2.0',
    id: id,
    result: data
  };
  sendResponse(response, logger);
}

// Helper to send error responses
function sendErrorResponse(requestOrId, code, message, logger) {
  // Get ID either from request object or directly
  const id = typeof requestOrId === 'object' ? requestOrId.id : requestOrId;
  
  const response = {
    jsonrpc: '2.0',
    id: id,
    error: {
      code: code,
      message: message
    }
  };
  sendResponse(response, logger);
}

module.exports = {
  sendResponse,
  sendTextResponse,
  sendStandardResponse,
  sendErrorResponse
};