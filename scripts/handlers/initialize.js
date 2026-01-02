/**
 * initialize.js
 * 
 * Handler for MCP initialization
 */

const { sendResponse } = require('./index');

/**
 * Handle MCP initialize request
 */
function handleInitialize(request, config, logger) {
  logger.info('Handling initialize request');
  
  const response = {
    jsonrpc: '2.0',
    id: request.id,
    result: {
      protocolVersion: request.params?.protocolVersion || '0.1.0',
      capabilities: {
        experimental: {},
        prompts: { listChanged: false },
        resources: { subscribe: false, listChanged: false },
        tools: { listChanged: false }
      },
      serverInfo: {
        name: config.SERVER_NAME,
        version: config.VERSION
      }
    }
  };
  
  sendResponse(response, logger);
  logger.info('Initialization complete');
}

module.exports = { handleInitialize };
