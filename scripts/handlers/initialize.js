/**
 * initialize.js
 * 
 * Handler for MCP initialization
 */

const { sendResponse } = require('../lib/utils');

/**
 * Handle MCP initialization
 * @param {Object} request - MCP request
 * @param {Object} config - Configuration object
 * @param {Object} logger - Logger instance
 */
function handleInitialize(request, config, logger) {
  logger.info('Handling initialize request');
  
  // Send successful initialization response
  const response = {
    jsonrpc: '2.0',
    id: request.id,
    result: {
      protocolVersion: request.params.protocolVersion || '0.1.0',
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

module.exports = {
  handleInitialize
};