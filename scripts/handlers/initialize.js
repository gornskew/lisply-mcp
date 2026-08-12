/*
 * Copyright (C) 2026 Gornskew Enterprises
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.  Distributed WITHOUT
 * ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.
 */

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
