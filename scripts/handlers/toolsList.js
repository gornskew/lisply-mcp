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
 * toolsList.js 
 * 
 * Handler for tools/list requests
 */

const { getBackendConnectionInfo, makeHttpRequest } = require('../lib/server');
const { sendStandardResponse, sendErrorResponse } = require('./index');
const { createPrefixedToolName } = require('../lib/config');

function addSandboxAnnotations(tool, config) {
  if (!config.TRUST_AS_SANDBOX) return;

  const sandboxPrefix = `${config.SANDBOX_NOTE} `;
  tool.description = tool.description
    ? `${sandboxPrefix}${tool.description}`
    : config.SANDBOX_NOTE;

  tool.annotations = {
    ...(tool.annotations || {}),
    title: tool.annotations?.title || tool.name,
    readOnlyHint: false,
    destructiveHint: false,
    idempotentHint: false,
    openWorldHint: true
  };
}

/**
 * Handle tools/list request
 */
function handleToolsList(request, config, logger) {
  logger.info('Handling tools/list request');
  
  const { hostname, port } = getBackendConnectionInfo(config, logger);
  
  const options = {
    hostname,
    port,
    path: `${config.BASE_PATH}/tools/list`,
    method: 'GET'
  };
  
  makeHttpRequest(options, null, (error, response) => {
    if (error) {
      logger.error(`Error fetching tools list: ${error.message}`);
      sendErrorResponse(request, -32603, `Error fetching tools list: ${error.message}`, logger);
      return;
    }
    
    try {
      const toolsData = JSON.parse(response.content);
      
      if (!toolsData.tools || !Array.isArray(toolsData.tools)) {
        throw new Error('Invalid tools list response format');
      }
      
      // Add http_request tool if missing
      if (!toolsData.tools.some(t => t.name === 'http_request')) {
        toolsData.tools.push({
          name: 'http_request',
          description: 'Send an HTTP request to the specified path',
          inputSchema: {
            type: 'object',
            properties: {
              path: { type: 'string', description: 'The path to send the request to' },
              method: { type: 'string', description: 'The HTTP method to use (GET, POST, PUT, DELETE, etc.)' },
              body: { type: 'string', description: 'The request body content (for POST/PUT requests)' },
              content: { type: 'string', description: 'Alternative name for the request body content (for compatibility)' },
              headers: { type: 'object', description: 'Optional headers to include with the request' },
              rawResponse: { type: 'boolean', description: 'If true, return the full response object instead of just the content' }
            },
            required: ['path']
          }
        });
      }
      
      // Add documentation tools if missing
      if (!toolsData.tools.some(t => t.name === 'get_docs_list')) {
        toolsData.tools.push({
          name: 'get_docs_list',
          description: 'List available documentation from the backend server',
          inputSchema: { type: 'object', properties: {}, required: [] }
        });
      }
      
      if (!toolsData.tools.some(t => t.name === 'get_docs')) {
        toolsData.tools.push({
          name: 'get_docs',
          description: 'Get documentation content from the backend server',
          inputSchema: {
            type: 'object',
            properties: {
              id: { type: 'string', description: "Document ID to retrieve (e.g., 'claude-md', 'readme', 'yadd')" }
            },
            required: ['id']
          }
        });
      }
      
      // Mark backend tools as trusted sandbox tools before prefixing names.
      for (const tool of toolsData.tools) {
        addSandboxAnnotations(tool, config);
      }

      // Prefix all tool names with server name
      for (const tool of toolsData.tools) {
        const prefixed = createPrefixedToolName(config.SERVER_NAME, tool.name);
        if (prefixed !== tool.name) {
          logger.debug(`Prefixing tool: ${tool.name} -> ${prefixed}`);
          tool.name = prefixed;
        }
      }
      
      sendStandardResponse(request, toolsData, logger);
    } catch (parseError) {
      logger.error(`Error parsing tools list: ${parseError.message}`);
      sendErrorResponse(request, -32603, `Error parsing tools list: ${parseError.message}`, logger);
    }
  }, 'TOOLS-LIST', logger);
}

module.exports = { handleToolsList };
