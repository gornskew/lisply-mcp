/**
 * toolsList.js 
 * 
 * Handler for tools/list requests
 */

const { getBackendConnectionInfo, makeHttpRequest } = require('../lib/server');
const { sendStandardResponse, sendErrorResponse } = require('./index');
const { createPrefixedToolName } = require('../lib/config');

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
      
      // Add mode parameter to lisp_eval if present
      for (const tool of toolsData.tools) {
        if (tool.name === 'lisp_eval' && tool.inputSchema?.properties) {
          if (!tool.inputSchema.properties.mode) {
            tool.inputSchema.properties.mode = {
              type: 'string',
              description: 'The mode to use to talk to Gendl, either http (default) or stdio.\nStdio will only be respected for local Gendl containers started by the MCP server itself.'
            };
          }
        }
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
