/**
 * toolsList.js
 * 
 * Handler for tools/list requests
 */



/**
 * toolsList.js
 *
 * Handler for tools/list requests
 */

const { makeHttpRequest } = require('../lib/server');
const { sendStandardResponse, sendErrorResponse } = require('../lib/utils');
const { createPrefixedToolName } = require('../lib/config');

/**
 * Handle tool list requests
 * @param {Object} request - MCP request
 * @param {Object} config - Configuration object
 * @param {Object} logger - Logger instance
 */
function handleToolsList(request, config, logger) {
  logger.info('Handling tools/list request');

  const options = {
    hostname: config.BACKEND_HOST,
    port: config.HTTP_HOST_PORT,
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

      // Validate the response structure
      if (!toolsData.tools || !Array.isArray(toolsData.tools)) {
        throw new Error('Invalid tools list response format');
      }

      // Add 'mode' parameter to lisp_eval tool if it exists
      for (let i = 0; i < toolsData.tools.length; i++) {
        const tool = toolsData.tools[i];
        
        if (tool.name === 'lisp_eval' && 
            tool.inputSchema && 
            tool.inputSchema.properties) {
          
          // Add mode parameter if it doesn't exist
          if (!tool.inputSchema.properties.mode) {
            logger.debug('Adding "mode" parameter to lisp_eval tool definition');
            
            tool.inputSchema.properties.mode = {
              "type": "string",
              "description": "The mode to use to talk to Gendl, either http (default) or stdio.\nStdio will only be respected for local Gendl containers started by the MCP server itself."
            };
          }
        }
      }
      
      // Add http_request tool if it doesn't exist
      const hasHttpRequestTool = toolsData.tools.some(tool => tool.name === 'http_request');
      
      if (!hasHttpRequestTool) {
        logger.debug('Adding http_request tool to tools list');
        
        toolsData.tools.push({
          "name": "http_request",
          "description": "Send an HTTP request to the specified path",
          "inputSchema": {
            "type": "object",
            "properties": {
              "path": {
                "type": "string",
                "description": "The path to send the request to"
              },
              "method": {
                "type": "string",
                "description": "The HTTP method to use (GET, POST, PUT, DELETE, etc.)"
              },
              "body": {
                "type": "string",
                "description": "The request body content (for POST/PUT requests)"
              },
              "content": {
                "type": "string",
                "description": "Alternative name for the request body content (for compatibility)"
              },
              "headers": {
                "type": "object",
                "description": "Optional headers to include with the request"
              },
              "rawResponse": {
                "type": "boolean",
                "description": "If true, return the full response object instead of just the content"
              }
            },
            "required": ["path"]
          }
        });
      }

      // Prefix all tool names with server name for disambiguation
      logger.debug(`Prefixing tool names with server name: ${config.SERVER_NAME}`);
      for (let i = 0; i < toolsData.tools.length; i++) {
        const tool = toolsData.tools[i];
        const originalName = tool.name;
        const prefixedName = createPrefixedToolName(config.SERVER_NAME, originalName);
        
        if (prefixedName !== originalName) {
          logger.debug(`Renaming tool: ${originalName} -> ${prefixedName}`);
          tool.name = prefixedName;
        }
      }

      sendStandardResponse(request, toolsData, logger);
    } catch (parseError) {
      logger.error(`Error parsing tools list: ${parseError.message}`);
      sendErrorResponse(request, -32603, `Error parsing tools list: ${parseError.message}`, logger);
    }
  }, 'TOOLS-LIST', logger);
}

module.exports = {
  handleToolsList
};



/*
const { makeHttpRequest } = require('../lib/server');
const { sendStandardResponse, sendErrorResponse } = require('../lib/utils');

*/

/**
 * Handle tool list requests
 * @param {Object} request - MCP request
 * @param {Object} config - Configuration object
 * @param {Object} logger - Logger instance
 */

/*
function handleToolsList(request, config, logger) {
  logger.info('Handling tools/list request');
  
  const options = {
    hostname: config.BACKEND_HOST,
    port: config.HTTP_HOST_PORT,
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
      
      // Validate the response structure
      if (!toolsData.tools || !Array.isArray(toolsData.tools)) {
        throw new Error('Invalid tools list response format');
      }
      
      sendStandardResponse(request, toolsData, logger);
    } catch (parseError) {
      logger.error(`Error parsing tools list: ${parseError.message}`);
      sendErrorResponse(request, -32603, `Error parsing tools list: ${parseError.message}`, logger);
    }
  }, 'TOOLS-LIST', logger);
}

module.exports = {
  handleToolsList
};

*/
