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
