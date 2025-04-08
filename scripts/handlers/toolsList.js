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