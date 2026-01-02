/**
 * gdlSearch.js
 *
 * Handle gdl_search tool via HTTP POST to backend.
 */

const { getBackendConnectionInfo, makeHttpRequest } = require('../lib/server');
const { sendStandardResponse, sendErrorResponse } = require('./index');

function handleGdlSearch(request, args, config, logger) {
  const query = args.query;

  if (!query || typeof query !== 'string' || !query.trim()) {
    sendErrorResponse(request, -32602, 'Missing required parameter: query', logger);
    return;
  }

  const payload = JSON.stringify(args);
  const { hostname, port } = getBackendConnectionInfo(config, logger);

  const options = {
    hostname,
    port,
    path: `${config.BASE_PATH}/gdl-search`,
    method: 'POST',
    timeoutMs: config.REQUEST_TIMEOUT_MS,
    headers: {
      'Content-Type': 'application/json',
      'Content-Length': Buffer.byteLength(payload)
    }
  };

  makeHttpRequest(options, payload, (error, response) => {
    if (error) {
      logger.error(`gdl_search error: ${error.message}`);
      sendErrorResponse(request, -32603, `Error calling gdl_search: ${error.message}`, logger);
      return;
    }

    try {
      const result = JSON.parse(response.content);
      sendStandardResponse(request, {
        content: [{
          type: 'text',
          text: JSON.stringify(result, null, 2)
        }]
      }, logger);
    } catch (e) {
      logger.error(`gdl_search response parse error: ${e.message}`);
      sendErrorResponse(request, -32603, `Error parsing gdl_search response: ${e.message}`, logger);
    }
  }, 'GDL-SEARCH', logger);
}

module.exports = { handleGdlSearch };
