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
 * lisplySearch.js
 *
 * Handle the lisply_search tool via HTTP POST to the backend's
 * /lisply-search endpoint.  The tool was called skewed_search until
 * 2026-09-09; the alias was dropped on 2026-09-10 along with the
 * backend's /skewed-search endpoint.
 */

const { getBackendConnectionInfo, makeHttpRequest } = require('../lib/server');
const { sendStandardResponse, sendErrorResponse } = require('./index');

const ENDPOINT = '/lisply-search';
const TOOL_NAME = 'lisply_search';

function handleLisplySearch(request, args, config, logger) {
  const query = args.query;
  const endpoint = ENDPOINT;
  const toolName = TOOL_NAME;

  if (!query || typeof query !== 'string' || !query.trim()) {
    sendErrorResponse(request, -32602, 'Missing required parameter: query', logger);
    return;
  }

  const payload = JSON.stringify(args);
  const { hostname, port } = getBackendConnectionInfo(config, logger);

  const options = {
    hostname,
    port,
    path: `${config.BASE_PATH}${endpoint}`,
    method: 'POST',
    timeoutMs: config.REQUEST_TIMEOUT_MS,
    headers: {
      'Content-Type': 'application/json',
      'Content-Length': Buffer.byteLength(payload)
    }
  };

  makeHttpRequest(options, payload, (error, response) => {
    if (error) {
      logger.error(`${toolName} error: ${error.message}`);
      sendErrorResponse(request, -32603, `Error calling ${toolName}: ${error.message}`, logger);
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
      logger.error(`${toolName} response parse error: ${e.message}`);
      sendErrorResponse(request, -32603, `Error parsing ${toolName} response: ${e.message}`, logger);
    }
  }, 'LISPLY-SEARCH', logger);
}

module.exports = { handleLisplySearch };
