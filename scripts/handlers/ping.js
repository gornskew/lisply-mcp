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
 * ping.js 
 * 
 * Handler for ping_lisp tool
 */

const { getBackendConnectionInfo, makeHttpRequest } = require('../lib/server');
const { sendTextResponse, sendToolErrorResponse } = require('./index');

/**
 * Handle ping_lisp tool
 */
function handlePingLisp(request, config, logger) {
  logger.info('Handling ping_lisp request');
  
  const { hostname, port } = getBackendConnectionInfo(config, logger);
  
  const options = {
    hostname,
    port,
    path: config.PING_ENDPOINT,
    method: 'GET'
  };
  
  makeHttpRequest(options, null, (error, response) => {
    if (error) {
      logger.error(`Ping error: ${error.message}`);
      sendToolErrorResponse(request, `Error pinging backend: ${error.message}`, logger);
      return;
    }
    
    sendTextResponse(request, response.content, logger);
  }, 'PING', logger);
}

module.exports = { handlePingLisp };
