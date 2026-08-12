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
* lispEval.js (HTTP only, no more docker exec access)
 * 
 * Handle lisp_eval tool via HTTP POST to backend.
 * No stdio/docker container management - pure HTTP client.
 */

const { getBackendConnectionInfo, makeHttpRequest } = require('../lib/server');
const { sendTextResponse, sendToolErrorResponse } = require('./index');

/**
 * Handle lisp_eval tool call
 */
function handleLispEval(request, args, config, logger) {
  logger.info(`Evaluating Lisp code: ${args.code.substring(0, 100)}${args.code.length > 100 ? '...' : ''}`);
  
  if (args.package) {
    logger.info(`Using package: ${args.package}`);
  }
  
  return handleLispEvalViaHttp(request, args, config, logger);
}

/**
 * Handle lisp_eval via HTTP POST
 */
function handleLispEvalViaHttp(request, args, config, logger) {
  const payload = JSON.stringify({
    code: args.code,
    ...(args.package && { package: args.package })
  });
  
  const { hostname, port } = getBackendConnectionInfo(config, logger);
  
  const options = {
    hostname,
    port,
    path: config.EVAL_ENDPOINT,
    method: 'POST',
    timeoutMs: config.EVAL_TIMEOUT,
    headers: {
      'Content-Type': 'application/json',
      'Content-Length': Buffer.byteLength(payload)
    }
  };
  
  makeHttpRequest(options, payload, (error, response) => {
    if (error) {
      logger.error(`Lisp eval error: ${error.message}`);
      sendToolErrorResponse(request, `Error evaluating Lisp code: ${error.message}`, logger);
      return;
    }
    
    try {
      const result = JSON.parse(response.content);
      
      if ('success' in result) {
        if (result.success) {
          logger.info(`Lisp eval success: ${result.result}`);
          sendTextResponse(request, `Result: ${result.result}, Stdout: ${result.stdout}`, logger);
        } else {
          logger.error(`Lisp eval failed: ${result.error || 'Unknown error'}`);
          sendToolErrorResponse(request, `Error: ${result.error || 'Unknown error'}${result.stdout ? `, Stdout: ${result.stdout}` : ''}`, logger);
        }
      } else {
        // Non-standard format, return as-is
        logger.info('Non-standard JSON response');
        sendTextResponse(request, JSON.stringify(result, null, 2), logger);
      }
    } catch (e) {
      // Not JSON, return as text
      logger.debug(`Response is not JSON: ${e.message}`);
      sendTextResponse(request, response.content, logger);
    }
  }, 'LISP-EVAL', logger);
}

module.exports = {
  handleLispEval,
  handleLispEvalViaHttp
};
