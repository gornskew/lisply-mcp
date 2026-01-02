/**
* lispEval.js (HTTP only, no more docker exec access)
 * 
 * Handle lisp_eval tool via HTTP POST to backend.
 * No stdio/docker container management - pure HTTP client.
 */

const { getBackendConnectionInfo, makeHttpRequest } = require('../lib/server');
const { sendTextResponse, sendErrorResponse } = require('./index');

/**
 * Handle lisp_eval tool call
 */
function handleLispEval(request, args, config, logger) {
  logger.info(`Evaluating Lisp code: ${args.code.substring(0, 100)}${args.code.length > 100 ? '...' : ''}`);
  
  if (args.package) {
    logger.info(`Using package: ${args.package}`);
  }
  
  // Mode parameter is accepted but ignored now (always HTTP)
  if (args.mode && args.mode !== 'http') {
    logger.debug(`Mode '${args.mode}' requested but we now only support HTTP mode`);
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
    timeoutMs: config.REQUEST_TIMEOUT_MS,
    headers: {
      'Content-Type': 'application/json',
      'Content-Length': Buffer.byteLength(payload)
    }
  };
  
  makeHttpRequest(options, payload, (error, response) => {
    if (error) {
      logger.error(`Lisp eval error: ${error.message}`);
      sendErrorResponse(request, -32603, `Error evaluating Lisp code: ${error.message}`, logger);
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
          sendErrorResponse(request, -32603, `Error: ${result.error || 'Unknown error'}`, logger);
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
