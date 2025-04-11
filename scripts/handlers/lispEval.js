/**
 * lispEval.js
 * 
 * Handler for lisp_eval tool
 */

const { makeHttpRequest } = require('../lib/server');
const { tryAttachToContainer } = require('../lib/docker');
const { sendTextResponse, sendErrorResponse } = require('../lib/utils');
const { exec } = require('child_process');

/**
 * Handle lisp_eval tool with either HTTP or stdio based on mode parameter
 * @param {Object} request - MCP request
 * @param {Object} args - Tool arguments
 * @param {Object} config - Configuration object
 * @param {Object} logger - Logger instance
 */
function handleLispEval(request, args, config, logger) {
  logger.info(`Handling lisp_eval with code: ${args.code?.substring(0, 100)}${args.code?.length > 100 ? '...' : ''}`);
  
  try {
    // Check for required parameter
    if (!args.code) {
      sendErrorResponse(request, -32602, "Missing required parameter: code", logger);
      return;
    }
    
    // Check the mode parameter (default to 'http' if not specified)
    const requestedMode = (args.mode || 'http').toLowerCase();
    logger.info(`Requested mode: ${requestedMode}`);
    
    // Check if we should use stdio for local containers
    const isLocalHost = (config.BACKEND_HOST === '127.0.0.1' || config.BACKEND_HOST === 'localhost');
    
    // First check if we already have a direct stdio connection
    let canUseStdio = isLocalHost && global.dockerProcess && ! config.NO_USE_STDIO;
    
    // If we don't have a direct connection but stdio is enabled, try to find and attach to the container
    if (isLocalHost && !global.dockerProcess && !config.NO_USE_STDIO && requestedMode === 'stdio') {
      canUseStdio = tryAttachToContainer(config, logger);
    }
    
    // Check if this is a debugger command
    const isDebuggerCommand = args.debugger_mode === true;
    
    // If mode is explicitly set to stdio and we can't use stdio, we should warn about it
    if (requestedMode === 'stdio' && !canUseStdio) {
      logger.warn(`Stdio mode requested but not available. Will use HTTP mode instead.`);
    }
    
    // Determine which mode to use based on requested mode and availability
    const useStdio = (requestedMode === 'stdio' && canUseStdio);
    
    if (useStdio) {
      // Use stdin/stdout communication with the Docker container
      if (isDebuggerCommand) {
        logger.info(`Using stdio for Lisp debugger command with local container`);
      } else {
        logger.info(`Using stdio for Lisp evaluation with local container`);
      }
      handleLispEvalViaStdio(request, args, config, logger);
    } else {
      // Debugger mode requires stdio
      if (isDebuggerCommand) {
        logger.error(`Debugger mode requires local container with stdio enabled`);
        sendErrorResponse(request, -32603, `Debugger mode requires local container with stdio enabled`, logger);
        return;
      }
      
      // Use HTTP communication
      logger.info(`Using HTTP for Lisp evaluation with ${config.BACKEND_HOST}`);
      handleLispEvalViaHttp(request, args, config, logger);
    }
  } catch (error) {
    logger.error(`Error in lisp_eval: ${error.message}`);
    if (error.stack) {
      logger.error(`Error stack: ${error.stack}`);
    }
    sendErrorResponse(request, -32603, `Error evaluating Lisp code: ${error.message}`, logger);
  }
}

/**
 * Handle lisp_eval tool using stdio with local Docker container
 * @param {Object} request - MCP request
 * @param {Object} args - Tool arguments
 * @param {Object} config - Configuration object
 * @param {Object} logger - Logger instance
 */
function handleLispEvalViaStdio(request, args, config, logger) {
  // Safety check for dockerProcess
  if (!global.dockerProcess || !global.dockerProcess.stdin || !global.dockerProcess.stdout) {
    logger.error('Docker process or its stdio streams are not available');
    return handleLispEvalViaHttp(request, args, config, logger); // Fallback to HTTP
  }
  
  const lisp_expr = args.code;
  const packageName = args.package || 'COMMON-LISP-USER';
  
  logger.info(`Sending Lisp expression to container REPL via stdio`);
  
  // Add package prefix if requested
  const fullExpr = packageName !== 'COMMON-LISP-USER' 
    ? `(in-package :${packageName}) ${lisp_expr}`
    : lisp_expr;
    
  // Set up variables to capture the response
  let responseData = '';
  let isEvaluationComplete = false;
  let responseTimeout = null;
  
  // Prepare event handlers for stdout data
  const stdoutDataHandler = (data) => {
    const output = data.toString();
    logger.debug(`REPL output: ${output}`);
    
    // Collect all output
    responseData += output;
    
    // Log the current state
    logger.debug(`Current responseData: ${responseData}`);
    
    // Look for prompt in both the entire response and just this output chunk
    const responseTrimmed = responseData.trim();
    const outputTrimmed = output.trim();
    const debuggerPrompt = config.DEBUGGER_PROMPTS[config.LISP_IMPL];
    
    // Log what we're looking for
    logger.debug(`Looking for REPL prompt: '${config.REPL_PROMPT}' or debugger prompt: '${debuggerPrompt}'`);
    
    // Check for regular REPL prompt in both places
    const outputEndsWithPrompt = outputTrimmed.endsWith(config.REPL_PROMPT);
    const responseEndsWithPrompt = responseTrimmed.endsWith(config.REPL_PROMPT);
    const isReplPrompt = outputEndsWithPrompt || responseEndsWithPrompt;
    
    // Check for debugger prompt
    const isDebuggerPrompt = debuggerPrompt && (
      outputTrimmed.endsWith(debuggerPrompt) || 
      responseTrimmed.endsWith(debuggerPrompt) ||
      outputTrimmed.includes(debuggerPrompt)
    );
    
    // Log detection status
    logger.debug(`Prompt detection - outputEndsWithPrompt: ${outputEndsWithPrompt}, responseEndsWithPrompt: ${responseEndsWithPrompt}, isDebuggerPrompt: ${isDebuggerPrompt}`);
    
    // Only complete when we see a prompt
    if (isReplPrompt || isDebuggerPrompt) {
      if (isReplPrompt) {
        logger.info('Detected REPL prompt - evaluation complete');
      } else {
        logger.info('Detected debugger prompt - evaluation entered debugger');
      }
      
      // Prevent multiple completions
      if (isEvaluationComplete) {
        logger.debug('Evaluation already marked as complete, skipping processing');
        return;
      }
      
      isEvaluationComplete = true;
      clearTimeout(responseTimeout);
      
      // Remove the stdout data handler to prevent capturing unrelated output
      logger.info('Removing stdout data handler');
      global.dockerProcess.stdout.removeListener('data', stdoutDataHandler);
      
      // Clean up response
      let cleanedResponse = responseData;
      
      // For regular REPL prompt, just remove it
      if (isReplPrompt) {
        try {
          // Try to extract the result - everything before the prompt
          const promptIndex = responseData.lastIndexOf(config.REPL_PROMPT);
          if (promptIndex > 0) {
            // Find the last newline before the prompt
            const lastNewline = responseData.lastIndexOf('\n', promptIndex);
            if (lastNewline > 0) {
              // Get the text between the last newline and the prompt
              cleanedResponse = responseData.substring(0, lastNewline).trim();
            } else {
              // Just remove the prompt if no newline
              cleanedResponse = responseData.replace(new RegExp(config.REPL_PROMPT + '$'), '').trim();
            }
          } else {
            // Fallback to simple replacement
            cleanedResponse = responseData.replace(new RegExp(config.REPL_PROMPT + '$'), '').trim();
          }
        } catch (e) {
          logger.error(`Error cleaning response: ${e.message}`);
          cleanedResponse = responseData.trim();
        }
      } 
      // For debugger prompt, keep it in the response
      else if (isDebuggerPrompt) {
        // Just trim whitespace
        cleanedResponse = responseData.trim();
      }
      
      logger.info(`Lisp evaluation complete, response length: ${cleanedResponse.length}`);
      logger.info(`Response content: "${cleanedResponse}"`);
      logger.info(`Debugger detected: ${isDebuggerPrompt}`);
      
      // Add debugger metadata to help the LLM
      if (isDebuggerPrompt) {
        // Create a response with metadata about the debugger state
        const responseWithMetadata = {
          output: cleanedResponse,
          debugger: {
            active: true,
            implementation: config.LISP_IMPL,
            prompt: debuggerPrompt
          }
        };
        
        try {
          logger.info('Sending debugger response to LLM');
          sendTextResponse(request, responseWithMetadata, logger);
          logger.info('Debugger response sent successfully');
        } catch (error) {
          logger.error(`Error sending debugger response: ${error.message}`);
        }
      } else {
        // Regular response without debugger
        try {
          logger.info('Sending regular response to LLM');
          sendTextResponse(request, cleanedResponse, logger);
          logger.info('Regular response sent successfully');
        } catch (error) {
          logger.error(`Error sending regular response: ${error.message}`);
        }
      }
    }
  };
  
  // Set up timeout for evaluation
  responseTimeout = setTimeout(() => {
    if (!isEvaluationComplete) {
      logger.warn(`Lisp evaluation timed out after ${config.EVAL_TIMEOUT}ms`);
      global.dockerProcess.stdout.removeListener('data', stdoutDataHandler);
      
      // Send what we have so far plus timeout message
      const timeoutResponse = responseData + "\n;; ERROR: Evaluation timed out";
      sendTextResponse(request, timeoutResponse, logger);
    }
  }, config.EVAL_TIMEOUT);
  
  // Set up error handler
  const errorHandler = (error) => {
    logger.error(`Error during Lisp evaluation via stdio: ${error.message}`);
    clearTimeout(responseTimeout);
    global.dockerProcess.stdout.removeListener('data', stdoutDataHandler);
    sendErrorResponse(request, -32603, `Error during Lisp evaluation: ${error.message}`, logger);
  };
  
  // Attach the stdout data handler
  global.dockerProcess.stdout.on('data', stdoutDataHandler);
  
  // Handle potential errors
  global.dockerProcess.stdin.once('error', errorHandler);
  global.dockerProcess.stdout.once('error', errorHandler);
  
  // Send the Lisp code to the container's stdin
  try {
    // Check if dockerProcess is still valid and writable
    if (!global.dockerProcess || !global.dockerProcess.stdin || !global.dockerProcess.stdin.writable) {
      logger.error('Docker process is no longer available or stdin is not writable');
      
      // Try to reattach to the container if it exists but our connection was lost
      if (global.backendContainerName && !global.dockerProcess) {
        logger.info(`Attempting to reattach to container ${global.backendContainerName}`);
        // Check if container is still running
        exec(`docker ps --filter "name=${global.backendContainerName}" --format "{{.ID}}"`, (error, stdout, stderr) => {
          if (!error && stdout.trim()) {
            logger.info(`Container ${global.backendContainerName} is still running, reattaching`);
            tryAttachToContainer(config, logger);
          } else {
            logger.error(`Container ${global.backendContainerName} is no longer running`);
            return handleLispEvalViaHttp(request, args, config, logger); // Fall back to HTTP
          }
        });
      } else {
        return handleLispEvalViaHttp(request, args, config, logger); // Fall back to HTTP
      }
    }
    
    // Send a newline first to ensure we're at a fresh prompt
    logger.debug('Sending initial newline to REPL');
    global.dockerProcess.stdin.write('\n');
    
    // Then send the actual Lisp code with a newline
    logger.debug(`Sending Lisp expression: ${fullExpr}`);
    global.dockerProcess.stdin.write(fullExpr + '\n');
    
    // Keep stdin open
    logger.debug('Keeping stdin open after sending Lisp code');
    
    logger.debug(`Sent Lisp code to container: ${fullExpr}`);
  } catch (error) {
    logger.error(`Error sending code to container: ${error.message}`);
    errorHandler(error);
  }
}

/**
 * Handle lisp_eval tool with HTTP request (original implementation)
 * @param {Object} request - MCP request
 * @param {Object} args - Tool arguments
 * @param {Object} config - Configuration object
 * @param {Object} logger - Logger instance
 */
function handleLispEvalViaHttp(request, args, config, logger) {
  // Create JSON payload for the request
  const payload = JSON.stringify({ 
    code: args.code,
    ...(args.package && { package: args.package })
  });
  
  // HTTP POST to lisp-eval endpoint with proper content type
  const options = {
    hostname: config.BACKEND_HOST,
    port: config.HTTP_HOST_PORT,
    path: config.EVAL_ENDPOINT,
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'Content-Length': Buffer.byteLength(payload)
    }
  };
  
  // Use the common HTTP request helper
  makeHttpRequest(options, payload, (error, response) => {
    if (error) {
      logger.error(`Error evaluating Lisp code: ${error.message}`);
      sendErrorResponse(request, -32603, `Error evaluating Lisp code: ${error.message}`, logger);
      return;
    }
    
    // Process the response
    try {
      // Try to parse as JSON
      const result = JSON.parse(response.content);
      
      // Handle success/error based on the JSON structure
      if ('success' in result) {
        if (result.success) {
          logger.info(`Lisp eval success result: ${result.result}`);
            sendTextResponse(request, `Result: ${result.result}, Stdout: ${result.stdout}`, logger);
        } else {
          logger.error(`Lisp eval error result: ${result.error || "Unknown error"}`);
          sendErrorResponse(request, -32603, `Error evaluating Lisp code: ${result.error || "Unknown error"}`, logger);
        }
      } else {
        // Not a standard success/error format, return as-is
        logger.info(`Non-standard JSON format, returning as-is`);
        sendTextResponse(request, JSON.stringify(result, null, 2), logger);
      }
    } catch (e) {
      // Not JSON, treat as text
      logger.info(`Response is not JSON: ${e.message}`);
      sendTextResponse(request, response.content, logger);
    }
  }, 'LISP-EVAL', logger);
}

module.exports = {
  handleLispEval,
  handleLispEvalViaStdio,
  handleLispEvalViaHttp
};
