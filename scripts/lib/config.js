/**
 * config.js
 * 
 * Configuration for Lisply MCP wrapper.
 * Stripped of docker/container orchestration - just connection config.
 */

const fs = require('fs');
const path = require('path');
const { execSync } = require('child_process');

/**
 * Get environment variable with fallback
 */
function getEnvVar(name, defaultValue, prefixes = ['', 'LISPLY_']) {
  for (const prefix of prefixes) {
    const fullName = `${prefix}${name}`;
    if (process.env[fullName] !== undefined) {
      return process.env[fullName];
    }
  }
  return defaultValue;
}

/**
 * Parse boolean from string
 */
function parseBool(value, defaultValue = false) {
  if (value === undefined || value === null) return defaultValue;
  if (typeof value === 'boolean') return value;
  if (typeof value === 'string') {
    const lowered = value.toLowerCase().trim();
    if (lowered === 'true' || lowered === 'yes' || lowered === '1') return true;
    if (lowered === 'false' || lowered === 'no' || lowered === '0') return false;
  }
  return defaultValue;
}

/**
 * Parse integer with bounds
 */
function parseInt10(value, defaultValue, min = null, max = null) {
  if (value === undefined || value === null) return defaultValue;
  const intValue = parseInt(value, 10);
  if (isNaN(intValue)) return defaultValue;
  if (min !== null && intValue < min) return min;
  if (max !== null && intValue > max) return max;
  return intValue;
}

/**
 * Get git branch (for version info)
 */
function getGitBranch() {
  try {
    const projectRoot = path.join(__dirname, '..', '..');
    return execSync(`cd "${projectRoot}" && git rev-parse --abbrev-ref HEAD`, 
                    { encoding: 'utf8', stdio: ['pipe', 'pipe', 'pipe'] }).trim();
  } catch (e) {
    return 'unknown';
  }
}

/**
 * Create prefixed tool name
 */
function createPrefixedToolName(serverName, toolName) {
  if (!serverName || !toolName) return toolName;
  const coreServerName = serverName
    .replace(/^lisply-mcp-?/, '')
    .replace(/^mcp-?/, '')
    .replace(/-+$/, '');
  const prefix = coreServerName || serverName;
  return `${prefix}__${toolName}`;
}

/**
 * Extract original tool name from prefixed name
 */
function extractOriginalToolName(prefixedToolName) {
  if (!prefixedToolName) return prefixedToolName;
  const parts = prefixedToolName.split('__');
  if (parts.length >= 2) {
    return parts.slice(1).join('__');
  }
  return prefixedToolName;
}

/**
 * Load configuration from CLI args and environment
 */
function loadConfig(program) {
  const options = program.opts();
  
  const MIN_PORT = 1024;
  const MAX_PORT = 65535;
  
  // Backend connection
  const BACKEND_HOST = options.backendHost || getEnvVar('BACKEND_HOST', '127.0.0.1');
  const HTTP_PORT = parseInt10(options.httpPort || getEnvVar('HTTP_PORT', '9080'), 9080, MIN_PORT, MAX_PORT);
  const HTTP_HOST_PORT = parseInt10(options.httpHostPort || getEnvVar('HTTP_HOST_PORT', '9081'), 9081, MIN_PORT, MAX_PORT);
  
  // SWANK ports (informational, for docs/diagnostics)
  const SWANK_PORT = parseInt10(options.swankPort || getEnvVar('SWANK_PORT', '4200'), 4200, MIN_PORT, MAX_PORT);
  const SWANK_HOST_PORT = parseInt10(options.swankHostPort || getEnvVar('SWANK_HOST_PORT', '4201'), 4201, MIN_PORT, MAX_PORT);
  
  // Endpoints
  const ENDPOINT_PREFIX = options.endpointPrefix || getEnvVar('ENDPOINT_PREFIX', 'lisply');
  const LISP_EVAL_ENDPOINT_NAME = options.lispEvalEndpoint || getEnvVar('LISP_EVAL_ENDPOINT', 'lisp-eval');
  const HTTP_REQUEST_ENDPOINT_NAME = options.httpRequestEndpoint || getEnvVar('HTTP_REQUEST_ENDPOINT', 'http-request');
  const PING_ENDPOINT_NAME = options.pingEndpoint || getEnvVar('PING_ENDPOINT', 'ping-lisp');
  
  const BASE_PATH = `/${ENDPOINT_PREFIX}`;
  const EVAL_ENDPOINT = `${BASE_PATH}/${LISP_EVAL_ENDPOINT_NAME}`;
  const HTTP_REQUEST_ENDPOINT = `${BASE_PATH}/${HTTP_REQUEST_ENDPOINT_NAME}`;
  const PING_ENDPOINT = `${BASE_PATH}/${PING_ENDPOINT_NAME}`;
  
  // Server identification
  const SERVER_NAME = options.serverName || getEnvVar('SERVER_NAME', 'lisply-mcp');
  
  // Timeouts
  const EVAL_TIMEOUT = parseInt10(options.evalTimeout || getEnvVar('EVAL_TIMEOUT', '30000'), 30000);
  const REQUEST_TIMEOUT_MS = parseInt10(
    options.requestTimeoutMs || getEnvVar('REQUEST_TIMEOUT_MS', '10000'),
    10000
  );
  
  // Logging
  const LOG_FILE = options.logFile || getEnvVar('LOG_FILE', '/tmp/lisply-mcp-wrapper.log');
  const DEBUG_MODE = parseBool(options.debug, false) || parseBool(getEnvVar('DEBUG_MODE', 'false'), false);
  
  // Version
  const VERSION = `2.0.0-${getGitBranch()}`;
  
  return {
    BACKEND_HOST,
    HTTP_PORT,
    HTTP_HOST_PORT,
    SWANK_PORT,
    SWANK_HOST_PORT,
    ENDPOINT_PREFIX,
    BASE_PATH,
    EVAL_ENDPOINT,
    HTTP_REQUEST_ENDPOINT,
    PING_ENDPOINT,
    SERVER_NAME,
    EVAL_TIMEOUT,
    REQUEST_TIMEOUT_MS,
    LOG_FILE,
    DEBUG_MODE,
    VERSION,
    options
  };
}

module.exports = {
  getEnvVar,
  parseBool,
  parseInt10,
  getGitBranch,
  createPrefixedToolName,
  extractOriginalToolName,
  loadConfig
};
