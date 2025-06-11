/**
 * config.js
 * 
 * Configuration functions for the MCP wrapper
 */

const fs = require('fs');
const path = require('path');
const { exec, execSync } = require('child_process');

/**
 * Helper function to collect multiple mount options
 * @param {string} value - The value to collect
 * @param {Array} previous - The previous values
 * @returns {Array} The concatenated array
 */
function collect(value, previous) {
  return previous.concat([value]);
}

/**
 * Helper function to get environment variables with multiple possible prefixes
 * @param {string} name - Base name of the environment variable
 * @param {*} defaultValue - Default value if no environment variable is found
 * @param {Array<string>} prefixes - Prefixes to try (in order of preference)
 * @param {function} transform - Optional function to transform the value
 * @returns {*} The environment variable value or default
 */
function getEnvVar(name, defaultValue, prefixes = ['', 'LISPLY_'], transform = null) {
  // Try each prefix in order
  for (const prefix of prefixes) {
    const fullName = `${prefix}${name}`;
    if (process.env[fullName] !== undefined) {
      const value = process.env[fullName];
      return transform ? transform(value) : value;
    }
  }
  
  // Fall back to default if no environment variable is found
  return defaultValue;
}

/**
 * Helper function to parse boolean environment variables
 * @param {string} value - The environment variable value
 * @param {boolean} defaultValue - Default value if parsing fails
 * @returns {boolean} The parsed boolean value
 */
function parseBool(value, defaultValue = false) {
  if (value === undefined || value === null) return defaultValue;
  if (typeof value === 'boolean') return value;
  if (typeof value === 'number') return value !== 0;
  if (typeof value === 'string') {
    const lowered = value.toLowerCase().trim();
    if (lowered === 'true' || lowered === 'yes' || lowered === '1') return true;
    if (lowered === 'false' || lowered === 'no' || lowered === '0') return false;
  }
  return defaultValue;
}

/**
 * Helper function to parse integer environment variables
 * @param {string} value - The environment variable value
 * @param {number} defaultValue - Default value if parsing fails
 * @param {number} min - Optional minimum value
 * @param {number} max - Optional maximum value
 * @returns {number} The parsed integer value
 */
function parseInt10(value, defaultValue, min = null, max = null) {
  if (value === undefined || value === null) return defaultValue;
  if (typeof value === 'number') {
    const intValue = Math.floor(value);
    if (min !== null && intValue < min) return min;
    if (max !== null && intValue > max) return max;
    return intValue;
  }
  try {
    const intValue = parseInt(value, 10);
    if (isNaN(intValue)) return defaultValue;
    if (min !== null && intValue < min) return min;
    if (max !== null && intValue > max) return max;
    return intValue;
  } catch (e) {
    return defaultValue;
  }
}

/**
 * Helper to safely execute git commands with improved error handling
 * @param {string} cmd - The command to execute
 * @param {boolean} logError - Whether to log errors (default: true)
 * @returns {string|null} - The command output or null if error
 */
function safeExec(cmd, logError = true) {
  try {
    return execSync(cmd, { encoding: 'utf8', stdio: ['pipe', 'pipe', 'pipe'] }).trim();
  } catch (error) {
    if (logError) {
      // Log at debug level to avoid cluttering logs with expected errors
      process.stderr.write(`[DEBUG] Error executing command: ${cmd}. Error: ${error.message}\n`);
      
      // Add more details at trace level if available
      if (error.stderr) {
        process.stderr.write(`[DEBUG] Command stderr: ${error.stderr}\n`);
      }
    }
    return null;
  }
}

/**
 * Get git branch information with multiple detection methods and fallbacks
 * @param {Object} options - Command line options
 * @returns {string} - The detected branch name or default branch
 */
function getGitBranch(options) {
  // Provide a default for the default branch
  const DEFAULT_BRANCH = 'master';
  
  // Try git command first (most reliable)
  // Run the git command from the project root
  const projectRoot = path.join(__dirname, '..', '..');
  const branch = safeExec(`cd "${projectRoot}" && git rev-parse --abbrev-ref HEAD`);
  if (branch && branch !== 'HEAD') {
    process.stderr.write(`[INFO] Git branch detected via command: ${branch}\n`);
    return branch;
  }
  
  // Fallback to reading .git/HEAD file directly
  try {
    const gitHeadContent = fs.readFileSync(path.join(__dirname, '..', '..', '.git', 'HEAD'), 'utf8').trim();
    if (gitHeadContent.startsWith('ref: refs/heads/')) {
      const branchName = gitHeadContent.substring('ref: refs/heads/'.length);
      process.stderr.write(`[INFO] Git branch detected via HEAD file: ${branchName}\n`);
      return branchName;
    }
  } catch (error) {
    process.stderr.write(`[DEBUG] Could not read .git/HEAD file: ${error.message}\n`);
  }
  
  // Final fallback to default branch
  process.stderr.write('[WARN] Could not determine git branch, using default branch\n');
  return DEFAULT_BRANCH;
}

/**
 * Get comprehensive version information with fallbacks for non-git environments
 * @returns {Object} Version information
 */
function getVersionInfo() {
  // Initialize with fallback values
  const info = {
    branch: 'unknown',
    commit: 'unknown',
    tag: null,
    dirty: false,
    date: new Date().toISOString(),
    version: '1.0.0' // Default version
  };
  
  // Check if we're in a git repo first
  const projectRoot = path.join(__dirname, '..', '..');
  const isGitRepo = safeExec(`cd "${projectRoot}" && git rev-parse --is-inside-work-tree`) === 'true';
  
  if (isGitRepo) {
    // Get branch name using our unified function
    info.branch = getGitBranch();
    
    // Get commit hash (short)
    const commit = safeExec(`cd "${projectRoot}" && git rev-parse --short HEAD`);
    if (commit) {
      info.commit = commit;
    }
    
    // Try to get the latest tag
    const tag = safeExec(`cd "${projectRoot}" && git describe --tags --abbrev=0 2>/dev/null`);
    if (tag) {
      info.tag = tag;
    }
    
    // Check if working directory is dirty
    const status = safeExec(`cd "${projectRoot}" && git status --porcelain`);
    info.dirty = status && status.length > 0;
    
    // Get commit date
    const date = safeExec(`cd "${projectRoot}" && git log -1 --format=%cI`);
    if (date) {
      info.date = date;
    }
    
    // Construct version string
    if (info.tag) {
      // If we have a tag, use tag-commithash format
      info.version = `${info.tag}-${info.commit}${info.dirty ? '-dirty' : ''}`;
    } else {
      // Otherwise use branch-commithash format
      info.version = `${info.branch}-${info.commit}${info.dirty ? '-dirty' : ''}`;
    }
  } else {
    // Not in a git repo, try to get some system info instead
    try {
      const hostname = safeExec('hostname') || 'unknown';
      const username = process.env.USER || process.env.USERNAME || 'unknown';
      info.version = `1.0.0-${username}@${hostname}`;
    } catch (error) {
      // Keep the default version
    }
  }
  
  return info;
}

/**
 * Validate ports for potential conflicts
 * @param {Object} config - Configuration object
 * @returns {boolean} - True if no conflicts found
 */
function checkPortConflicts(config) {
  const hostPorts = [
    { name: 'HTTP', port: config.HTTP_HOST_PORT },
    { name: 'HTTPS', port: config.HTTPS_HOST_PORT },
    { name: 'SWANK', port: config.SWANK_HOST_PORT },
    { name: 'TELNET', port: config.TELNET_HOST_PORT }
  ];
  
  // Check for duplicate ports
  const conflicts = [];
  
  for (let i = 0; i < hostPorts.length; i++) {
    for (let j = i + 1; j < hostPorts.length; j++) {
      if (hostPorts[i].port === hostPorts[j].port) {
        conflicts.push(`${hostPorts[i].name} and ${hostPorts[j].name} use the same port: ${hostPorts[i].port}`);
      }
    }
  }
  
  // Log any conflicts but don't abort
  if (conflicts.length > 0) {
    process.stderr.write(`[WARN] Detected port conflicts: ${conflicts.join(', ')}\n`);
  }
  
  return conflicts.length === 0;
}

/**
 * Handle mount points from environment variables with proper parsing
 * @returns {Array} Array of mount points
 */
function getEnvMounts() {
  const mountsStr = getEnvVar('MOUNTS', '');
  if (!mountsStr) return [];
  
  // Split by comma and filter out empty entries
  return mountsStr.split(',').filter(mount => mount && mount.trim().length > 0);
}

/**
 * Sanitize a string for use as a Docker image tag
 * @param {string} input - String to sanitize
 * @returns {string} - Sanitized string safe for use as a Docker tag
 */
function sanitizeDockerTag(input) {
  if (!input) return 'unknown';
  
  // Replace any characters that aren't letters, numbers, underscores, periods, or hyphens
  // Convert slashes to double hyphens (common convention for branch names)
  let sanitized = input
    .replace(/\//g, '--')
    .replace(/[^a-zA-Z0-9_.-]/g, '-');
  
  // Tags can't start with a period or hyphen
  sanitized = sanitized.replace(/^[.-]+/, '');
  
  // Limit to 128 characters max
  if (sanitized.length > 128) {
    sanitized = sanitized.substring(0, 128);
  }
  
  // Ensure we still have a valid tag after all replacements
  if (!sanitized || sanitized.length === 0) {
    return 'unknown';
  }
  
  return sanitized;
}

/**
 * Detect server name from environment or process context
 * @returns {string|null} - Detected server name or null if not found
 */
function detectServerNameFromEnvironment() {
  // Check if we're running under a process manager that might set useful env vars
  // MCP servers often get launched with specific environment context
  
  // Try to detect from common environment variables that might indicate the server purpose
  const envHints = [
    process.env.MCP_SERVER_NAME,
    process.env.LISPLY_SERVER_TYPE,
    process.env.SERVER_TYPE,
    process.env.BACKEND_TYPE
  ];
  
  for (const hint of envHints) {
    if (hint && hint.trim()) {
      return hint.trim();
    }
  }
  
  // Try to detect from command line arguments or working directory
  const cwd = process.cwd();
  
  // Look for hints in the current working directory path
  if (cwd.includes('gendl')) return 'gendl';
  if (cwd.includes('emacs')) return 'emacs';
  
  // Check if we're connecting to specific ports that might indicate the backend type
  const args = process.argv;
  for (let i = 0; i < args.length - 1; i++) {
    if (args[i] === '--http-host-port') {
      const port = args[i + 1];
      if (port === '7080') return 'emacs'; // Common port for Emacs lisply backend
      if (port === '9081') return 'gendl'; // Common port for Gendl backend
    }
    if (args[i] === '--backend-host' && i < args.length - 2) {
      // Check if next argument after host is a port
      const nextArg = args[i + 2];
      if (nextArg === '7080') return 'emacs';
      if (nextArg === '9081') return 'gendl';
    }
  }
  
  return null;
}

/**
 * Create a prefixed tool name using the server name
 * @param {string} serverName - Server name to use as prefix
 * @param {string} toolName - Original tool name
 * @returns {string} - Prefixed tool name (e.g., "gendl__lisp_eval")
 */
function createPrefixedToolName(serverName, toolName) {
  if (!serverName || !toolName) return toolName;
  
  // Extract the core name from server name if it contains "lisply-mcp-" or similar
  const coreServerName = serverName
    .replace(/^lisply-mcp-?/, '') // Remove "lisply-mcp" prefix and optional dash
    .replace(/^mcp-?/, '')        // Remove "mcp" prefix and optional dash
    .replace(/-+$/, '');          // Remove trailing dashes
  
  // If we have a meaningful core name, use it; otherwise use full server name
  const prefix = coreServerName || serverName;
  
  return `${prefix}__${toolName}`;
}

/**
 * Extract the original tool name from a prefixed tool name
 * @param {string} prefixedToolName - Prefixed tool name (e.g., "gendl__lisp_eval")
 * @returns {string} - Original tool name (e.g., "lisp_eval")
 */
function extractOriginalToolName(prefixedToolName) {
  if (!prefixedToolName) return prefixedToolName;
  
  const parts = prefixedToolName.split('__');
  if (parts.length >= 2) {
    // Return everything after the first "__"
    return parts.slice(1).join('__');
  }
  
  // If no prefix found, return as-is
  return prefixedToolName;
}

/**
 * Load and validate the configuration
 * @param {Object} program - Commander program instance
 * @returns {Object} - Configuration object
 */
function loadConfig(program) {
  const options = program.opts();
  
  // Define port ranges for validation
  const MIN_PORT = 1024;  // Avoid privileged ports
  const MAX_PORT = 65535; // Maximum valid port number
  
  // Configure the backend server details - Use CLI args, env vars, or defaults
  const BACKEND_HOST = options.backendHost || getEnvVar('BACKEND_HOST', '127.0.0.1');
  
  // Internal ports (inside the container)
  const HTTP_PORT = parseInt10(options.httpPort || getEnvVar('HTTP_PORT', '9080'), 9080, MIN_PORT, MAX_PORT);
  const HTTPS_PORT = parseInt10(options.httpsPort || getEnvVar('HTTPS_PORT', '9443'), 9443, MIN_PORT, MAX_PORT);
  const SWANK_PORT = parseInt10(options.swankPort || getEnvVar('SWANK_PORT', '4200'), 4200, MIN_PORT, MAX_PORT);
  const TELNET_PORT = parseInt10(options.telnetPort || getEnvVar('TELNET_PORT', '4023'), 4023, MIN_PORT, MAX_PORT);
  
  // Host ports (ports exposed on the host system) - set to internal port + 1
  const SWANK_HOST_PORT = parseInt10(options.swankHostPort || getEnvVar('SWANK_HOST_PORT', String(SWANK_PORT + 1)), SWANK_PORT + 1, MIN_PORT, MAX_PORT);
  const HTTP_HOST_PORT = parseInt10(options.httpHostPort || getEnvVar('HTTP_HOST_PORT', String(HTTP_PORT + 1)), HTTP_PORT + 1, MIN_PORT, MAX_PORT);
  const HTTPS_HOST_PORT = parseInt10(options.httpsHostPort || getEnvVar('HTTPS_HOST_PORT', String(HTTPS_PORT + 1)), HTTPS_PORT + 1, MIN_PORT, MAX_PORT);
  const TELNET_HOST_PORT = parseInt10(options.telnetHostPort || getEnvVar('TELNET_HOST_PORT', String(TELNET_PORT + 1)), TELNET_PORT + 1, MIN_PORT, MAX_PORT);
  
  // Constants for Docker image configuration
  const DEFAULT_IMPL = 'ccl';
  const DEFAULT_BRANCH = 'master';
  const DEFAULT_IMAGE_BASE = 'genworks/gendl';
  
  // Validate Lisp implementation
  const SUPPORTED_IMPLS = ['ccl', 'sbcl'];
  
  // Get Lisp implementation from arguments or environment variable
  const LISP_IMPL = (options.lispImpl || getEnvVar('LISP_IMPL', DEFAULT_IMPL)).toLowerCase();
  
  // Service startup flags - use consistent parsing with our helper functions
  const START_HTTP = options.startHttp !== false && parseBool(getEnvVar('START_HTTP', 'true'), true); // Default to true
  const START_HTTPS = parseBool(options.startHttps, false) || parseBool(getEnvVar('START_HTTPS', 'false'), false);
  const START_SWANK = options.startSwank !== false && parseBool(getEnvVar('START_SWANK', 'true'), true); // Default to true
  const START_TELNET = parseBool(options.startTelnet, false) || parseBool(getEnvVar('START_TELNET', 'false'), false);
  
  // Lisp REPL communication configuration
  // Enable stdio capability by default (but http is still the default communication mode)
    const NO_USE_STDIO = options.noUseStdio !== false || parseBool(getEnvVar('NO_USE_STDIO', 'true'), true);
  
  // Default REPL prompts by implementation
  const DEFAULT_PROMPTS = {
    'ccl': '?',
    'sbcl': '*',
    'ielm': 'ELISP>' 
  };
  
  // Debugger prompts by implementation (for future use)
  const DEBUGGER_PROMPTS = {
    'ccl': '>', // CCL debugger prompt
    'sbcl': '0]', // SBCL debugger level 0 prompt
    'ielm': 'ELISP>' // ielm doesn't have a separate debug level
  };
  
  // Get the appropriate REPL prompt for the selected Lisp implementation
  const REPL_PROMPT = options.replPrompt || process.env.REPL_PROMPT || DEFAULT_PROMPTS[LISP_IMPL] || '?';
  const EVAL_TIMEOUT = parseInt(options.evalTimeout || process.env.EVAL_TIMEOUT || '30000', 10);
  
  // Set up endpoint paths with configurable prefix and names
  const ENDPOINT_PREFIX = options.endpointPrefix || getEnvVar('ENDPOINT_PREFIX', 'lisply');
  const LISP_EVAL_ENDPOINT_NAME = options.lispEvalEndpoint || getEnvVar('LISP_EVAL_ENDPOINT', 'lisp-eval');
  const HTTP_REQUEST_ENDPOINT_NAME = options.httpRequestEndpoint || getEnvVar('HTTP_REQUEST_ENDPOINT', 'http-request');
  const PING_ENDPOINT_NAME = options.pingEndpoint || getEnvVar('PING_ENDPOINT', 'ping-lisp');
  
  // Server name for MCP identification and tool prefixing
  // Auto-detect if not explicitly provided, considering the actual port being used
  let detectedName = detectServerNameFromEnvironment();
  
  // If no detection from args, check the actual HTTP port that will be used
  if (!detectedName) {
    if (HTTP_HOST_PORT === 7080) {
      detectedName = 'emacs';
    } else if (HTTP_HOST_PORT === 9081) {
      detectedName = 'gendl';
    }
  }
  
  const SERVER_NAME = options.serverName || getEnvVar('SERVER_NAME', detectedName || 'lisply-mcp');
  
  // Construct full endpoint paths with prefix
  const BASE_PATH = `/${ENDPOINT_PREFIX}`;
  const EVAL_ENDPOINT = `${BASE_PATH}/${LISP_EVAL_ENDPOINT_NAME}`;
  const HTTP_REQUEST_ENDPOINT = `${BASE_PATH}/${HTTP_REQUEST_ENDPOINT_NAME}`;
  const PING_ENDPOINT = `${BASE_PATH}/${PING_ENDPOINT_NAME}`;
  
  // Set up logging to file for debugging
  const LOG_FILE = options.logFile || getEnvVar('LOG_FILE', '/tmp/lisply-mcp-wrapper.log');
  const DEBUG_MODE = parseBool(options.debug, false) || parseBool(getEnvVar('DEBUG_MODE', 'false'), false);
  
  // Get version info
  const versionInfo = getVersionInfo();
  const VERSION = versionInfo.version;
  
  // Docker configuration
  const DOCKER_SOCKET = options.dockerSocket || getEnvVar('DOCKER_SOCKET', '/var/run/docker.sock');
  const AUTO_START = options.autoStart !== false && parseBool(getEnvVar('AUTO_START', 'true'), true);
  
  // Handle mount points
  const MOUNTS = options.mount || []; // Mount points from command line
  const ENV_MOUNTS = getEnvMounts();
  const ALL_MOUNTS = [...MOUNTS, ...ENV_MOUNTS];
  
  return {
    BACKEND_HOST,
    SWANK_HOST_PORT,
    HTTP_HOST_PORT,
    HTTPS_HOST_PORT,
    TELNET_HOST_PORT,
    HTTP_PORT,
    HTTPS_PORT,
    SWANK_PORT,
    TELNET_PORT,
    LISP_IMPL,
    SUPPORTED_IMPLS,
    DEFAULT_IMPL,
    DEFAULT_BRANCH,
    DEFAULT_IMAGE_BASE,
    START_HTTP,
    START_HTTPS,
    START_SWANK,
    START_TELNET,
    NO_USE_STDIO,
    DEFAULT_PROMPTS,
    DEBUGGER_PROMPTS,
    REPL_PROMPT,
    EVAL_TIMEOUT,
    ENDPOINT_PREFIX,
    SERVER_NAME,
    BASE_PATH,
    EVAL_ENDPOINT,
    HTTP_REQUEST_ENDPOINT,
    PING_ENDPOINT,
    LOG_FILE,
    DEBUG_MODE,
    VERSION,
    versionInfo,
    DOCKER_SOCKET,
    AUTO_START,
    ALL_MOUNTS,
    options
  };
}

module.exports = {
  collect,
  getEnvVar,
  parseBool,
  parseInt10,
  safeExec,
  getGitBranch,
  getVersionInfo,
  checkPortConflicts,
  getEnvMounts,
  sanitizeDockerTag,
  detectServerNameFromEnvironment,
  createPrefixedToolName,
  extractOriginalToolName,
  loadConfig
};
