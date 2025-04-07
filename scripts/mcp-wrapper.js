#!/usr/bin/env node

/**
 * mcp-wrapper.js
 * 
 * Copyright (C) 2025 Genworks
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 * 
 * This file is part of the Lisply Model Context Protocol integration.
 * 
 * Wrapper script for Lisply MCP integration with:
 * - Configurable backend host and port via environment variables or CLI arguments
 * - Docker container management for local deployments (using -i mode for automatic cleanup)
 * - Support for running inside a container or directly on the host
 * - Support for mounting volumes into the backend container
 */

const readline = require('readline');
const fs = require('fs');
const http = require('http');
const { exec, execSync, spawn } = require('child_process');
const path = require('path');
const { program } = require('commander');

// Helper function to collect multiple mount options
function collect(value, previous) {
  return previous.concat([value]);
}

// Parse command line arguments
program
    .option('-H, --host <host>', 'Backend server host (default: 127.0.0.1)')
    .option('--swank-host-port <port>', 'SWANK port on host system (default: 4201)')
    .option('--http-host-port <port>', 'HTTP port on host system (default: 9081)')
    .option('--https-host-port <port>', 'HTTPS port on host system (default: 9444)')
    .option('--telnet-host-port <port>', 'TELNET port on host system (default: 4024)')
    .option('--http-port <port>', 'HTTP port inside container (default: 9080)')
    .option('--https-port <port>', 'HTTPS port inside container (default: 9443)')
    .option('--swank-port <port>', 'SWANK port inside container (default: 4200)')
    .option('--telnet-port <port>', 'TELNET port inside container (default: 4023)')
    .option('--image-base-name <name>', 'Base name for Docker image (default: dcooper8/gendl)')
    .option('--image-branch <branch>', 'Branch to use for Docker image (default: auto-detected)')
    .option('--docker-image <image>', 'Full Docker image for backend (overrides base name and branch)')
    .option('--lisp-impl <impl>', 'Lisp implementation to use, ccl or sbcl (default: ccl)')
    .option('--no-auto-start', 'Do not auto-start backend docker container if not running')
    .option('--docker-socket <path>', 'Path to docker socket (default: /var/run/docker.sock)')
    .option('--log-file <path>', 'Path to log file (default: /tmp/mcp-wrapper.log)')
    .option('--debug', 'Enable debug logging')
    .option('--mount <mounts...>', 'Mount volumes in format "src:dst" (can specify multiple times)', collect, [])
    .option('--start-http', 'Start HTTP service in backend container (default: true)')
    .option('--start-https', 'Start HTTPS service in backend container (default: false)')
    .option('--start-swank', 'Start SWANK service in backend container (default: true)')
    .option('--start-telnet', 'Start TELNET service in backend container (default: false)')
    .option('--no-use-stdio', 'Disable stdio capability for local containers. When disabled, HTTP mode will always be used even when stdio mode is requested.')
    .option('--repl-prompt <pattern>', 'REPL prompt pattern to detect Lisp evaluation completion (default: ?)')
    .option('--eval-timeout <ms>', 'Timeout for Lisp evaluation in milliseconds (default: 30000)')
    .option('--eval-endpoint <path>', 'HTTP endpoint for Lisp evaluation (default: /mcp/lisp-eval)')
    .option('--ping-endpoint <path>', 'HTTP endpoint for ping (default: /mcp/ping-gendl)')
    .parse(process.argv);

const options = program.opts();

// Configure the backend server details - Use CLI args, env vars, or defaults
const BACKEND_HOST = options.host || process.env.LISPLY_HOST || process.env.LISPY_HOST || process.env.GENDL_HOST || '127.0.0.1';

// Host ports (ports exposed on the host system)
const SWANK_HOST_PORT = parseInt(options.swankHostPort || process.env.SWANK_HOST_PORT || '4201', 10);
const HTTP_HOST_PORT = parseInt(options.httpHostPort || process.env.HTTP_HOST_PORT || '9081', 10);
const HTTPS_HOST_PORT = parseInt(options.httpsHostPort || process.env.HTTPS_HOST_PORT || '10443', 10);
const TELNET_HOST_PORT = parseInt(options.telnetHostPort || process.env.TELNET_HOST_PORT || '5023', 10);

// Internal ports (inside the container)
const HTTP_PORT = parseInt(options.httpPort || process.env.HTTP_PORT || '9080', 10);
const HTTPS_PORT = parseInt(options.httpsPort || process.env.HTTPS_PORT || '9443', 10);
const SWANK_PORT = parseInt(options.swankPort || process.env.SWANK_PORT || '4200', 10);
const TELNET_PORT = parseInt(options.telnetPort || process.env.TELNET_PORT || '4023', 10);

// Other configuration
// Version information
const VERSION = '1.0.1';

// Constants for Docker image configuration
const DEFAULT_IMPL = 'ccl';
const DEFAULT_BRANCH = 'master';
const DEFAULT_IMAGE_BASE = 'dcooper8/gendl';

// Validate Lisp implementation
const SUPPORTED_IMPLS = ['ccl', 'sbcl'];

// Get Lisp implementation from arguments or environment variable
const LISP_IMPL = (options.lispImpl || process.env.LISPLY_LISP_IMPL || process.env.LISPY_LISP_IMPL || process.env.GENDL_LISP_IMPL || DEFAULT_IMPL).toLowerCase();

// Service startup flags
const START_HTTP = options.startHttp !== false && (process.env.START_HTTP !== 'false'); // Default to true like SWANK
const START_HTTPS = options.startHttps || process.env.START_HTTPS === 'true' || false;
const START_SWANK = options.startSwank !== false && (process.env.START_SWANK !== 'false'); // Default to true
const START_TELNET = options.startTelnet || process.env.START_TELNET === 'true' || false;

// Lisp REPL communication configuration
// Enable stdio capability by default (but http is still the default communication mode)
const USE_STDIO = options.useStdio !== false && process.env.USE_STDIO !== 'false';

// Default REPL prompts by implementation
const DEFAULT_PROMPTS = {
  'ccl': '?',
  'sbcl': '*'
};

// Debugger prompts by implementation (for future use)
const DEBUGGER_PROMPTS = {
  'ccl': '>', // CCL debugger prompt
  'sbcl': '0]' // SBCL debugger level 0 prompt
};

// Get the appropriate REPL prompt for the selected Lisp implementation
const REPL_PROMPT = options.replPrompt || process.env.REPL_PROMPT || DEFAULT_PROMPTS[LISP_IMPL] || '?';
const EVAL_TIMEOUT = parseInt(options.evalTimeout || process.env.EVAL_TIMEOUT || '30000', 10);

// Set up endpoint paths
const EVAL_ENDPOINT = options.evalEndpoint || process.env.EVAL_ENDPOINT || '/lisply/lisp-eval';
const PING_ENDPOINT = options.pingEndpoint || process.env.PING_ENDPOINT || '/lisply/ping-lisp';

// Set up logging to file for debugging
const LOG_FILE = options.logFile || process.env.LISPLY_LOG_FILE || process.env.LISPY_LOG_FILE || process.env.GENDL_LOG_FILE || '/tmp/mcp-wrapper.log';
const DEBUG_MODE = options.debug || process.env.DEBUG_LISPLY === 'true' || process.env.DEBUG_LISPY === 'true' || process.env.DEBUG_GENDL === 'true';
let logStream;

try {
  logStream = fs.createWriteStream(LOG_FILE, { flags: 'a' });
  logStream.write(`\n\n---- STARTING MCP WRAPPER LOG AT ${new Date().toISOString()} ----\n\n`);
} catch (error) {
  console.error(`Failed to create log file: ${error.message}`);
}

// logging function
const log = (level, message) => {
  const timestamp = new Date().toISOString();
  const logMessage = `[${timestamp}] [${level}] ${message}`;
  
  // Log to stderr
  console.error(`[MCP-WRAPPER] ${logMessage}`);
  
  // Log to file if available
  if (logStream) {
    logStream.write(logMessage + '\n');
  }
};


// Logger implementation
const logger = {
  error: (message) => log('ERROR', message),
  warn: (message) => log('WARN', message),
  info: (message) => log('INFO', message),
  debug: (message) => DEBUG_MODE ? log('DEBUG', message) : undefined
};


// Function to get the current branch name
function getCurrentBranch() {
  try {
    // Try to read the current branch from git
    const gitHeadContent = fs.readFileSync(path.join(__dirname, '..', '.git', 'HEAD'), 'utf8').trim();
    // Check if HEAD points to a branch reference
    if (gitHeadContent.startsWith('ref: refs/heads/')) {
      // Extract branch name from ref
      const branchName = gitHeadContent.substring('ref: refs/heads/'.length);
      logger.info(`Current git branch detected: ${branchName}`);
      return branchName;
    }
    logger.warn('Git HEAD does not point to a branch reference, using default branch');
    return DEFAULT_BRANCH;
  } catch (error) {
    logger.warn(`Could not determine current branch: ${error.message}, using default branch`);
    return DEFAULT_BRANCH;
  }
}

// Construct Docker image name
function constructDockerImageName() {
  const branchName = options.imageBranch || process.env.LISPLY_IMAGE_BRANCH || process.env.LISPY_IMAGE_BRANCH || getCurrentBranch();
  // Convert any slashes in branch name to double hyphens for Docker image tag
  const formattedBranch = branchName.replace(/\//g, '--');
  const impl = SUPPORTED_IMPLS.includes(LISP_IMPL) ? LISP_IMPL : DEFAULT_IMPL;
  const baseName = options.imageBaseName || process.env.LISPLY_IMAGE_BASE || process.env.LISPY_IMAGE_BASE || DEFAULT_IMAGE_BASE;
  return `${baseName}:${formattedBranch}-${impl}`;
}

const DOCKER_IMAGE = options.dockerImage || process.env.LISPLY_DOCKER_IMAGE || process.env.LISPY_DOCKER_IMAGE || process.env.GENDL_DOCKER_IMAGE || constructDockerImageName();
const AUTO_START = options.autoStart !== false && (process.env.LISPLY_AUTO_START !== 'false' && process.env.LISPY_AUTO_START !== 'false' && process.env.GENDL_AUTO_START !== 'false');
const DOCKER_SOCKET = options.dockerSocket || process.env.DOCKER_SOCKET || '/var/run/docker.sock';
const MOUNTS = options.mount || []; // Mount points from command line
const ENV_MOUNTS = process.env.LISPLY_MOUNTS ? process.env.LISPLY_MOUNTS.split(',') : 
                 (process.env.LISPY_MOUNTS ? process.env.LISPY_MOUNTS.split(',') : 
                 (process.env.GENDL_MOUNTS ? process.env.GENDL_MOUNTS.split(',') : [])); // Mount points from env
const ALL_MOUNTS = [...MOUNTS, ...ENV_MOUNTS];

// Base path for MCP endpoints
const MCP_BASE_PATH = process.env.MCP_BASE_PATH || process.env.GENDL_BASE_PATH || '/lisply';



// Now that logger is initialized, validate Lisp implementation
if (!SUPPORTED_IMPLS.includes(LISP_IMPL)) {
  logger.warn(`Unsupported Lisp implementation: ${LISP_IMPL}, defaulting to ${DEFAULT_IMPL}`);
}

logger.info(`Starting MCP wrapper with backend host: ${BACKEND_HOST}, SWANK port: ${SWANK_HOST_PORT}, HTTP port: ${HTTP_HOST_PORT}`);
logger.info(`Auto-start is ${AUTO_START ? 'enabled' : 'disabled'}, Docker image: ${DOCKER_IMAGE}`);
logger.info(`Stdio capability for local containers: ${USE_STDIO ? 'enabled' : 'disabled'} (default: enabled), HTTP is default mode, Prompt: '${REPL_PROMPT}', Timeout: ${EVAL_TIMEOUT}ms`);
logger.info(`Endpoints - Eval: ${EVAL_ENDPOINT}, Ping: ${PING_ENDPOINT}`);
if (ALL_MOUNTS.length > 0) {
  logger.info(`Configured mounts: ${ALL_MOUNTS.join(', ')}`);
}

// Main promise chain
checkBackendAvailability()
  .then(available => {
    if (available) {
      logger.info(`Backend service already available at ${BACKEND_HOST}:${HTTP_HOST_PORT}`);
      startMcpWrapper();
      return Promise.resolve(true);
    } else {
      const isLocalHost = (BACKEND_HOST === '127.0.0.1' || BACKEND_HOST === 'localhost');
      const dockerAvailable = isDockerAvailable();
      
      if (AUTO_START && isLocalHost && dockerAvailable) {
        logger.info(`Backend server not available, attempting to start container`);
        return startBackendContainer();
      } else {
        logger.error(`Cannot start backend container. Conditions not met:
  - Auto-start: ${AUTO_START}
  - Local host: ${isLocalHost}
  - Docker available: ${dockerAvailable}`);
        process.exit(1);
      }
    }
  })
  .then((containerStarted) => {
    if (!isMcpWrapperStarted) {
      startMcpWrapper();
    }
  })
  .catch(error => {
    logger.error(`Failed to start MCP wrapper: ${error.message}`);
    if (error.stack) {
      logger.error(`Error stack: ${error.stack}`);
    }
    process.exit(1);
  });


// Simplified backend availability check
function checkBackendAvailability() {
  return new Promise((resolve) => {
    logger.info(`Checking backend HTTP service availability at ${BACKEND_HOST}:${HTTP_HOST_PORT}`);
    
    const options = {
      hostname: BACKEND_HOST,
      port: HTTP_HOST_PORT,
      path: PING_ENDPOINT,
      method: 'GET',
      timeout: 5000
    };

    const req = http.request(options, (res) => {
      let responseBody = '';

      res.on('data', (chunk) => {
        responseBody += chunk;
      });

      res.on('end', () => {
        if (res.statusCode === 200 && responseBody.length > 0) {
          logger.info(`Gendl service is available. Ping response: ${responseBody}`);
          resolve(true);
        } else {
          logger.warn(`Gendl service ping failed. Status: ${res.statusCode}, Response length: ${responseBody.length}`);
          resolve(false);
        }
      });
    });

    req.on('timeout', () => {
      logger.warn('Gendl service ping request timed out');
      req.destroy();
      resolve(false);
    });

    req.on('error', (error) => {
      logger.warn(`Gendl service ping request error: ${error.message}`);
      resolve(false);
    });

    req.end();
  });
}

// Removed unused shouldStartGendlContainer function

// Check if docker is available
function isDockerAvailable() {
  try {
    // Check if we're running in a container
    const cgroup = fs.readFileSync('/proc/self/cgroup', 'utf8');
    const isContainer = cgroup.includes('docker');
    
    // If we're in a container, we need the Docker socket to be mounted
    if (isContainer) {
      return fs.existsSync(DOCKER_SOCKET);
    }
    
    // If we're on the host, just check if docker command works
    execSync('docker --version', { stdio: 'ignore' });
    return true;
  } catch (error) {
    logger.warn(`Docker does not seem to be available: ${error.message}`);
    return false;
  }
}


// Removed unused getDockerCommand function

// Global variable to track if MCP wrapper is started
let isMcpWrapperStarted = false;

// Global variable to store the container name for reference and logging
let backendContainerName = null;

// Try to find and attach to an existing backend container
function tryAttachToContainer() {
  try {
    logger.info('Attempting to find and attach to existing backend container');
    
    // Find containers using our expected ports
    const command = `docker ps --filter "publish=${HTTP_HOST_PORT}" --format "{{.ID}}:{{.Names}}"`;
    const result = execSync(command, { encoding: 'utf8' }).trim();
    
    if (!result) {
      logger.info('No running containers found with matching port');
      return false;
    }
    
    // Parse container info
    const containers = result.split('\n').map(line => {
      const [id, name] = line.split(':');
      return { id, name };
    });
    
    logger.info(`Found ${containers.length} potential containers: ${JSON.stringify(containers)}`);
    
    if (containers.length === 0) {
      return false;
    }
    
    // Choose the first container
    const container = containers[0];
    backendContainerName = container.name;
    
    logger.info(`Connecting to container ${container.id} (${container.name})`);
    
    // Try using 'exec' instead of 'attach' for better stability
    // The -i flag keeps stdin open
    const dockerProcess = spawn('docker', ['exec', '-i', container.id, 'ccl', '--no-init', '--quiet'], {
      stdio: ['pipe', 'pipe', 'pipe'] // Keep stdin open with pipe
    });
    
    // Add explicit exit handler to log when the process exits
    dockerProcess.on('exit', (code, signal) => {
      logger.error(`Docker exec process exited with code ${code} and signal ${signal}`);
      global.dockerProcess = null;
    });
    
    // Handle potential errors
    dockerProcess.on('error', (error) => {
      logger.error(`Error executing in container: ${error.message}`);
      return false;
    });
    
    // Store the process globally
    global.dockerProcess = dockerProcess;
    
    // Setup event handlers for the process
    dockerProcess.stdout.on('data', (data) => {
      const output = data.toString();
      logger.debug(`Container stdout: ${output.substring(0, 100)}${output.length > 100 ? '...' : ''}`);
    });
    
    dockerProcess.stderr.on('data', (data) => {
      logger.error(`Container stderr: ${data.toString().trim()}`);
    });
    
    // Send a newline to check if the container is responsive
    dockerProcess.stdin.write('\n');
    
    logger.info('Successfully started Lisp REPL in container');
    return true;
  } catch (error) {
    logger.error(`Failed to connect to container: ${error.message}`);
    return false;
  }
}

// Check if Docker login is valid and attempt login if necessary
async function ensureDockerLogin() {
  return new Promise((resolve) => {
    logger.info('Checking Docker Hub authentication status');
    exec('docker info', (error, stdout, stderr) => {
      // If docker info works, we have docker access
      if (error) {
        logger.warn(`Docker info failed: ${error.message}`);
        resolve(false);
        return;
      }
      
      // Check if we can access Docker Hub - try a simple pull of a small public image
      exec('docker pull hello-world:latest', (error, stdout, stderr) => {
        if (error) {
          logger.warn(`Docker authentication check failed: ${error.message}`);
          logger.info('Attempting Docker Hub login...');
          
          // Try to login interactively or with stored credentials
          exec('docker login', (error, stdout, stderr) => {
            if (error) {
              logger.warn(`Docker login failed: ${error.message}`);
              resolve(false);
            } else {
              logger.info('Docker login successful');
              resolve(true);
            }
          });
        } else {
          logger.info('Docker authentication is valid');
          resolve(true);
        }
      });
    });
  });
}

// First try to use the image matching the current branch
async function pullLatestBackendImage() {
  return new Promise(async (resolve) => {
    let currentImage = DOCKER_IMAGE; // Store the current image name
    logger.info(`Attempting to pull latest backend image: ${currentImage}`);
    
    // Try pulling the image matching the current branch
    try {
      await execPromise(`docker pull ${currentImage}`);
      logger.info(`Successfully pulled latest backend image: ${currentImage}`);
      resolve({ success: true, image: currentImage });
      return;
    } catch (pullError) {
      logger.warn(`Failed to pull latest backend image: ${pullError.message}`);
      
      // Check if the image exists locally
      try {
        await execPromise(`docker image inspect ${currentImage}`);
        logger.info(`Using existing local backend image: ${currentImage}`);
        resolve({ success: true, image: currentImage });
        return;
      } catch (inspectError) {
        logger.warn(`Backend image ${currentImage} does not exist locally`);
        
        // If the current image is already the default, we've run out of options
        const imageBaseName = options.imageBaseName || process.env.LISPY_IMAGE_BASE || DEFAULT_IMAGE_BASE;
        const defaultImage = `${imageBaseName}:${DEFAULT_BRANCH}-${DEFAULT_IMPL}`;
        if (currentImage === defaultImage) {
          logger.error(`No suitable backend image available`);
          resolve(false);
          return;
        }
        
        // Try to pull the default image as fallback
        logger.info(`Trying to pull default image: ${defaultImage}`);
        try {
          await execPromise(`docker pull ${defaultImage}`);
          logger.info(`Successfully pulled default backend image`);
          // Update the environment variable with the new value
          process.env.LISPY_DOCKER_IMAGE = defaultImage;
          resolve({ success: true, image: defaultImage });
          return;
        } catch (defaultPullError) {
          // Try to check if default image exists locally
          try {
            await execPromise(`docker image inspect ${defaultImage}`);
            logger.info(`Using existing local default backend image: ${defaultImage}`);
            process.env.LISPY_DOCKER_IMAGE = defaultImage;
            resolve({ success: true, image: defaultImage });
            return;
          } catch (defaultInspectError) {
            logger.error(`Failed to find or pull default backend image: ${defaultPullError.message}`);
            resolve(false);
            return;
          }
        }
      }
    }
  });
}

// Promise wrapper for exec
function execPromise(command) {
  return new Promise((resolve, reject) => {
    exec(command, (error, stdout, stderr) => {
      if (error) {
        reject(error);
      } else {
        resolve(stdout.trim());
      }
    });
  });
}

// Modify startBackendContainer to handle port-in-use scenarios and keep stdin open
function startBackendContainer() {
  return new Promise(async (resolve, reject) => {

    // Existing container startup logic...
    try {
      if (!isDockerAvailable()) {
        return reject(new Error('Docker is not available'));
      }

      const loginStatus = await ensureDockerLogin();
      if (loginStatus) {
        logger.info('Docker login confirmed, proceeding with image pull');
      } else {
        logger.warn('Docker login not confirmed, will try to use local images');
      }

      const pullResult = await pullLatestBackendImage();
      if (!pullResult || !pullResult.success) {
        logger.warn('Could not pull or find a suitable backend image');
        return reject(new Error('No suitable backend image available'));
      }

      const dockerImage = pullResult.image || DOCKER_IMAGE;
      logger.info(`Preparing to start backend container using image ${dockerImage}`);

      // in debug mode, log the following `-e` environment variables
      // so we can see what's going on when docker container starts:
      logger.debug(`Environment variables at container start:
      START_HTTP: ${START_HTTP}
      HTTP_PORT: ${HTTP_PORT}
      HTTP_HOST_PORT: ${HTTP_HOST_PORT}
      START_HTTPS: ${START_HTTPS}
      HTTPS_PORT: ${HTTPS_PORT}
      HTTPS_HOST_PORT: ${HTTPS_HOST_PORT}
      START_SWANK: ${START_SWANK}
      SWANK_PORT: ${SWANK_PORT}
      SWANK_HOST_PORT: ${SWANK_HOST_PORT}
      START_TELNET: ${START_TELNET}
      TELNET_PORT: ${TELNET_PORT}
      TELNET_HOST_PORT: ${TELNET_HOST_PORT}
      `);
      
      // Generate a unique container name with timestamp
      // Extract the base name from the image for the container name
      const imageBaseName = dockerImage.split('/').pop().split(':')[0];
      const containerName = `${imageBaseName}-mcp-${new Date().getTime()}`;
      
      // Prepare docker arguments for spawn
      const dockerArgs = [
        'run',
        '-i',
        '--rm',
        '--name', containerName,
        '-e', `START_HTTP=${START_HTTP}`,
        '-e', `HTTP_PORT=${HTTP_PORT}`,
        '-e', `HTTP_HOST_PORT=${HTTP_HOST_PORT}`,
        '-e', `START_HTTPS=${START_HTTPS}`,
        '-e', `HTTPS_PORT=${HTTPS_PORT}`,
        '-e', `HTTPS_HOST_PORT=${HTTPS_HOST_PORT}`,
        '-e', `START_SWANK=${START_SWANK}`,
        '-e', `SWANK_PORT=${SWANK_PORT}`,
        '-e', `SWANK_HOST_PORT=${SWANK_HOST_PORT}`,
        '-p', `${SWANK_HOST_PORT}:${SWANK_PORT}`,
        '-p', `${HTTP_HOST_PORT}:${HTTP_PORT}`,
        // Add ports for HTTPS and TELNET if enabled
        ...(START_HTTPS ? ['-p', `${HTTPS_HOST_PORT}:${HTTPS_PORT}`] : []),
        ...(START_TELNET ? ['-p', `${TELNET_HOST_PORT}:${TELNET_PORT}`] : []),
        // Add any mount points
        ...ALL_MOUNTS.flatMap(mount => ['-v', mount])
      ];
      
      // Add the image name as the last argument
      dockerArgs.push(dockerImage);
      
      // Log the complete docker command
      logger.debug(`Docker command: docker ${dockerArgs.join(' ')}`);
      
      // Final availability check immediately before container spawning to handle race conditions
      const finalAvailabilityCheck = await checkBackendAvailability();
      if (finalAvailabilityCheck) {
        logger.info('Backend service became available during final check. Skipping container start.');
        return resolve(true);
      }
      
      // Use spawn instead of exec to keep stdin open
      const dockerProcess = spawn('docker', dockerArgs, {
        stdio: ['pipe', 'pipe', 'pipe'] // Keep stdin open with pipe
      });
      
      // Store the container name and log container output
      backendContainerName = containerName;
      logger.info(`Started backend container with name: ${containerName}`);
      
      // Just log the container output
      dockerProcess.stdout.on('data', (data) => {
        const output = data.toString().trim();
        logger.debug(`Docker stdout: ${output}`);
      });
      
      // Log stderr
      dockerProcess.stderr.on('data', (data) => {
        const errorMsg = data.toString().trim();
        logger.error(`Docker stderr: ${errorMsg}`);
        
        // Check for port already in use
        if (errorMsg.includes('port is already allocated')) {
          logger.warn('Port already in use. Another process likely started the container before we could.');
          dockerProcess.kill(); // Kill the process
          return resolve(true);
        }
      });
      
      // Handle process exit
      dockerProcess.on('exit', (code, signal) => {
        if (code !== 0) {
          logger.error(`Docker process exited with code ${code} and signal ${signal}`);
          return reject(new Error(`Docker process exited with code ${code}`));
        }
      });
      
      // Handle process errors
      dockerProcess.on('error', (error) => {
        logger.error(`Failed to start Docker container: ${error.message}`);
        return reject(error);
      });
      
      // Keep the stdin pipe open (critical to prevent container from exiting)
      dockerProcess.stdin.on('error', (error) => {
        logger.error(`Docker stdin error: ${error.message}`);
      });
      
      // Store the process globally so it stays alive with the script
      global.dockerProcess = dockerProcess;
      
      // Add explicit exit handler to log when the process exits
      dockerProcess.on('exit', (code, signal) => {
        logger.error(`Docker container process exited with code ${code} and signal ${signal}`);
        global.dockerProcess = null;
      });
      
      // Wait briefly for the container to start before continuing
      setTimeout(() => {
        // Check if the container is running using the container name
        exec(`docker ps --filter "name=${containerName}" --format "{{.ID}}"`, (error, stdout, stderr) => {
          if (error) {
            logger.error(`Error checking container status: ${error.message}`);
            return reject(error);
          }
          
          if (stdout.trim()) {
            // Container is running, wait for the server to become available
            logger.info(`Container ${containerName} is running, waiting for backend server to start`);
            waitForBackendServer(30)
              .then(() => resolve(true))
              .catch(error => reject(error));
          } else {
            // Container may still be starting, wait a bit more
            logger.info('Container not found in ps output yet, waiting more time for startup');
            setTimeout(() => {
              exec(`docker ps --filter "name=${containerName}" --format "{{.ID}}"`, (error, stdout, stderr) => {
                if (error) {
                  logger.error(`Error checking container status: ${error.message}`);
                  return reject(error);
                }
                
                if (stdout.trim()) {
                  logger.info(`Container ${containerName} is now running, waiting for backend server to start`);
                  waitForBackendServer(30)
                    .then(() => resolve(true))
                    .catch(error => reject(error));
                } else {
                  logger.warn(`Container ${containerName} not found after extended wait`);
                  // Try to continue anyway - the container might still be starting
                  waitForBackendServer(30)
                    .then(() => resolve(true))
                    .catch(error => reject(error));
                }
              });
            }, 2000);
          }
        });
      }, 1000);
    } catch (error) {
      logger.error(`Container startup error: ${error.message}`);
      reject(error);
    }
  });
}

// Wait for the backend server to become available
function waitForBackendServer(maxWaitSeconds) {
  return new Promise((resolve, reject) => {
    logger.info(`Waiting up to ${maxWaitSeconds} seconds for backend server to become available`);
    
    let attempts = 0;
    const maxAttempts = maxWaitSeconds;
    const interval = 1000; // 1 second interval
    
    const check = () => {
      attempts++;
      
      checkBackendAvailability()
        .then(available => {
          if (available) {
            logger.info(`Backend server is now available after ${attempts} seconds`);
            resolve();
          } else if (attempts < maxAttempts) {
            logger.debug(`Waiting for backend server (attempt ${attempts}/${maxAttempts})...`);
            setTimeout(check, interval);
          } else {
            reject(new Error(`Backend server did not become available after ${maxAttempts} seconds`));
          }
        })
        .catch(error => {
          reject(error);
        });
    };
    
    // Start checking
    check();
  });
}

// Start the MCP wrapper
function startMcpWrapper() {
  // Set flag to prevent double-starting
  isMcpWrapperStarted = true;
  
  logger.info('Starting MCP wrapper');
  
  // Create readline interface for stdin/stdout communication
  let rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    terminal: false
  });
  
  // Handle MCP requests
  rl.on('line', (line) => {
    logger.info(`Received: ${line}`);
    
    try {
      const request = JSON.parse(line);
      
      switch (request.method) {
        case 'initialize':
          handleInitialize(request);
          break;
        case 'tools/call':
          handleToolCall(request);
          break;
        case 'tools/list':
          handleToolsList(request);
          break;
        case 'resources/list':
          sendStandardResponse(request, { resources: [] });
          break;
        case 'prompts/list':
          sendStandardResponse(request, { prompts: [] });
          break;
        case 'notifications/initialized':
          // Just acknowledge notification, no response needed
          logger.info('Received initialization notification');
          break;
        default:
          // Send method not supported error for any other method
          logger.warn(`Unsupported method: ${request.method}`);
          sendErrorResponse(request, -32601, `Method not supported: ${request.method}`);
      }
    } catch (error) {
      logger.error(`Error processing request: ${error.message}`);
      if (line && line.includes('"id"')) {
        try {
          const id = JSON.parse(line).id;
          sendErrorResponse({ id }, -32603, `Internal error: ${error.message}`);
        } catch (e) {
          logger.error(`Could not extract request ID: ${e.message}`);
        }
      }
    }
  });
  
  // Handle process events
  setupProcessEvents(rl);
}


// Set up process event handlers
function setupProcessEvents(rl) {
  // Function to clean up resources on exit
  const cleanup = async () => {
    logger.info('Cleanup started'); // Log when cleanup begins
    
    // Check if we're the process that started the container or just attached to it
    if (global.dockerProcess) {
      // If we started the container, it will terminate automatically due to --rm flag
      // If we attached to the container, we need to detach cleanly
      if (backendContainerName && !backendContainerName.includes('-mcp-')) {
        // We likely attached to an existing container, so just detach
        logger.info(`Detaching from container ${backendContainerName}`);
        
        try {
          // Send CTRL+P CTRL+Q to detach from the container
          if (global.dockerProcess.stdin.writable) {
            // We need to do this without actually detaching ourselves,
            // so we'll just close our stdin pipe
            global.dockerProcess.stdin.end();
          }
        } catch (error) {
          logger.error(`Error detaching from container: ${error.message}`);
        }
      } else {
        logger.info('Container will be removed automatically due to --rm flag');
      }
    }
    
    if (logStream) {
      logStream.end();
    }
    if (rl) rl.close();
    
    logger.info('Cleanup completed'); // Log when cleanup finishes
  };

  // Add event handlers to prevent unexpected termination
  process.on('SIGINT', async () => {
    logger.info('Received SIGINT signal - cleaning up and exiting');
    await cleanup();
    process.exit(0);
  });
  
  process.on('SIGTERM', async () => {
    logger.info('Received SIGTERM signal - cleaning up and exiting');
    await cleanup();
    process.exit(0);
  });

  // Add handler for normal exit (uncaught process exit)
  process.on('exit', (code) => {
    logger.info(`Process exiting with code ${code} - cleanup should have run`);
  });

  // Add handler for unhandled promise rejections to catch async issues
  process.on('unhandledRejection', (reason, promise) => {
    logger.error(`Unhandled promise rejection: ${reason}`);
  });
}


// Removed unused fastTerminateGendlContainer function

// Handle MCP initialization
function handleInitialize(request) {
  logger.info('Handling initialize request');
  
  // Send successful initialization response
  const response = {
    jsonrpc: '2.0',
    id: request.id,
    result: {
      protocolVersion: request.params.protocolVersion || '0.1.0',
      capabilities: {
        experimental: {},
        prompts: { listChanged: false },
        resources: { subscribe: false, listChanged: false },
        tools: { listChanged: false }
      },
      serverInfo: {
        name: 'lisply-mcp',
        version: VERSION
      }
    }
  };
  
  sendResponse(response);
  logger.info('Initialization complete');
}


// Handle tool list requests
function handleToolsList(request) {
  logger.info('Handling tools/list request');
  
  const options = {
    hostname: BACKEND_HOST,
    port: HTTP_HOST_PORT,
    path: `${MCP_BASE_PATH}/tools/list`,
    method: 'GET'
  };
  
  makeHttpRequest(options, null, (error, response) => {
    if (error) {
      logger.error(`Error fetching tools list: ${error.message}`);
      sendErrorResponse(request, -32603, `Error fetching tools list: ${error.message}`);
      return;
    }
    
    try {
      const toolsData = JSON.parse(response.content);
      
      // Validate the response structure
      if (!toolsData.tools || !Array.isArray(toolsData.tools)) {
        throw new Error('Invalid tools list response format');
      }
      
      sendStandardResponse(request, toolsData);
    } catch (parseError) {
      logger.error(`Error parsing tools list: ${parseError.message}`);
      sendErrorResponse(request, -32603, `Error parsing tools list: ${parseError.message}`);
    }
  }, 'TOOLS-LIST');
}

// Handle tool calls
async function handleToolCall(request) {
  const toolName = request.params?.name;
  const args = request.params?.arguments || {};
  
  logger.info(`Handling tool call: ${toolName}`);
  
  try {
    // Special handling for each tool
    if (toolName === 'http_request') {
      return handleHttpRequest(request, args);
    } else if (toolName === 'ping_gendl') {
      return handlePingGendl(request);
    } else if (toolName === 'lisp_eval') {
      return handleLispEval(request, args);
    // Removed KB query handler
    } else {
      // Unknown tool
      sendErrorResponse(request, -32601, `Unknown tool: ${toolName}`);
    }
  } catch (error) {
    logger.error(`Error in tool call: ${error.message}`);
    sendErrorResponse(request, -32603, `Error calling tool: ${error.message}`);
  }
}

// Handle HTTP request with support for all methods and bodies
async function handleHttpRequest(request, args) {
  logger.info(`Handling http_request: ${JSON.stringify(args)}`);
  
  try {
    // Check for required path parameter
    if (!args.path) {
      sendErrorResponse(request, -32602, "Missing required parameter: path");
      return;
    }
    
    // Prepare the request options
    const options = {
      hostname: BACKEND_HOST,
      port: HTTP_HOST_PORT,
      path: args.path,
      method: args.method || 'GET',
      headers: {
        'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8'
      }
    };
    
    // Add custom headers if provided
    if (args.headers && typeof args.headers === 'object') {
      options.headers = { ...options.headers, ...args.headers };
    }
    
    // If Content-Type is not specified for POST/PUT and we have a body, default to application/json
    if (['POST', 'PUT'].includes(options.method) && args.body && 
        !options.headers['Content-Type'] && !options.headers['content-type']) {
      options.headers['Content-Type'] = 'application/json';
    }
    
    makeHttpRequest(options, args.body, (error, response) => {
      if (error) {
        logger.error(`HTTP request error: ${error.message}`);
        sendErrorResponse(request, -32603, `Error making HTTP request: ${error.message}`);
        return;
      }
      
      // If rawResponse is set, return the full response object
      if (args.rawResponse) {
        sendTextResponse(request, JSON.stringify(response, null, 2));
      } else {
        // Otherwise, return just the content
        sendTextResponse(request, response.content);
      }
    });
  } catch (error) {
    logger.error(`Error in http_request: ${error.message}`);
    sendErrorResponse(request, -32603, `Error making HTTP request: ${error.message}`);
  }
}


// Helper function to make HTTP requests with unified logging and error handling
function makeHttpRequest(options, body, callback, logPrefix = '') {
  const prefix = logPrefix ? `[${logPrefix}] ` : '';
  logger.debug(`${prefix}Making ${options.method} request to http://${options.hostname}:${options.port}${options.path}`);
  logger.debug(`${prefix}Request headers: ${JSON.stringify(options.headers)}`);
  if (body) {
    logger.debug(`${prefix}Request body: ${body.substring(0, 500)}${body.length > 500 ? '...' : ''}`);
  }
  
  const req = http.request(options, (res) => {
    let data = '';
    
    res.on('data', (chunk) => {
      data += chunk;
      if (logPrefix) {
        logger.debug(`${prefix}Received chunk: ${chunk.toString().substring(0, 100)}...`);
      }
    });
    
    res.on('end', () => {
      logger.debug(`${prefix}Response status: ${res.statusCode}, Content-Type: ${res.headers['content-type']}`);
      logger.debug(`${prefix}Response data: ${data.substring(0, 500)}${data.length > 500 ? '...' : ''}`);
      
      // Follow redirects
      if ([301, 302, 303, 307, 308].includes(res.statusCode)) {
        const location = res.headers.location;
        
        if (location) {
          logger.info(`${prefix}Following redirect to: ${location}`);
          
          // Create new options for the redirect
          const redirectOptions = {
            ...options,
            path: location
          };
          
          // For 303 redirects, use GET
          if (res.statusCode === 303) {
            redirectOptions.method = 'GET';
          }
          
          // Follow the redirect
          return makeHttpRequest(redirectOptions, null, callback, logPrefix);
        }
      }
      
      // Return a comprehensive response object
      callback(null, {
        content: data,
        statusCode: res.statusCode,
        headers: res.headers,
        finalUrl: options.path
      });
    });
  });
  
  req.on('error', (error) => {
    logger.error(`${prefix}HTTP request error: ${error.message}`);
    callback(error);
  });
  
  // Set timeout to prevent hanging
  req.setTimeout(10000, () => {
    logger.error(`${prefix}Request timed out after 10 seconds`);
    req.destroy();
    callback(new Error("Request timed out"));
  });
  
  // Send the request body if present
  if (body && ['POST', 'PUT', 'PATCH'].includes(options.method)) {
    req.write(body);
  }
  
  req.end();
  if (logPrefix) {
    logger.debug(`${prefix}Request sent`);
  }
}

// Handle simple ping_gendl tool
function handlePingGendl(request) {
  logger.info('Handling ping_gendl request');
  
  const options = {
    hostname: BACKEND_HOST,
    port: HTTP_HOST_PORT,
    path: PING_ENDPOINT,
    method: 'GET'
  };
  
  makeHttpRequest(options, null, (error, response) => {
    if (error) {
      logger.error(`Error pinging backend server: ${error.message}`);
      sendErrorResponse(request, -32603, `Error pinging backend server: ${error.message}`);
      return;
    }
    
    // Just return the raw text response
    sendTextResponse(request, response.content);
  }, 'PING');
}

// Handle lisp_eval tool with either HTTP or stdio based on mode parameter
function handleLispEval(request, args) {
  logger.info(`Handling lisp_eval with code: ${args.code?.substring(0, 100)}${args.code?.length > 100 ? '...' : ''}`);
  
  try {
    // Check for required parameter
    if (!args.code) {
      sendErrorResponse(request, -32602, "Missing required parameter: code");
      return;
    }
    
    // Check the mode parameter (default to 'http' if not specified)
    const requestedMode = (args.mode || 'http').toLowerCase();
    logger.info(`Requested mode: ${requestedMode}`);
    
    // Check if we should use stdio for local containers
    const isLocalHost = (BACKEND_HOST === '127.0.0.1' || BACKEND_HOST === 'localhost');
    
    // First check if we already have a direct stdio connection
    let canUseStdio = isLocalHost && global.dockerProcess && USE_STDIO;
    
    // If we don't have a direct connection but stdio is enabled, try to find and attach to the container
    if (isLocalHost && !global.dockerProcess && USE_STDIO && requestedMode === 'stdio') {
      canUseStdio = tryAttachToContainer();
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
      handleLispEvalViaStdio(request, args);
    } else {
      // Debugger mode requires stdio
      if (isDebuggerCommand) {
        logger.error(`Debugger mode requires local container with stdio enabled`);
        sendErrorResponse(request, -32603, `Debugger mode requires local container with stdio enabled`);
        return;
      }
      
      // Use HTTP communication
      logger.info(`Using HTTP for Lisp evaluation with ${BACKEND_HOST}`);
      handleLispEvalViaHttp(request, args);
    }
  } catch (error) {
    logger.error(`Error in lisp_eval: ${error.message}`);
    if (error.stack) {
      logger.error(`Error stack: ${error.stack}`);
    }
    sendErrorResponse(request, -32603, `Error evaluating Lisp code: ${error.message}`);
  }
}

// Handle lisp_eval tool using stdio with local Docker container
function handleLispEvalViaStdio(request, args) {
  // Safety check for dockerProcess
  if (!global.dockerProcess || !global.dockerProcess.stdin || !global.dockerProcess.stdout) {
    logger.error('Docker process or its stdio streams are not available');
    return handleLispEvalViaHttp(request, args); // Fallback to HTTP
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
    const debuggerPrompt = DEBUGGER_PROMPTS[LISP_IMPL];
    
    // Log what we're looking for
    logger.debug(`Looking for REPL prompt: '${REPL_PROMPT}' or debugger prompt: '${debuggerPrompt}'`);
    
    // Check for regular REPL prompt in both places
    const outputEndsWithPrompt = outputTrimmed.endsWith(REPL_PROMPT);
    const responseEndsWithPrompt = responseTrimmed.endsWith(REPL_PROMPT);
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
          const promptIndex = responseData.lastIndexOf(REPL_PROMPT);
          if (promptIndex > 0) {
            // Find the last newline before the prompt
            const lastNewline = responseData.lastIndexOf('\n', promptIndex);
            if (lastNewline > 0) {
              // Get the text between the last newline and the prompt
              cleanedResponse = responseData.substring(0, lastNewline).trim();
            } else {
              // Just remove the prompt if no newline
              cleanedResponse = responseData.replace(new RegExp(REPL_PROMPT + '$'), '').trim();
            }
          } else {
            // Fallback to simple replacement
            cleanedResponse = responseData.replace(new RegExp(REPL_PROMPT + '$'), '').trim();
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
            implementation: LISP_IMPL,
            prompt: debuggerPrompt
          }
        };
        
        try {
          logger.info('Sending debugger response to LLM');
          sendTextResponse(request, responseWithMetadata);
          logger.info('Debugger response sent successfully');
        } catch (error) {
          logger.error(`Error sending debugger response: ${error.message}`);
        }
      } else {
        // Regular response without debugger
        try {
          logger.info('Sending regular response to LLM');
          sendTextResponse(request, cleanedResponse);
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
      logger.warn(`Lisp evaluation timed out after ${EVAL_TIMEOUT}ms`);
      global.dockerProcess.stdout.removeListener('data', stdoutDataHandler);
      
      // Send what we have so far plus timeout message
      const timeoutResponse = responseData + "\n;; ERROR: Evaluation timed out";
      sendTextResponse(request, timeoutResponse);
    }
  }, EVAL_TIMEOUT);
  
  // Set up error handler
  const errorHandler = (error) => {
    logger.error(`Error during Lisp evaluation via stdio: ${error.message}`);
    clearTimeout(responseTimeout);
    global.dockerProcess.stdout.removeListener('data', stdoutDataHandler);
    sendErrorResponse(request, -32603, `Error during Lisp evaluation: ${error.message}`);
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
      if (backendContainerName && !global.dockerProcess) {
        logger.info(`Attempting to reattach to container ${backendContainerName}`);
        // Check if container is still running
        exec(`docker ps --filter "name=${backendContainerName}" --format "{{.ID}}"`, (error, stdout, stderr) => {
          if (!error && stdout.trim()) {
            logger.info(`Container ${backendContainerName} is still running, reattaching`);
            tryAttachToContainer();
          } else {
            logger.error(`Container ${backendContainerName} is no longer running`);
            return handleLispEvalViaHttp(request, args); // Fall back to HTTP
          }
        });
      } else {
        return handleLispEvalViaHttp(request, args); // Fall back to HTTP
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

// Handle lisp_eval tool with HTTP request (original implementation)
function handleLispEvalViaHttp(request, args) {
  // Create JSON payload for the request
  const payload = JSON.stringify({ 
    code: args.code,
    ...(args.package && { package: args.package })
  });
  
  // HTTP POST to lisp-eval endpoint with proper content type
  const options = {
    hostname: BACKEND_HOST,
    port: HTTP_HOST_PORT,
    path: EVAL_ENDPOINT,
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
      sendErrorResponse(request, -32603, `Error evaluating Lisp code: ${error.message}`);
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
            sendTextResponse(request, `Result: ${result.result}, Stdout: ${result.stdout}`);
        } else {
          logger.error(`Lisp eval error result: ${result.error || "Unknown error"}`);
          sendErrorResponse(request, -32603, `Error evaluating Lisp code: ${result.error || "Unknown error"}`);
        }
      } else {
        // Not a standard success/error format, return as-is
        logger.info(`Non-standard JSON format, returning as-is`);
        sendTextResponse(request, JSON.stringify(result, null, 2));
      }
    } catch (e) {
      // Not JSON, treat as text
      logger.info(`Response is not JSON: ${e.message}`);
      sendTextResponse(request, response.content);
    }
  }, 'LISP-EVAL');
}

// Knowledge base queries are now handled via lisp_eval or http_request

// Standard text response format
function sendTextResponse(request, text) {
  logger.debug(`sendTextResponse called with: ${JSON.stringify(text)}`);
  // Check if text is an object and not a string
  if (typeof text === 'object' && text !== null) {
    logger.debug(`Text is an object, converting to string: ${JSON.stringify(text)}`);
    text = JSON.stringify(text, null, 2);
  }
  const response = {
    jsonrpc: '2.0',
    id: request.id,
    result: {
      content: [
        {
          type: 'text',
          text: String(text || '')
        }
      ]
    }
  };
  sendResponse(response);
}

// Standard response for simple data
function sendStandardResponse(request, data) {
  const response = {
    jsonrpc: '2.0',
    id: request.id,
    result: data
  };
  sendResponse(response);
}

// Helper to send MCP responses
function sendResponse(response) {
  const jsonResponse = JSON.stringify(response);
  logger.info(`Sending response: ${jsonResponse.substring(0, 500)}${jsonResponse.length > 500 ? '...' : ''}`);
  console.log(jsonResponse);
}

// Helper to send error responses
function sendErrorResponse(request, code, message) {
  const response = {
    jsonrpc: '2.0',
    id: request.id,
    error: {
      code: code,
      message: message
    }
  };
  sendResponse(response);
}
