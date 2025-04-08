/**
 * docker.js
 * 
 * Docker container management for the MCP wrapper
 */

const fs = require('fs');
const { exec, execSync, spawn } = require('child_process');
const { getGitBranch, getEnvVar, sanitizeDockerTag } = require('./config');

/**
 * Check if Docker is available for container management
 * @param {Object} logger - Logger instance
 * @returns {boolean} - Whether Docker is available
 */
function isDockerAvailable(logger, dockerSocket) {
  try {
    // Check if we're running in a container
    let isContainer = false;
    
    try {
      const cgroup = fs.readFileSync('/proc/self/cgroup', 'utf8');
      isContainer = cgroup.includes('docker');
    } catch (e) {
      // /proc might not be available on all platforms (e.g., Windows)
      logger.debug(`Could not read cgroup info: ${e.message}`);
      // Fall back to checking environment variables
      isContainer = !!process.env.CONTAINER || !!process.env.DOCKER_CONTAINER;
    }
    
    logger.debug(`Running in container: ${isContainer}`);
    
    // If we're in a container, we need the Docker socket to be mounted
    if (isContainer) {
      const socketExists = fs.existsSync(dockerSocket);
      logger.debug(`Docker socket ${dockerSocket} exists: ${socketExists}`);
      return socketExists;
    }
    
    // If we're on the host, check if docker command works
    const dockerVersion = execSync('docker --version', { encoding: 'utf8', stdio: ['pipe', 'pipe', 'pipe'] }).trim();
    if (dockerVersion) {
      logger.debug(`Docker available: ${dockerVersion}`);
      return true;
    } else {
      logger.warn('Docker command not found or not working');
      return false;
    }
  } catch (error) {
    logger.warn(`Docker does not seem to be available: ${error.message}`);
    return false;
  }
}

/**
 * Check if Docker login is valid and attempt login if necessary
 * @param {Object} logger - Logger instance
 * @returns {Promise<boolean>} - Whether Docker login is valid
 */
async function ensureDockerLogin(logger) {
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

/**
 * Construct Docker image name from branch and implementation
 * @param {Object} config - Configuration object
 * @param {Object} logger - Logger instance
 * @returns {string} - Fully qualified Docker image name
 */
function constructDockerImageName(config, logger) {
  const { options } = config;
  
  // Get branch name with fallbacks
  const branchName = options.imageBranch || 
                    getEnvVar('IMAGE_BRANCH', getGitBranch(options));
  
  // Sanitize branch name for Docker tag
  const formattedBranch = sanitizeDockerTag(branchName);
  
  // Get implementation with validation
  const impl = config.SUPPORTED_IMPLS.includes(config.LISP_IMPL) ? config.LISP_IMPL : config.DEFAULT_IMPL;
  
  // Get base name with fallbacks
  const baseName = options.imageBaseName || 
                  getEnvVar('IMAGE_BASE', config.DEFAULT_IMAGE_BASE);
  
  // Log the components used to construct the image name
  logger.debug(`Constructing Docker image name from: Base=${baseName}, Branch=${branchName} (Formatted=${formattedBranch}), Impl=${impl}`);
  
  return `${baseName}:${formattedBranch}-${impl}`;
}

/**
 * Promise wrapper for exec
 * @param {string} command - Command to execute
 * @returns {Promise<string>} - Command output
 */
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

/**
 * Try to pull the latest version of the Docker image
 * @param {Object} config - Configuration object
 * @param {Object} logger - Logger instance
 * @returns {Promise<Object>} - Result object with success flag and image name
 */
async function pullLatestBackendImage(config, logger) {
  return new Promise(async (resolve) => {
    let currentImage = config.options.dockerImage || getEnvVar('DOCKER_IMAGE', constructDockerImageName(config, logger));
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
        const DEFAULT_IMAGE_BASE = 'dcooper8/gendl'; // Define default here to avoid dependency
        const DEFAULT_BRANCH = 'master';
        const DEFAULT_IMPL = 'ccl';
        const imageBaseName = config.options.imageBaseName || process.env.LISPLY_IMAGE_BASE || DEFAULT_IMAGE_BASE;
        const defaultImage = `${imageBaseName}:${DEFAULT_BRANCH}-${DEFAULT_IMPL}`;
        if (currentImage === defaultImage) {
          logger.error(`No suitable backend image available`);
          resolve({ success: false });
          return;
        }
        
        // Try to pull the default image as fallback
        logger.info(`Trying to pull default image: ${defaultImage}`);
        try {
          await execPromise(`docker pull ${defaultImage}`);
          logger.info(`Successfully pulled default backend image`);
          // Update the environment variable with the new value
          process.env.LISPLY_DOCKER_IMAGE = defaultImage;
          resolve({ success: true, image: defaultImage });
          return;
        } catch (defaultPullError) {
          // Try to check if default image exists locally
          try {
            await execPromise(`docker image inspect ${defaultImage}`);
            logger.info(`Using existing local default backend image: ${defaultImage}`);
            process.env.LISPLY_DOCKER_IMAGE = defaultImage;
            resolve({ success: true, image: defaultImage });
            return;
          } catch (defaultInspectError) {
            logger.error(`Failed to find or pull default backend image: ${defaultPullError.message}`);
            resolve({ success: false });
            return;
          }
        }
      }
    }
  });
}

/**
 * Try to find and attach to an existing backend container
 * @param {Object} config - Configuration object
 * @param {Object} logger - Logger instance
 * @returns {boolean} - Whether attachment was successful
 */
function tryAttachToContainer(config, logger) {
  try {
    logger.info('Attempting to find and attach to existing backend container');
    
    // Find containers using our expected ports
    const command = `docker ps --filter "publish=${config.HTTP_HOST_PORT}" --format "{{.ID}}:{{.Names}}"`;
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
    global.backendContainerName = container.name;
    
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

/**
 * Wait for the backend server to become available
 * @param {Object} config - Configuration object
 * @param {Object} logger - Logger instance
 * @param {Function} checkBackendAvailability - Function to check backend availability
 * @param {number} maxWaitSeconds - Maximum wait time in seconds
 * @returns {Promise<boolean>} - Whether the backend server became available
 */
function waitForBackendServer(config, logger, checkBackendAvailability, maxWaitSeconds) {
  return new Promise((resolve, reject) => {
    logger.info(`Waiting up to ${maxWaitSeconds} seconds for backend server to become available`);
    
    let attempts = 0;
    const maxAttempts = maxWaitSeconds;
    const interval = 1000; // 1 second interval
    
    const check = () => {
      attempts++;
      
      checkBackendAvailability(config, logger)
        .then(available => {
          if (available) {
            logger.info(`Backend server is now available after ${attempts} seconds`);
            resolve(true);
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

/**
 * Start a backend container
 * @param {Object} config - Configuration object
 * @param {Object} logger - Logger instance
 * @param {Function} checkBackendAvailability - Function to check backend availability
 * @returns {Promise<boolean>} - Whether the container was started successfully
 */
function startBackendContainer(config, logger, checkBackendAvailability) {
  return new Promise(async (resolve, reject) => {
    try {
      if (!isDockerAvailable(logger, config.DOCKER_SOCKET)) {
        return reject(new Error('Docker is not available'));
      }

      const loginStatus = await ensureDockerLogin(logger);
      if (loginStatus) {
        logger.info('Docker login confirmed, proceeding with image pull');
      } else {
        logger.warn('Docker login not confirmed, will try to use local images');
      }

      const pullResult = await pullLatestBackendImage(config, logger);
      if (!pullResult || !pullResult.success) {
        logger.warn('Could not pull or find a suitable backend image');
        return reject(new Error('No suitable backend image available'));
      }

      const dockerImage = pullResult.image;
      logger.info(`Preparing to start backend container using image ${dockerImage}`);

      // in debug mode, log the following `-e` environment variables
      // so we can see what's going on when docker container starts:
      logger.debug(`Environment variables at container start:
      START_HTTP: ${config.START_HTTP}
      HTTP_PORT: ${config.HTTP_PORT}
      HTTP_HOST_PORT: ${config.HTTP_HOST_PORT}
      START_HTTPS: ${config.START_HTTPS}
      HTTPS_PORT: ${config.HTTPS_PORT}
      HTTPS_HOST_PORT: ${config.HTTPS_HOST_PORT}
      START_SWANK: ${config.START_SWANK}
      SWANK_PORT: ${config.SWANK_PORT}
      SWANK_HOST_PORT: ${config.SWANK_HOST_PORT}
      START_TELNET: ${config.START_TELNET}
      TELNET_PORT: ${config.TELNET_PORT}
      TELNET_HOST_PORT: ${config.TELNET_HOST_PORT}
      `);
      
      // Generate a unique container name with timestamp
      // Extract the base name from the image for the container name
      const imageBaseName = dockerImage.split('/').pop().split(':')[0];
      const containerName = `lisply-mcp-${new Date().getTime()}`; // name lisply to match README.md
      
      // Prepare docker arguments for spawn
      const dockerArgs = [
        'run',
        '-i',
        '--rm',
        '--name', containerName,
        '-e', `START_HTTP=${config.START_HTTP}`,
        '-e', `HTTP_PORT=${config.HTTP_PORT}`,
        '-e', `HTTP_HOST_PORT=${config.HTTP_HOST_PORT}`,
        '-e', `START_HTTPS=${config.START_HTTPS}`,
        '-e', `HTTPS_PORT=${config.HTTPS_PORT}`,
        '-e', `HTTPS_HOST_PORT=${config.HTTPS_HOST_PORT}`,
        '-e', `START_SWANK=${config.START_SWANK}`,
        '-e', `SWANK_PORT=${config.SWANK_PORT}`,
        '-e', `SWANK_HOST_PORT=${config.SWANK_HOST_PORT}`,
        '-p', `${config.SWANK_HOST_PORT}:${config.SWANK_PORT}`,
        '-p', `${config.HTTP_HOST_PORT}:${config.HTTP_PORT}`,
        // Add ports for HTTPS and TELNET if enabled
        ...(config.START_HTTPS ? ['-p', `${config.HTTPS_HOST_PORT}:${config.HTTPS_PORT}`] : []),
        ...(config.START_TELNET ? ['-p', `${config.TELNET_HOST_PORT}:${config.TELNET_PORT}`] : []),
        // Add any mount points
        ...config.ALL_MOUNTS.flatMap(mount => ['-v', mount])
      ];
      
      // Add the image name as the last argument
      dockerArgs.push(dockerImage);
      
      // Log the complete docker command
      logger.debug(`Docker command: docker ${dockerArgs.join(' ')}`);
      
      // Final availability check immediately before container spawning to handle race conditions
      const finalAvailabilityCheck = await checkBackendAvailability(config, logger);
      if (finalAvailabilityCheck) {
        logger.info('Backend service became available during final check. Skipping container start.');
        return resolve(true);
      }
      
      // Use spawn instead of exec to keep stdin open
      const dockerProcess = spawn('docker', dockerArgs, {
        stdio: ['pipe', 'pipe', 'pipe'] // Keep stdin open with pipe
      });
      
      // Store the container name and log container output
      global.backendContainerName = containerName;
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
            waitForBackendServer(config, logger, checkBackendAvailability, 30)
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
                  waitForBackendServer(config, logger, checkBackendAvailability, 30)
                    .then(() => resolve(true))
                    .catch(error => reject(error));
                } else {
                  logger.warn(`Container ${containerName} not found after extended wait`);
                  // Try to continue anyway - the container might still be starting
                  waitForBackendServer(config, logger, checkBackendAvailability, 30)
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

/**
 * Clean up Docker-related resources on exit
 * @param {Object} logger - Logger instance
 * @returns {Promise<void>}
 */
async function cleanup(logger) {
  logger.info('Cleanup started'); // Log when cleanup begins
  
  // Check if we're the process that started the container or just attached to it
  if (global.dockerProcess) {
    // If we started the container, it will terminate automatically due to --rm flag
    // If we attached to the container, we need to detach cleanly
    if (global.backendContainerName && !global.backendContainerName.includes('-mcp-')) {
      // We likely attached to an existing container, so just detach
      logger.info(`Detaching from container ${global.backendContainerName}`);
      
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
  
  logger.info('Cleanup completed'); // Log when cleanup finishes
}

module.exports = {
  isDockerAvailable,
  ensureDockerLogin,
  constructDockerImageName,
  pullLatestBackendImage,
  tryAttachToContainer,
  waitForBackendServer,
  startBackendContainer,
  cleanup
};