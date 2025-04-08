/**
 * logger.js
 * 
 * Logging facilities for the MCP wrapper
 */

const fs = require('fs');

class Logger {
  constructor(logFile, debugMode) {
    this.debugMode = debugMode;
    this.logStream = null;
    
    try {
      this.logStream = fs.createWriteStream(logFile, { flags: 'a' });
      this.logStream.write(`\n\n---- STARTING MCP WRAPPER LOG AT ${new Date().toISOString()} ----\n\n`);
    } catch (error) {
      console.error(`Failed to create log file: ${error.message}`);
    }
  }

  /**
   * Internal logging function
   * @param {string} level - Log level
   * @param {string} message - Log message
   */
  log(level, message) {
    const timestamp = new Date().toISOString();
    const logMessage = `[${timestamp}] [${level}] ${message}`;
    
    // Log to stderr
    console.error(`[MCP-WRAPPER] ${logMessage}`);
    
    // Log to file if available
    if (this.logStream) {
      this.logStream.write(logMessage + '\n');
    }
  }

  /**
   * Log error message
   * @param {string} message - Error message
   */
  error(message) {
    this.log('ERROR', message);
  }

  /**
   * Log warning message
   * @param {string} message - Warning message
   */
  warn(message) {
    this.log('WARN', message);
  }

  /**
   * Log info message
   * @param {string} message - Info message
   */
  info(message) {
    this.log('INFO', message);
  }

  /**
   * Log debug message (only in debug mode)
   * @param {string} message - Debug message
   */
  debug(message) {
    if (this.debugMode) {
      this.log('DEBUG', message);
    }
  }

  /**
   * Close the log stream
   */
  close() {
    if (this.logStream) {
      this.logStream.end();
      this.logStream = null;
    }
  }
}

/**
 * Create a logger instance
 * @param {Object} config - Configuration with LOG_FILE and DEBUG_MODE
 * @returns {Logger} Logger instance
 */
function createLogger(config) {
  return new Logger(config.LOG_FILE, config.DEBUG_MODE);
}

module.exports = {
  createLogger
};