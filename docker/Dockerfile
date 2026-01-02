# Lisply-MCP: Minimal Node.js container for MCP wrapper middleware
# This container provides the MCP protocol bridge to Lisp backends (Gendl, Emacs)
# It is designed to be a neutral middleware - configuration comes from the orchestrating context
FROM node:23-slim

# Set working directory inside the container
WORKDIR /app

# Install minimal system dependencies
RUN apt-get update && apt-get install -y \
    curl \
    procps \
    && rm -rf /var/lib/apt/lists/*

# Copy package.json and package-lock.json if they exist
COPY package*.json ./

# Install any local dependencies
RUN if [ -f package.json ]; then npm install; else echo "No package.json found, skipping npm install"; fi

# Switch to node user for projects link
USER node
RUN ln -s /projects /home/node/projects

# Switch back to root for remaining setup
USER root

# Copy the entire project (scripts, lib, handlers, etc.)
COPY . .

# Make the wrapper script executable
RUN chmod +x ./scripts/mcp-wrapper.js

# Run mcp-wrapper.js --help once, to fetch dependencies
RUN ./scripts/mcp-wrapper.js --help

# Simple entrypoint - just keep container running for MCP access
# The actual MCP servers are spawned by Claude Code/Gemini/etc as needed
COPY ./docker/entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
