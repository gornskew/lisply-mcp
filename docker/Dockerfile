# Node.js service container for MCP, Copilot, and Claude Code
FROM node:20-slim

# Set working directory inside the container
WORKDIR /app

# Install system dependencies including Docker CLI
RUN apt-get update && apt-get install -y \
    git \
    curl \
    procps \
    socat \
    && rm -rf /var/lib/apt/lists/*

RUN curl -fsSL https://download.docker.com/linux/static/stable/x86_64/docker-24.0.7.tgz | tar xz --strip-components=1 -C /usr/local/bin docker/docker \
    && chmod +x /usr/local/bin/docker

# Add node user to docker group for Docker socket access
# don't need this because of mounted socket somehow?
# RUN usermod -aG docker node

# Maybe need this later: Fix git safe directory globally
# RUN git config --global --add safe.directory '*'

# Copy package.json and package-lock.json if they exist
COPY package*.json ./

# Install Node dependencies including language servers and tools
RUN npm install -g \
    @anthropic-ai/claude-code \
    @github/copilot-language-server

# Install any local dependencies
RUN npm install || true

# Copy the entire project (scripts, config, etc.)
COPY . .

# Make the wrapper script executable
RUN chmod +x ./scripts/mcp-wrapper.js

# Run mcp-wrapper.js --help once, to fetch dependencies.
RUN ./scripts/mcp-wrapper.js --help


# Create entrypoint script
COPY <<EOF /usr/local/bin/entrypoint.sh
#!/bin/bash
set -e

echo "Starting container setup..."

echo "Container ready. Programs ready to be exec'ed:"
echo "- lisply-mcp/scripts/mcp-wrapper.js (Lisply-MCP)"
echo "- claude (Claude Code) "
echo "- copilot-language-server"

echo "Entering sleep loop..."

tail -f /dev/null

EOF

RUN chmod +x /usr/local/bin/entrypoint.sh

# Expose ports for language servers and other services
EXPOSE 3000 8080 9001

USER node

# Keep the container running in background - simple sleep loop
# CMD ["tail", "-f", "/dev/null"]

# Set the entrypoint
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
