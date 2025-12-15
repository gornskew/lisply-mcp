# Node.js service container for MCP, Copilot, and Claude Code
FROM node:23-slim

ARG DOCKER_CLI_VERSION=29.0.2

# Set working directory inside the container
WORKDIR /app

# Install system dependencies including Docker CLI and Python
#RUN apt-get update && apt-get install -y \
#    git \
#    curl \
#    socat \
#    procps \
#    python3 \
#    python3-pip \
#    python3-venv \
#    && rm -rf /var/lib/apt/lists/*


# Install curl
RUN apt-get update && apt-get install -y \
    curl \
    && rm -rf /var/lib/apt/lists/*


RUN curl -fsSL https://download.docker.com/linux/static/stable/x86_64/docker-${DOCKER_CLI_VERSION}.tgz | tar xz --strip-components=1 -C /usr/local/bin docker/docker \
    && chmod +x /usr/local/bin/docker

# Copy package.json and package-lock.json if they exist
COPY package*.json ./

# Install global tools that don't need user updates
RUN npm install -g \
    @github/copilot-language-server

# Install any local dependencies
RUN if [ -f package.json ]; then npm install; else echo "No package.json found, skipping npm install"; fi

# Switch to node user to install claude locally
USER node

# Install claude locally for the node user (simulating claude migrate-installer)
RUN mkdir -p /home/node/.claude/local && \
    npm install @anthropic-ai/claude-code --prefix /home/node/.claude/local && \
    ln -sf /home/node/.claude/local/node_modules/.bin/claude /home/node/.claude/local/claude && \
    ln -s /projects /home/node/projects

# Add claude alias to .bashrc
RUN echo 'alias claude="/home/node/.claude/local/claude"' >> /home/node/.bashrc


# Install Gemini Code locally for the node user
RUN mkdir -p /home/node/.gemini/local && \
    npm install @google/gemini-cli --prefix /home/node/.gemini/local && \
    ln -sf /home/node/.gemini/local/node_modules/.bin/gemini /home/node/.gemini/local/gemini && \
    echo 'alias gemini="/home/node/.gemini/local/gemini"' >> /home/node/.bashrc

# Install OpenAI Codex CLI locally for the node user
RUN mkdir -p /home/node/.codex/local && \
    npm install @openai/codex --prefix /home/node/.codex/local && \
    ln -sf /home/node/.codex/local/node_modules/.bin/codex /home/node/.codex/local/codex && \
    echo 'alias codex="/home/node/.codex/local/codex"' >> /home/node/.bashrc



# Switch back to root for remaining setup
USER root

# Copy the entire project (scripts, config, etc.)
COPY . .

# Make the wrapper script executable
RUN chmod +x ./scripts/mcp-wrapper.js

# Run mcp-wrapper.js --help once, to fetch dependencies.
RUN ./scripts/mcp-wrapper.js --help

# Create entrypoint script
COPY ./docker/entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

# Expose ports for language servers and other services
# FLAG -- maybe don't need these. 
EXPOSE 3000 8080 9001

USER node

# Keep the container running in background - simple sleep loop
# CMD ["tail", "-f", "/dev/null"]

# Set the entrypoint
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
