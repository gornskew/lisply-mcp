# Node.js service container for MCP, Copilot, and Claude Code
FROM node:20-slim

# Set working directory inside the container
WORKDIR /app

# Install system dependencies including Docker CLI
RUN apt-get update && apt-get install -y \
    git \
    curl \
    socat \
    procps \
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

# Install global tools that don't need user updates
RUN npm install -g \
    @github/copilot-language-server

# Install any local dependencies
RUN npm install || true

# Switch to node user to install claude locally
USER node

# Install claude locally for the node user (simulating claude migrate-installer)
RUN mkdir -p /home/node/.claude/local && \
    npm install @anthropic-ai/claude-code --prefix /home/node/.claude/local && \
    ln -sf /home/node/.claude/local/node_modules/.bin/claude /home/node/.claude/local/claude

# Add claude alias to .bashrc
RUN echo 'alias claude="/home/node/.claude/local/claude"' >> /home/node/.bashrc

# Switch back to root for remaining setup
USER root

# Copy the entire project (scripts, config, etc.)
COPY . .

# Make the wrapper script executable
RUN chmod +x ./scripts/mcp-wrapper.js

# Run mcp-wrapper.js --help once, to fetch dependencies.
RUN ./scripts/mcp-wrapper.js --help


# Create entrypoint script
COPY <<EOF /usr/local/bin/entrypoint.sh
#!/bin/bash

cleanup() {
    echo "Received SIGTERM, shutting down..."
    exit 0
}

# Trap SIGTERM and call cleanup
trap cleanup SIGTERM

set -e

echo "Starting container setup..."

# Source .bashrc to get claude alias for node user
export HOME=/home/node
source /home/node/.bashrc

echo "Container ready. Programs ready to be exec'ed:"
echo "- lisply-mcp/scripts/mcp-wrapper.js (Lisply-MCP)"
echo "- claude (Claude Code) - local installation at /home/node/.claude/local/claude"
echo "- copilot-language-server"

echo "Entering sleep loop..."

# Interactive loop to keep container running and respect SIGTERM
while true; do
    echo -n "node> "
    if ! read -r line; then
        echo ""
        echo "stdin closed - shutting down container"
        break
    fi

    # Skip empty lines
    if [[ -z "$line" ]]; then
        continue
    fi

    # Handle special commands
    case "$line" in
        ".help")
            echo "Available commands:"
            echo "  .help    - Show this help"
            echo "  .quit    - Exit container"
            continue
            ;;
        ".quit" | "quit" | "exit")
            echo "Goodbye!"
            break
            ;;
        *)
            echo "Unknown command: $line"
            echo "Try: .help for available commands"
            continue
            ;;
    esac
done

cleanup

EOF

RUN chmod +x /usr/local/bin/entrypoint.sh

# Expose ports for language servers and other services
EXPOSE 3000 8080 9001

USER node

# Keep the container running in background - simple sleep loop
# CMD ["tail", "-f", "/dev/null"]

# Set the entrypoint
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
