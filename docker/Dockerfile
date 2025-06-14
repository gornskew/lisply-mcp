# Lisply-MCP Emacs Development Container
# Based on skewed-emacs but optimized for lisply-mcp development

FROM genworks/skewed-emacs:devo

# Switch back to root to install additional packages if needed
USER root

# Install any additional development tools for lisply-mcp
RUN apt-get update && apt-get install -y \
    nodejs \
    npm \
    jq \
    && rm -rf /var/cache/apt/* && apt-get clean

# Create a specific workspace for lisply-mcp
RUN mkdir -p /workspace/lisply-mcp && \
    chown emacs-user:emacs-user /workspace/lisply-mcp

# Switch back to emacs-user
USER emacs-user
WORKDIR /workspace/lisply-mcp

# Copy lisply-mcp project files
COPY --chown=emacs-user:emacs-user . /workspace/lisply-mcp/

# Set up environment for lisply-mcp development
ENV LISPLY_MCP_ROOT=/workspace/lisply-mcp
ENV PROJECTS_ROOT=/projects

# Create a custom startup script for lisply-mcp development
COPY --chown=emacs-user:emacs-user docker/startup-lisply-mcp.sh /home/emacs-user/startup-lisply-mcp.sh
RUN chmod +x /home/emacs-user/startup-lisply-mcp.sh

# Expose ports for both Emacs HTTP API and any Node.js services
EXPOSE 7080 3000 8080

# Use the lisply-mcp specific startup script
ENTRYPOINT ["/home/emacs-user/startup-lisply-mcp.sh"]
