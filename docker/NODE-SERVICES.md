# Lisply-MCP Node Services Container

This container provides a shared Node.js runtime environment for
multiple services:

## Available Services

### 1. MCP Wrapper (`mcp-wrapper.js`)
- Used by Claude Desktop and other MCP servers
- Accessed via `docker exec` through `mcp-exec` wrapper
- `docker/mcp-wrap` automatically starts container when needed

### 2. Copilot Language Server
- GitHub Copilot language server for code completion
- Can be used by Emacs or other editors

**Start via port (recommended for Emacs):**
```bash
./manage-container copilot
# Starts on port 9001
```

**Start via exec:**
```bash
docker exec -d lisply-mcp copilot-language-server --stdio --port 9001
```

**Use from Emacs:** 
```elisp
;; Connect to Copilot Language Server on localhost:9001
(setq copilot-server-host "lisply-mcp"
      copilot-server-port 9001)
```

### 3. Claude Code
- Anthropic's command-line coding assistant
- Used for AI-assisted development tasks

**Use via management script:**
```bash
./manage-container claude-code --help
./manage-container claude-code "create a Python web scraper"
```

**Use via direct exec:**
```bash
docker exec -it lisply-mcp claude-code "help me debug this function"
```

## Container Management

### Start/Stop
```bash
./manage-container start    # Start container with all ports exposed
./manage-container stop     # Stop container
./manage-container status   # Check if running
```

### Port Mappings
- `3000`: General Node.js applications
- `7080-7081`: MCP wrapper services (Emacs/Gendl specific)
- `8080`: Additional Node.js services
- `9001`: Copilot Language Server

### Emacs Integration Examples

**1. Use Copilot from Emacs:**
```elisp
;; Ensure node-services container is running
(shell-command "docker exec lisply-mcp echo 'container ready' || /path/to/docker/manage-container start")

;; Start Copilot Language Server if not already running
(shell-command "docker exec -d lisply-mcp copilot-language-server --stdio --port 9001")

;; Configure Emacs to use Copilot on localhost:9001
```

**2. Use Claude Code from Emacs:**
```elisp
(defun emacs-ask-claude-code (prompt)
  "Ask Claude Code a question from Emacs"
  (interactive "sAsk Claude Code: ")
  (let ((result (shell-command-to-string 
                 (format "docker exec -i node-services claude-code '%s'" prompt))))
    (message result)))
```

**3. Run arbitrary Node.js tools:**
```bash
# From Emacs or shell
docker exec -i lisply-mcp node -e "console.log('Hello from Node!')"
docker exec -i lisply-mcp npm --version
```

## File Access
The container has access to your project files via bind mount, if run with standard manage-container script:

- Host: `~/projects/` 
- Container: `/projects/`

All Node.js services can read/write project files through this mount.
