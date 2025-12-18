# Lisply-MCP Container

This container provides a minimal Node.js runtime for the MCP (Model Context Protocol) wrapper middleware.

## Purpose

Lisply-MCP is a neutral middleware that bridges AI coding assistants (Claude Code, Gemini CLI, etc.) 
to Lisp backends (Gendl, Emacs) via the Model Context Protocol.

**Note:** AI CLI tools (Claude Code, Gemini CLI, Codex) have been moved to the skewed-emacs container.
This container now only provides the MCP wrapper functionality.

## Available Services

### MCP Wrapper (`/app/scripts/mcp-wrapper.js`)

The MCP wrapper provides:
- Protocol bridging between MCP clients and Lisp HTTP backends
- Support for multiple Lisp implementations (Gendl CCL, Gendl SBCL, Skewed Emacs)
- Configurable via command-line arguments

**Usage:**
```bash
node /app/scripts/mcp-wrapper.js --help
```

## Configuration

MCP configuration is managed by the orchestrating context (skewed-emacs):
- Base config: `/projects/skewed-emacs/docker/mcp-config.json`
- Private config: `/projects/skewed-emacs/private-mcp-config.json` (gitignored)
- Merged config generated at: `/tmp/merged-mcp-config.json`

## Container Management

When used with docker-compose via skewed-emacs:
```bash
cd /projects/skewed-emacs
./compose-dev up      # Start all services including lisply-mcp
./compose-dev down    # Stop all services
```

## File Access

The container has access to project files via bind mount:
- Host: `~/projects/`
- Container: `/projects/`

The MCP wrapper scripts at `/projects/lisply-mcp/scripts/` are accessible from all containers.
