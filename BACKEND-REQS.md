# Lisply Backend Requirements

This document outlines the requirements for a Lisp-based system to be
considered a "Lisply-compliant" backend for use with the MCP (Model
Context Protocol) wrapper. Compliant backends enable direct
AI-assisted symbolic programming through protocols defined in this
document.

## Core Requirements

### 1. HTTP Server Capability

A Lisply backend must provide an HTTP server that exposes the following endpoints:

- `/mcp/ping-lisp`: Simple ping endpoint for availability checks
- `/mcp/lisp-eval`: Endpoint for evaluating Lisp expressions
- `/mcp/tools/list`: Endpoint that returns a list of available tools

### 2. Lisp Evaluation Protocol

The backend must support Lisp code evaluation through the `/mcp/lisp-eval` endpoint with the following characteristics:

- **Request Format**: HTTP POST accepting JSON payload with:
  ```json
  {
    "code": "Lisp code to evaluate",
    "package": "Optional package name"
  }
  ```

- **Response Format**: JSON response with the following structure:
  ```json
  {
    "success": true,
    "result": "Result of evaluation",
    "stdout": "Any stdout output generated"
  }
  ```
  
  Or in case of error:
  ```json
  {
    "success": false,
    "error": "Error message"
  }
  ```

### 3. Tool Definitions

The backend must expose a list of its capabilities through the `/mcp/tools/list` endpoint, which returns a JSON object with the structure:

```json
{
  "tools": [
    {
      "name": "lisp_eval",
      "description": "Evaluates Lisp code directly within the Lisply environment",
      "parameters": {
        "type": "object",
        "properties": {
          "code": {
            "type": "string",
            "description": "The Lisp code to evaluate"
          },
          "package": {
            "type": "string",
            "description": "The package to use for the evaluation (optional)"
          },
          "mode": {
            "type": "string",
            "description": "The mode to use: 'http' (default) or 'stdio' for interactive REPL"
          }
        },
        "required": ["code"]
      }
    },
    {
      "name": "http_request",
      "description": "Makes HTTP requests to endpoints implemented in the Lisply environment",
      "parameters": {
        "type": "object",
        "properties": {
          "path": {
            "type": "string",
            "description": "The path of the endpoint to request"
          },
          "method": {
            "type": "string",
            "description": "The HTTP method to use (GET, POST, etc.)"
          },
          "headers": {
            "type": "object",
            "description": "Request headers to include"
          },
          "body": {
            "type": "string",
            "description": "Request body for POST/PUT requests"
          }
        },
        "required": ["path"]
      }
    },
    {
      "name": "ping_lisp",
      "description": "Checks if the Lisply backend is available",
      "parameters": {
        "type": "object",
        "properties": {}
      }
    }
  ]
}
```

## Optional Capabilities

### 1. SWANK Server Support

For enhanced integration with development environments:

- **SWANK Protocol**: Support for connecting via SWANK (Superior Lisp Interaction Mode for Emacs)
- **Default Port**: 4200 (customizable)

### 2. Interactive Debugger

For local deployments, provide interactive debugging capabilities:

- Interactive REPL via stdio mode
- Debugger interface
- Support for commands like abort, continue, etc.

### 3. HTTPS Support

For secure deployments:

- HTTPS server with valid certificates
- Default port: 9443 (customizable)

### 4. Telnet Interface

For legacy access methods:

- Telnet server for direct Lisp interaction
- Default port: 4023 (customizable)

## Containerization Support

For standardized deployment, backends should provide:

1. **Docker Container**: A Docker image containing the Lisply backend
2. **Service Configuration**: Environment variables to configure service startup:
   - START_HTTP, HTTP_PORT
   - START_HTTPS, HTTPS_PORT
   - START_SWANK, SWANK_PORT
   - START_TELNET, TELNET_PORT

3. **Volume Mounting**: Support for mounting host directories into the container

## Implementation Examples

Currently, there are implementations or planned implementations for:

1. **Gendl**: A full implementation available at [Gendl on GitLab](https://gitlab.common-lisp.net/gendl/gendl)
2. **GNU Emacs Backend**: In development at [Skewed Emacs on GitHub](https://github.com/gornskew/skewed-emacs.git)

## Extension Protocol

Backends may provide additional tools beyond the core requirements, which should be documented and exposed through the `tools/list` endpoint. Such extensions might include:

- Domain-specific knowledge bases
- Specialized visualization tools
- File manipulation capabilities
- Custom REST APIs

## Testing for Compliance

To test a backend for compliance, implement the following checks:

1. Ping test: `GET /mcp/ping-gendl`
2. Tool list retrieval: `GET /mcp/tools/list`
3. Basic Lisp evaluation: 
   ```
   POST /mcp/lisp-eval
   {"code": "(+ 1 2 3)"}
   ```
4. Package specification:
   ```
   POST /mcp/lisp-eval
   {"code": "(package-name *package*)", "package": "gdl-user"}
   ```

A successful implementation should respond correctly to all these tests.
