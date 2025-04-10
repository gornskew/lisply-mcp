# Lisply Backend Requirements

This document outlines the requirements for any Lisp-based program to
become a "Lisply-compliant" backend for use with the MCP (Model
Context Protocol) wrapper implemented by this repository. Compliant
Lisply backend Lisp environments enable direct AI-assisted symbolic
programming, through protocols defined in this document.

## Core Requirements

### 2. Some kind of Lisp

It should be a [Lisp](https://common-lisp.net/). What is a Lisp? Well
for our purposes let's say a Lisp is a program which can accept
_expressions_ in some known syntax, and can _evaluate_ the expression
to produce possibly some output and probably one or more _return
values_. So, strictly speaking, according to our rather loose
definition, we're really talking more about a
[REPL](https://common-lisp.net/project/slime/) than a
[Lisp](https://www.paulgraham.com/lisp.html) per se. But you'll have a
better time if the engine behind your
[REPL](https://lisp-lang.org/learn/repl) is indeed a
[Lisp](https://franz.com/products/allegro-common-lisp/).

### 1. HTTP Server Capability

A Lisply backend must provide an HTTP server that exposes the
following endpoints:

- `/lisply/ping-lisp`: Simple ping endpoint for availability checks
- `/lisply/lisp-eval`: Endpoint for evaluating Lisp expressions
- `/lisply/tools/list`: Endpoint that returns a list of available tools

Note: The endpoint prefix (`/lisply/`) can be configured in the MCP
wrapper, but backends should support this as the default.

### 2. Lisp Evaluation Protocol

The backend must support Lisp code evaluation through the
`/lisply/lisp-eval` endpoint with the following characteristics:

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

The backend must expose a list of its capabilities through the
`/lisply/tools/list` endpoint, which returns a JSON object with the
structure:

```json
{
  "tools": [
    {
      "name": "lisp_eval",
      "description": "Evaluates Lisp code directly within the Lisply environment",
      "inputSchema": {
        "type": "object",
        "properties": {
          "code": {
            "type": "string",
            "description": "The Lisp code to evaluate"
          },
          "package": {
            "type": "string",
            "description": "The package to use for the evaluation (optional)"
          }
        },
        "required": ["code"]
      }
    },
    {
      "name": "ping_lisp",
      "description": "Checks if the Lisply backend is available",
      "inputSchema": {
        "type": "object",
        "properties": {}
      }
    }
  ]
}
```

**Note**: The backend is only expected to implement the `lisp_eval`
and `ping_lisp` tools. The `http_request` tool and the `mode`
parameter for `lisp_eval` are handled by the MCP wrapper middleware
(mcp-wrapper.js) and should not be implemented or documented by the
backend. The backend is not aware of these features, as they are an
abstraction provided by the middleware.

## Optional Capabilities

### 1. SWANK Server Support

For enhanced integration with development environments:

- **SWANK Protocol**: Support for connecting via SWANK (Superior Lisp
  Interaction Mode for Emacs) for Lisply backends which support that
  (e.g. Common Lisp based backends)
- **Default Port**: 4200 (internal to container) / 4201 (visible on
  docker host)

### 2. Interactive Debugger

For local deployments, provide interactive debugging capabilities:

- Interactive REPL via stdio mode
- Debugger interface with the ability to:
  - Display debug information for errors
  - Present multiple restart options
  - Support for common debugging commands (abort, continue, backtrace, etc.)
  - Return to normal evaluation upon command
- Proper handling of debugger prompt detection (implementation-specific)

### 3. Communication Modes

The middleware supports two evaluation modes with the Lisply
backend:

- **HTTP Mode**: Structured communication with separate result and
  stdout fields. The backend returns a JSON response with success,
  result, and stdout fields. This is the standard mode of
  communication and the only one the backend needs to implement
  directly.

- **Stdio Mode**: To support this (optional) mode, the backend must
  present a Lisp REPL (read-eval-print-loop) as the foreground process
  attached to its standard input/output. The communication for this
  mode is managed by the middleware (mcp-wrapper.js), which expects a
  native REPL interface on the standard input/output streams of the
  local backend container. Implementing/configuring this standard I/O
  behavior may be easier on some backends (e.g. Common Lisp, where a
  repl on standard I/O is typically the default anyway, vs. Emacs
  Lisp, where some tricks may be necessary to get something like
  [IELM](https://www.emacswiki.org/emacs/InferiorEmacsLispMode) to
  show up on standard I/O.


### 4. HTTPS Support

For secure deployments:

- HTTPS server with valid certificates
- Default port: 9443 (internal to container) / 9444 (visible on docker host)

### 5. Telnet Interface

For legacy access methods:

- Telnet server for direct Lisp interaction
- Default port: 4023 (internal to container) / 4024 (visible on docker host)

## Containerization Support (optional in principle, but required for local operation of lisply-mcp)

For standardized deployment, backends should support:

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
        with Lisply implementation [here] (https://gitlab.common-lisp.net/gendl/gendl/gwl/lisply-backend)
2. **GNU Emacs Backend**: In development at [Skewed Emacs on GitHub](https://github.com/gornskew/skewed-emacs.git)


## Testing for Compliance

To test a backend for compliance, implement the following checks:

1. Ping test: `GET /lisply/ping-lisp`
2. Tool list retrieval: `GET /lisply/tools/list`
3. Basic Lisp evaluation: 
   ```
   POST /lisply/lisp-eval
   {"code": "(+ 1 2 3)"}
   ```
4. Package specification (for backends that support package-based namespaces):
   ```
   POST /lisply/lisp-eval
   {"code": "(package-name *package*)", "package": "gdl-user"}
   ```

Note: Testing of the `mode` parameter should be done at the middleware
level, not directly with the backend, as this feature is handled by
the MCP wrapper and is expected to be ignored by the backend.

A successful implementation should respond correctly to all these tests.
