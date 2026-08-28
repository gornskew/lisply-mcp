# MCP Wrapper Regression Tests

## Overview

`harness.js` is a real, fast regression harness for the cyborg-whisperer
wrapper. It spawns the wrapper as a child process, speaks MCP JSON-RPC
to it over stdio (exactly as an MCP client would), and drives:
initialize -> tools/list -> tools/call for `ping_lisp`, `lisp_eval`
(success, list, and error cases), and `http_request`. It prints
normalized JSON to stdout, suitable for capturing a golden baseline
before a change and diffing after.

## Usage

Against a known-running Lisply backend, e.g.:

```bash
node regression-tests/harness.js --backend-host bridge --http-port 9080
node regression-tests/harness.js --backend-host ready-room --http-port 7080
node regression-tests/harness.js --backend-host 127.0.0.1 --http-host-port 7080  # loopback/host path
```

Capture and diff a golden baseline:

```bash
node regression-tests/harness.js --backend-host gendl-ccl --http-port 9080 > /tmp/golden.json
# ... make changes ...
node regression-tests/harness.js --backend-host gendl-ccl --http-port 9080 > /tmp/after.json
diff /tmp/golden.json /tmp/after.json
```

Caution: when driving the harness from Emacs Lisp inside the same
Emacs that serves the backend, run it as an async background process —
a synchronous shell call blocks the Emacs event loop and deadlocks
against its own HTTP server.

## Golden Patterns

- Ping: `pong`
- Arithmetic: `Result: 6, Stdout: ` for `(+ 1 2 3)`
- Lists: `Result: (1 2 3), Stdout: ` for `(list 1 2 3)`
- Errors: MCP error `-32603`

## History

Earlier python stub suites (`mcp_regression_tests.py`,
`mcp_live_tests.py`) hardcoded expected values and never spawned the
wrapper; they were removed (the core stack eschews python). Their
discovered golden patterns live on above.
