# MCP Server Regression Test Suite

## Overview
Comprehensive regression tests for Gendl and Skewed Emacs MCP servers.

## Files
- `mcp_regression_tests.py` - Template regression test suite
- `mcp_live_tests.py` - Live test runner with discovered patterns

## Discovered Test Patterns
From live testing:
- Ping: Both servers return 'pong'
- Arithmetic: Both return 'Result: 6, Stdout: ' for (+ 1 2 3)
- Lists: Both return 'Result: (1 2 3), Stdout: ' for (list 1 2 3)
- Errors: Both return MCP error -32603 with different messages

## Usage
Run the tests when ready to validate MCP server functionality.
