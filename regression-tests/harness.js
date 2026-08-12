#!/usr/bin/env node
/*
 * Copyright (C) 2026 Gornskew Enterprises
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.  Distributed WITHOUT
 * ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.
 */

// Real stdio JSON-RPC regression harness for the lisply-mcp wrapper.
// Spawns the wrapper, drives: initialize -> tools/list -> tools/call
// (ping_lisp, lisp_eval x3 incl. error case, http_request), and emits
// normalized JSON to stdout for golden-baseline capture/diffing.
//
// Usage: node harness.js --backend-host <host> --http-port <port> [wrapper args...]
// Golden expectations (from regression-tests/README.md):
//   ping -> "pong"
//   (+ 1 2 3) -> "Result: 6, Stdout: "
//   (list 1 2 3) -> "Result: (1 2 3), Stdout: "
//   errors -> MCP error -32603

const { spawn } = require('child_process');
const path = require('path');

const wrapperPath = path.join(__dirname, '..', 'scripts', 'mcp-wrapper.js');
const child = spawn('node', [wrapperPath, ...process.argv.slice(2)],
                    { stdio: ['pipe', 'pipe', 'pipe'] });

let buf = '';
const pending = new Map();
let nextId = 1;

child.stdout.on('data', (d) => {
  buf += d.toString();
  let idx;
  while ((idx = buf.indexOf('\n')) >= 0) {
    const line = buf.slice(0, idx).trim();
    buf = buf.slice(idx + 1);
    if (!line) continue;
    let msg;
    try { msg = JSON.parse(line); } catch (e) { continue; }
    if (msg.id !== undefined && pending.has(msg.id)) {
      const cb = pending.get(msg.id);
      pending.delete(msg.id);
      cb(msg);
    }
  }
});
child.stderr.on('data', () => {}); // wrapper logging; ignore

function rpc(method, params, timeoutMs = 30000) {
  return new Promise((resolve, reject) => {
    const id = nextId++;
    const t = setTimeout(() => {
      pending.delete(id);
      reject(new Error('timeout waiting for ' + method));
    }, timeoutMs);
    pending.set(id, (msg) => { clearTimeout(t); resolve(msg); });
    child.stdin.write(JSON.stringify({ jsonrpc: '2.0', id, method, params }) + '\n');
  });
}

(async () => {
  const out = {};
  out.initialize = await rpc('initialize', {
    protocolVersion: '2024-11-05', capabilities: {},
    clientInfo: { name: 'lisply-harness', version: '0.1' }
  });
  child.stdin.write(JSON.stringify({ jsonrpc: '2.0', method: 'notifications/initialized' }) + '\n');

  out.toolsList = await rpc('tools/list', {});
  const tools = out.toolsList.result.tools;
  const names = tools.map((t) => t.name);
  const pick = (suffix) => names.find((n) => n === suffix || n.endsWith(suffix));
  const call = (name, args) => rpc('tools/call', { name, arguments: args });

  out.ping = await call(pick('ping_lisp'), {});
  out.evalSum = await call(pick('lisp_eval'), { code: '(+ 1 2 3)' });
  out.evalList = await call(pick('lisp_eval'), { code: '(list 1 2 3)' });
  out.evalError = await call(pick('lisp_eval'), { code: '(error "deliberate-harness-error")' });
  out.httpRequest = await call(pick('http_request'), { path: '/lisply/ping-lisp', method: 'GET' });

  const lispEvalTool = tools.find((t) => t.name === pick('lisp_eval'));
  out.summary = {
    tools: names.slice().sort(),
    lisp_eval_params: Object.keys((lispEvalTool.inputSchema || {}).properties || {}).sort()
  };

  console.log(JSON.stringify(out, null, 2));
  child.kill();
  process.exit(0);
})().catch((e) => {
  console.error('HARNESS FAIL: ' + e.message);
  child.kill();
  process.exit(1);
});
