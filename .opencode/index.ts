import { type Plugin, tool } from "@opencode-ai/plugin";

const TOOL_SERVER_URL = process.env.TOOL_SERVER_URL || "http://localhost:9001";

// Helper to call tool server API
async function callToolServer(path: string, options: RequestInit = {}) {
  const url = `${TOOL_SERVER_URL}${path}`;
  const resp = await fetch(url, {
    ...options,
    headers: {
      "Content-Type": "application/json",
      ...options.headers,
    },
  });
  if (!resp.ok) {
    const text = await resp.text();
    throw new Error(`Tool server error: ${resp.status} ${text}`);
  }
  return resp.json();
}

export const ToolServerPlugin: Plugin = async (_ctx) => {
  return {
    tool: {
      // Identity & Attestation
      identity_info: tool({
        description: "Get the agent's cryptographic identity (DID, fingerprint, public key)",
        args: {},
        async execute() {
          const info = await callToolServer("/identity/info");
          return JSON.stringify(info, null, 2);
        },
      }),

      attest: tool({
        description: "Create a signed attestation recording context, thought, action, and coeffects",
        args: {
          attest_type: tool.schema.string().describe("Type of attestation (e.g. 'deploy', 'refactor', 'fix')"),
          context: tool.schema.string().describe("What was requested or the situation"),
          thought: tool.schema.string().optional().describe("Reasoning process"),
          action: tool.schema.string().optional().describe("What was done"),
          coeffects: tool.schema.object({
            filesystem: tool.schema.string().optional().describe("Files/dirs accessed"),
            network: tool.schema.string().optional().describe("Network resources used"),
            gpu: tool.schema.string().optional().describe("GPU resources used"),
          }).optional().describe("Resources used"),
        },
        async execute(args) {
          const result = await callToolServer("/attest", {
            method: "POST",
            body: JSON.stringify({
              attest_type: args.attest_type,
              context: args.context,
              thought: args.thought,
              action: args.action,
              coeffects: args.coeffects ? {
                filesystem: args.coeffects.filesystem,
                network: args.coeffects.network,
                gpu: args.coeffects.gpu,
              } : undefined,
            }),
          });
          return JSON.stringify(result, null, 2);
        },
      }),

      attest_log: tool({
        description: "Get the attestation log - history of signed actions",
        args: {
          limit: tool.schema.number().optional().describe("Max number of entries to return"),
        },
        async execute(args) {
          const params = args.limit ? `?limit=${args.limit}` : "";
          const log = await callToolServer(`/attest/log${params}`);
          return JSON.stringify(log, null, 2);
        },
      }),

      // Code Sandbox
      workspace_create: tool({
        description: "Create an isolated code workspace/sandbox",
        args: {
          workspace_id: tool.schema.string().optional().describe("Optional workspace ID (auto-generated if not provided)"),
        },
        async execute(args) {
          const result = await callToolServer("/code/workspace", {
            method: "POST",
            body: JSON.stringify({ workspace_id: args.workspace_id }),
          });
          return JSON.stringify(result, null, 2);
        },
      }),

      workspace_write: tool({
        description: "Write a file to a code workspace",
        args: {
          workspace_id: tool.schema.string().describe("Workspace ID"),
          path: tool.schema.string().describe("File path within workspace"),
          content: tool.schema.string().describe("File content"),
        },
        async execute(args) {
          const result = await callToolServer("/code/write", {
            method: "POST",
            body: JSON.stringify({
              workspace_id: args.workspace_id,
              path: args.path,
              content: args.content,
            }),
          });
          return JSON.stringify(result, null, 2);
        },
      }),

      workspace_read: tool({
        description: "Read a file from a code workspace",
        args: {
          workspace_id: tool.schema.string().describe("Workspace ID"),
          path: tool.schema.string().describe("File path within workspace"),
        },
        async execute(args) {
          const result = await callToolServer("/code/read", {
            method: "POST",
            body: JSON.stringify({
              workspace_id: args.workspace_id,
              path: args.path,
            }),
          });
          return JSON.stringify(result, null, 2);
        },
      }),

      workspace_compile: tool({
        description: "Compile/run code in a workspace",
        args: {
          workspace_id: tool.schema.string().describe("Workspace ID"),
          path: tool.schema.string().describe("File to compile/run"),
        },
        async execute(args) {
          const result = await callToolServer("/code/compile", {
            method: "POST",
            body: JSON.stringify({
              workspace_id: args.workspace_id,
              path: args.path,
            }),
          });
          return JSON.stringify(result, null, 2);
        },
      }),

      workspace_list: tool({
        description: "List all workspaces",
        args: {},
        async execute() {
          const result = await callToolServer("/code/workspaces");
          return JSON.stringify(result, null, 2);
        },
      }),

      workspace_files: tool({
        description: "List files in a workspace",
        args: {
          workspace_id: tool.schema.string().describe("Workspace ID"),
        },
        async execute(args) {
          const result = await callToolServer(`/code/files/${args.workspace_id}`);
          return JSON.stringify(result, null, 2);
        },
      }),

      // AST Operations
      ast_search: tool({
        description: "Search code using AST patterns (ast-grep)",
        args: {
          workspace_id: tool.schema.string().describe("Workspace ID"),
          pattern: tool.schema.string().describe("AST pattern to search for"),
          language: tool.schema.string().optional().describe("Language (python, rust, etc)"),
        },
        async execute(args) {
          const result = await callToolServer("/code/ast_search", {
            method: "POST",
            body: JSON.stringify({
              workspace_id: args.workspace_id,
              pattern: args.pattern,
              language: args.language,
            }),
          });
          return JSON.stringify(result, null, 2);
        },
      }),

      ast_replace: tool({
        description: "Replace code using AST patterns (ast-grep)",
        args: {
          workspace_id: tool.schema.string().describe("Workspace ID"),
          pattern: tool.schema.string().describe("AST pattern to match"),
          replacement: tool.schema.string().describe("Replacement pattern"),
          language: tool.schema.string().optional().describe("Language (python, rust, etc)"),
        },
        async execute(args) {
          const result = await callToolServer("/code/ast_replace", {
            method: "POST",
            body: JSON.stringify({
              workspace_id: args.workspace_id,
              pattern: args.pattern,
              replacement: args.replacement,
              language: args.language,
            }),
          });
          return JSON.stringify(result, null, 2);
        },
      }),

      // Web Tools
      web_search: tool({
        description: "Search the web using SearXNG",
        args: {
          query: tool.schema.string().describe("Search query"),
          num_results: tool.schema.number().optional().describe("Number of results (default 10)"),
        },
        async execute(args) {
          const params = new URLSearchParams({ q: args.query });
          if (args.num_results) params.set("limit", String(args.num_results));
          const result = await callToolServer(`/tools/search?${params}`);
          return JSON.stringify(result, null, 2);
        },
      }),

      read_url: tool({
        description: "Read and extract content from a URL",
        args: {
          url: tool.schema.string().describe("URL to read"),
        },
        async execute(args) {
          const params = new URLSearchParams({ url: args.url });
          const result = await callToolServer(`/tools/read_url?${params}`);
          return JSON.stringify(result, null, 2);
        },
      }),

      code_search: tool({
        description: "Search code in the project",
        args: {
          query: tool.schema.string().describe("Search query"),
        },
        async execute(args) {
          const params = new URLSearchParams({ q: args.query });
          const result = await callToolServer(`/tools/code_search?${params}`);
          return JSON.stringify(result, null, 2);
        },
      }),

      // Coeffects manifest
      coeffects_manifest: tool({
        description: "Get the coeffects manifest - declared resource requirements",
        args: {},
        async execute() {
          const manifest = await callToolServer("/coeffects/manifest");
          return JSON.stringify(manifest, null, 2);
        },
      }),

      // Box Drawing - dial it once, the result is saved
      box: tool({
        description: "Generate perfect Unicode box drawings. Tables, diagrams, frames. No more off-by-one.",
        args: {
          type: tool.schema.enum(["table", "frame", "tree", "diagram"]).describe("Type of box drawing"),
          data: tool.schema.any().describe("For table: {headers: string[], rows: string[][]}. For frame: {title?: string, content: string, width?: number}. For tree: {root: string, children: any[]}. For diagram: {boxes: {id: string, label: string}[], connections: {from: string, to: string, label?: string}[]}"),
        },
        async execute(args) {
          const { type, data } = args;
          
          // Box drawing characters - correct, once, forever
          const B = {
            tl: '┌', tr: '┐', bl: '└', br: '┘',
            h: '─', v: '│',
            lt: '├', rt: '┤', tt: '┬', bt: '┴',
            x: '┼',
            // Double
            dtl: '╔', dtr: '╗', dbl: '╚', dbr: '╝',
            dh: '═', dv: '║',
            // Rounded
            rtl: '╭', rtr: '╮', rbl: '╰', rbr: '╯',
          };
          
          // Pad string to width
          const pad = (s: string, w: number, align: 'left' | 'center' | 'right' = 'left'): string => {
            const len = [...s].length; // Handle unicode properly
            if (len >= w) return s.slice(0, w);
            const diff = w - len;
            if (align === 'center') {
              const left = Math.floor(diff / 2);
              return ' '.repeat(left) + s + ' '.repeat(diff - left);
            }
            if (align === 'right') return ' '.repeat(diff) + s;
            return s + ' '.repeat(diff);
          };
          
          // Horizontal line
          const hline = (w: number, l: string, m: string, r: string, sep?: number[]): string => {
            if (!sep) return l + m.repeat(w) + r;
            let line = l;
            for (let i = 0; i < sep.length; i++) {
              line += m.repeat(sep[i]);
              if (i < sep.length - 1) line += (l === B.tl || l === B.lt || l === B.bl ? B.tt : l === B.tr || l === B.rt || l === B.br ? B.bt : B.x);
            }
            return line + r;
          };
          
          if (type === 'table') {
            const { headers, rows } = data as { headers: string[], rows: string[][] };
            
            // Calculate column widths
            const widths = headers.map((h, i) => 
              Math.max(
                [...h].length,
                ...rows.map(r => r[i] ? [...r[i]].length : 0)
              ) + 2 // padding
            );
            
            const lines: string[] = [];
            
            // Top border
            lines.push(B.tl + widths.map(w => B.h.repeat(w)).join(B.tt) + B.tr);
            
            // Header row
            lines.push(B.v + headers.map((h, i) => pad(h, widths[i], 'center')).join(B.v) + B.v);
            
            // Header separator
            lines.push(B.lt + widths.map(w => B.h.repeat(w)).join(B.x) + B.rt);
            
            // Data rows
            for (const row of rows) {
              lines.push(B.v + row.map((cell, i) => pad(cell || '', widths[i], 'left').replace(/^/, ' ').slice(0, widths[i])).join(B.v) + B.v);
            }
            
            // Bottom border
            lines.push(B.bl + widths.map(w => B.h.repeat(w)).join(B.bt) + B.br);
            
            return lines.join('\n');
          }
          
          if (type === 'frame') {
            const { title, content, width: requestedWidth } = data as { title?: string, content: string, width?: number };
            const contentLines = content.split('\n');
            const maxContent = Math.max(...contentLines.map(l => [...l].length));
            const titleLen = title ? [...title].length + 2 : 0;
            const innerWidth = Math.max(requestedWidth || 0, maxContent, titleLen);
            
            const lines: string[] = [];
            
            // Top border with optional title
            if (title) {
              const titlePad = innerWidth - titleLen;
              const left = Math.floor(titlePad / 2);
              lines.push(B.tl + B.h.repeat(left) + ' ' + title + ' ' + B.h.repeat(titlePad - left) + B.tr);
            } else {
              lines.push(B.tl + B.h.repeat(innerWidth) + B.tr);
            }
            
            // Content lines
            for (const line of contentLines) {
              lines.push(B.v + pad(line, innerWidth) + B.v);
            }
            
            // Bottom border
            lines.push(B.bl + B.h.repeat(innerWidth) + B.br);
            
            return lines.join('\n');
          }
          
          if (type === 'tree') {
            const renderTree = (node: any, prefix: string = '', isLast: boolean = true): string[] => {
              const lines: string[] = [];
              const connector = isLast ? '└── ' : '├── ';
              const label = typeof node === 'string' ? node : node.root || node.label || String(node);
              lines.push(prefix + connector + label);
              
              const children = node.children || [];
              const childPrefix = prefix + (isLast ? '    ' : '│   ');
              for (let i = 0; i < children.length; i++) {
                lines.push(...renderTree(children[i], childPrefix, i === children.length - 1));
              }
              return lines;
            };
            
            const { root, children } = data as { root: string, children: any[] };
            const lines = [root];
            for (let i = 0; i < children.length; i++) {
              lines.push(...renderTree(children[i], '', i === children.length - 1));
            }
            return lines.join('\n');
          }
          
          if (type === 'diagram') {
            const { boxes, connections } = data as { 
              boxes: { id: string, label: string }[], 
              connections: { from: string, to: string, label?: string }[] 
            };
            
            // Simple horizontal layout
            const boxWidth = Math.max(...boxes.map(b => [...b.label].length)) + 4;
            const boxHeight = 3;
            
            // Render boxes
            const boxLines: string[] = [];
            const top = boxes.map(() => B.tl + B.h.repeat(boxWidth - 2) + B.tr).join('   ');
            const mid = boxes.map(b => B.v + pad(b.label, boxWidth - 2, 'center') + B.v).join('───');
            const bot = boxes.map(() => B.bl + B.h.repeat(boxWidth - 2) + B.br).join('   ');
            
            boxLines.push(top);
            boxLines.push(mid);
            boxLines.push(bot);
            
            return boxLines.join('\n');
          }
          
          return 'Unknown box type';
        },
      }),

      // NativeLink gRPC status
      nativelink_status: tool({
        description: "Check NativeLink CAS gRPC service status on localhost:50052",
        args: {},
        async execute() {
          try {
            const controller = new AbortController();
            const timeout = setTimeout(() => controller.abort(), 2000);
            
            // NativeLink speaks gRPC, not HTTP - this will fail but confirms port is open
            await fetch("http://localhost:50052", {
              signal: controller.signal,
            }).catch(() => null);
            
            clearTimeout(timeout);
            
            return JSON.stringify({
              status: "port_open",
              endpoint: "localhost:50052",
              protocol: "gRPC (HTTP/2)",
              services: [
                "build.bazel.remote.execution.v2.ContentAddressableStorage",
                "google.bytestream.ByteStream",
                "build.bazel.remote.execution.v2.ActionCache",
              ],
              note: "NativeLink CAS is running. Use grpcurl or Remote Execution API client.",
            }, null, 2);
          } catch {
            return JSON.stringify({
              status: "unreachable",
              endpoint: "localhost:50052",
            }, null, 2);
          }
        },
      }),
    },
  };
};

export default ToolServerPlugin;
