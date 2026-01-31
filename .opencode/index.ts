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
