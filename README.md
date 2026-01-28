# triton-tensorrt-llm

TensorRT-LLM inference on NVIDIA Triton, with Servant-based tool server.

## Components

| Package | Description |
|---------|-------------|
| `tool-server` | **Servant API with OpenAPI3** — code sandbox, attestation, tools |
| `openai-proxy` | Haskell AI Gateway — OpenAI-compatible chat completions |
| `trtllm-validate` | Engine validation CLI |
| `trtllm-engine` | TRT-LLM engine building infrastructure |

## Quick Start

```bash
# Run the Tool Server (Servant + OpenAPI3)
nix run github:straylight-software/triton-tensorrt-llm#tool-server

# Run the OpenAI Proxy
nix run github:straylight-software/triton-tensorrt-llm#openai-proxy

# Build an engine (requires GPU)
nix build .#qwen3-32b-engine --option sandbox false

# Run validation
nix run .#trtllm-validate -- detect-quant /path/to/model
```

---

## Tool Server

The `tool-server` provides a Servant-based API with automatic OpenAPI 3.0 spec generation.

### Features

- **Code Sandbox**: Isolated workspaces for code editing and compilation
- **AST Operations**: Pattern search/replace via [ast-grep](https://ast-grep.github.io/)
- **Compilation**: Type-check Rust, Haskell, Lean, Dhall, PureScript
- **Attestation**: Ed25519 identity with signed git commits
- **Tools**: Web search (SearXNG), URL reading (Jina)

### Endpoints

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/health` | Health check |
| GET | `/openapi.json` | OpenAPI 3.0 specification |
| | | |
| **Code Sandbox** | | |
| POST | `/code/workspace` | Create isolated workspace |
| GET | `/code/workspace/:id` | Get workspace info |
| GET | `/code/workspaces` | List all workspaces |
| POST | `/code/write` | Write file to workspace |
| POST | `/code/read` | Read file from workspace |
| GET | `/code/files/:workspace_id` | List files in workspace |
| POST | `/code/ast_search` | AST pattern search |
| POST | `/code/ast_replace` | AST pattern replace |
| POST | `/code/compile` | Compile/type-check file |
| | | |
| **Identity & Attestation** | | |
| GET | `/identity/info` | Get agent identity (fingerprint, DID) |
| POST | `/attest` | Create signed attestation |
| GET | `/attest/log` | Get attestation chain |
| GET | `/attest/:commit/verify` | Verify attestation signature |
| | | |
| **Tools** | | |
| GET | `/tools/search` | Web search via SearXNG |
| GET | `/tools/code_search` | Code-focused web search |
| GET | `/tools/read_url` | Read URL content via Jina |
| | | |
| **Coeffects** | | |
| GET | `/coeffects/manifest` | Resource requirements per endpoint |

### Example Usage

```bash
# Start server
TOOL_SERVER_PORT=9001 nix run .#tool-server

# Get OpenAPI spec
curl http://localhost:9001/openapi.json | jq .

# Create workspace
curl -X POST http://localhost:9001/code/workspace \
  -H "Content-Type: application/json" \
  -d '{"workspace_id": "my-project"}'

# Write a file
curl -X POST http://localhost:9001/code/write \
  -H "Content-Type: application/json" \
  -d '{
    "workspace_id": "my-project",
    "path": "src/Main.hs",
    "content": "module Main where\n\nmain :: IO ()\nmain = putStrLn \"Hello\""
  }'

# Compile (type-check)
curl -X POST http://localhost:9001/code/compile \
  -H "Content-Type: application/json" \
  -d '{"workspace_id": "my-project", "path": "src/Main.hs"}'

# AST search
curl -X POST http://localhost:9001/code/ast_search \
  -H "Content-Type: application/json" \
  -d '{
    "workspace_id": "my-project",
    "pattern": "putStrLn $_",
    "language": "haskell"
  }'

# Create attestation (requires identity configured)
curl -X POST http://localhost:9001/attest \
  -H "Content-Type: application/json" \
  -d '{
    "attest_type": "task",
    "context": "Implemented feature X",
    "thought": "Considered approaches A and B, chose A for simplicity",
    "action": "Created 3 files, modified 2",
    "coeffects": {"filesystem": "readWrite", "network": "read"}
  }'
```

### Supported Languages

The "Language Coset" for compilation:

| Language | Extension | Compiler | Check Mode |
|----------|-----------|----------|------------|
| Rust | `.rs` | `rustc` | `--emit=metadata` |
| Haskell | `.hs`, `.lhs` | `ghc` | `-fno-code` |
| Lean | `.lean` | `lean` | `--run` |
| Dhall | `.dhall` | `dhall` | `type` |
| PureScript | `.purs` | `purs` | `compile` |

### Architecture

```
lib/
├── CodeSandbox.hs    # Pure domain logic (reusable)
└── Attestation.hs    # Identity & signing (reusable)

tool-server/
├── API.hs            # Servant API type definition
├── API/Types.hs      # Request/Response with OpenAPI3 schemas
└── Server.hs         # Thin handlers over lib/
```

---

## OpenAI Proxy

The `openai-proxy` provides an OpenAI-compatible interface to TensorRT-LLM.

### Endpoints

| Endpoint | Description |
|----------|-------------|
| `POST /v1/chat/completions` | Chat completions (streaming & non-streaming) |
| `GET /v1/models` | List available models |
| `GET /health` | Health check |
| `GET /tools/search` | Web search |
| `GET /tools/read_url` | URL reader |

### Configuration

| Environment Variable | Default | Description |
|---------------------|---------|-------------|
| `OPENAI_PROXY_PORT` | `9000` | Server port |
| `TRITON_URL` | `http://localhost:8000` | Triton backend URL |
| `MODEL_NAME` | `qwen3` | Model name for API |
| `SEARXNG_URL` | — | SearXNG instance URL |
| `STRIP_THINKING` | `true` | Strip `<think>` blocks |
| `METRICS_ENABLED` | `false` | Enable ClickHouse metrics |

---

## NixOS Module

The module is **self-contained** — it pulls in all dependencies including:
- Triton server with TRT-LLM backend
- OpenAI proxy (Haskell/Warp)
- Tool server (Servant + OpenAPI3)
- SearXNG (native, no Docker required)

### Usage

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    triton-tensorrt-llm.url = "github:straylight-software/triton-tensorrt-llm";
  };

  outputs = { self, nixpkgs, triton-tensorrt-llm, ... }: {
    nixosConfigurations.myhost = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        # Apply the overlay to get packages
        { nixpkgs.overlays = [ triton-tensorrt-llm.overlays.default ]; }
        
        # Import the NixOS module
        triton-tensorrt-llm.nixosModules.default
        
        # Configure the service
        {
          services.triton-trtllm = {
            enable = true;
            model = "qwen3";
            enginePath = /var/lib/trtllm/engines/qwen3;
            
            triton = {
              enable = true;
              httpPort = 8000;
            };
            
            openaiProxy = {
              enable = true;
              port = 9000;
            };
            
            toolServer = {
              enable = true;
              port = 9001;
            };
            
            searxng.enable = true;  # Native SearXNG, no Docker
          };
        }
      ];
    };
  };
}
```

### Module Options

| Option | Default | Description |
|--------|---------|-------------|
| `enable` | `false` | Enable the service |
| `model` | `"qwen3"` | Model name for service identification |
| `enginePath` | — | Path to TRT-LLM engine directory |
| `triton.enable` | `true` | Enable Triton server |
| `triton.httpPort` | `8000` | Triton HTTP port |
| `openaiProxy.enable` | `true` | Enable OpenAI proxy |
| `openaiProxy.port` | `9000` | OpenAI proxy port |
| `toolServer.enable` | `false` | Enable tool server |
| `toolServer.port` | `9001` | Tool server port |
| `searxng.enable` | `false` | Enable SearXNG (native) |
| `searxng.port` | `8888` | SearXNG port |

### Package Overrides

You can override packages if needed:

```nix
services.triton-trtllm = {
  enable = true;
  package.openaiProxy = myCustomProxy;
  package.toolServer = myCustomToolServer;
  # ...
};
```

---

## Development

```bash
# Enter dev shell
nix develop

# Run tool-server tests
cd tool-server && cabal test

# Run openai-proxy property tests
nix build .#openai-proxy.tests.proxy-proptest

# Check flake
nix flake check
```

---

## Architecture

```
┌─────────────────┐     ┌──────────────────┐     ┌─────────────────┐
│   AI Agent /    │────▶│   tool-server    │────▶│   Compilers     │
│   Open WebUI    │     │   (port 9001)    │     │   ast-grep      │
└─────────────────┘     └──────────────────┘     └─────────────────┘
        │                        │
        │               ┌────────┴────────┐
        │               ▼                 ▼
        │         ┌──────────┐     ┌────────────┐
        │         │ Identity │     │ Attestation│
        │         │ Ed25519  │     │ Git Repo   │
        │         └──────────┘     └────────────┘
        │
        ▼
┌──────────────────┐     ┌─────────────────┐
│   openai-proxy   │────▶│  Triton Server  │
│   (port 9000)    │     │   (port 8000)   │
└────────┬─────────┘     └─────────────────┘
         │
┌────────┼────────┐
▼        ▼        ▼
┌─────────┐ ┌──────────┐ ┌─────────┐
│ SearXNG │ │   Jina   │ │ Metrics │
│ (8888)  │ │ (remote) │ │ (CH)    │
└─────────┘ └──────────┘ └─────────┘
```

---

## Coeffects

The tool-server tracks resource requirements (coeffects) per endpoint:

```json
{
  "tools": {
    "code/compile": {"filesystem": "readWrite"},
    "code/ast_search": {"filesystem": "read"},
    "code/ast_replace": {"filesystem": "readWrite"},
    "attest": {"filesystem": "readWrite", "crypto": "sign"},
    "tools/search": {"network": "read"},
    "tools/read_url": {"network": "read"}
  },
  "algebra": "Coeffect semiring: (R, join, 0, meet, 1)"
}
```

This supports the Aleph coeffect system for provenance tracking.

---

## License

MIT
