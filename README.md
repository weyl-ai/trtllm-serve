# trtllm-serve

TensorRT-LLM inference stack for NixOS.

## Components

| Package | Description |
|---------|-------------|
| `openai-proxy` | Haskell AI Gateway - OpenAI-compatible API with tool support |
| `trtllm-validate` | Engine validation CLI |
| `trtllm-engine` | Engine building infrastructure |

## Quick Start

```bash
# Run the AI Gateway
nix run github:weyl-ai/trtllm-serve

# Build an engine (requires GPU)
nix build .#qwen3-32b-engine --option sandbox false

# Run validation
nix run .#trtllm-validate -- detect-quant /path/to/model
```

## NixOS Module

```nix
{
  inputs.trtllm-serve.url = "github:weyl-ai/trtllm-serve";

  outputs = { self, nixpkgs, trtllm-serve, ... }: {
    nixosConfigurations.myhost = nixpkgs.lib.nixosSystem {
      modules = [
        trtllm-serve.nixosModules.default
        {
          services.trtllm-serve = {
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
            
            searxng.enable = true;  # Optional web search
          };
        }
      ];
    };
  };
}
```

## Endpoints

When running, the following endpoints are available:

| Endpoint | Description |
|----------|-------------|
| `http://localhost:9000/v1/chat/completions` | OpenAI chat API |
| `http://localhost:9000/v1/models` | List models |
| `http://localhost:9000/health` | Health check |
| `http://localhost:9000/tools/search?query=...` | Web search (requires SearXNG) |
| `http://localhost:9000/tools/read_url?url=...` | URL reader |
| `http://localhost:8000/v2/health/ready` | Triton health |
| `http://localhost:8002/metrics` | Prometheus metrics |

## Development

```bash
# Enter dev shell
nix develop

# Run tests
nix build .#openai-proxy.tests.proxy-proptest
nix build .#trtllm-validate.tests.trtllm-validate-test

# Check flake
nix flake check
```

## Architecture

```
┌─────────────────┐     ┌──────────────────┐     ┌─────────────────┐
│   Open WebUI    │────▶│   openai-proxy   │────▶│  Triton Server  │
│   (port 3000)   │     │   (port 9000)    │     │   (port 8000)   │
└─────────────────┘     └────────┬─────────┘     └─────────────────┘
                                 │
                    ┌────────────┼────────────┐
                    ▼            ▼            ▼
              ┌─────────┐  ┌──────────┐  ┌─────────┐
              │ SearXNG │  │   Jina   │  │ Metrics │
              │ (8888)  │  │ (remote) │  │ (8002)  │
              └─────────┘  └──────────┘  └─────────┘
```

## License

MIT
