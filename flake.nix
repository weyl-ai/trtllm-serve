{
  description = "// weyl // trtllm-serve // TensorRT-LLM inference stack";

  nixConfig = {
    extra-substituters = [
      "https://weyl-ai.cachix.org"
    ];

    extra-trusted-public-keys = [
      "weyl-ai.cachix.org-1:cR0SpSAPw7wejZ21ep4SLojE77gp5F2os260eEWqTTw="
    ];

    extra-experimental-features = [
      "nix-command"
      "flakes"
      "pipe-operators"
    ];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";

    # NVIDIA SDK provides CUDA, cuDNN, NCCL, TensorRT, Triton
    nvidia-sdk = {
      url = "github:weyl-ai/nvidia-sdk";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];

      perSystem =
        {
          config,
          self',
          inputs',
          pkgs,
          system,
          ...
        }:
        let
          # Get nvidia-sdk overlay applied
          pkgs' = import inputs.nixpkgs {
            inherit system;

            config = {
              cudaSupport = true;
              cudaCapabilities = [ "12.0" ];
              allowUnfree = true;
            };

            overlays = [
              inputs.nvidia-sdk.overlays.default
              inputs.nvidia-sdk.overlays.stdenv-overlay
              inputs.self.overlays.default
            ];
          };
        in
        {
          packages = {
            default = pkgs'.openai-proxy;

            # Haskell AI Gateway (OpenAI proxy + tools + metrics)
            inherit (pkgs')
              openai-proxy
              trtllm-validate
              ;

            # Re-export from nvidia-sdk for convenience
            inherit (pkgs')
              tritonserver-trtllm
              cuda
              cudnn
              nccl
              tensorrt
              ;

            # ════════════════════════════════════════════════════════════════════
            # Example: Qwen3-32B-NVFP4 engine
            # Build with: nix build .#qwen3-32b-engine --option sandbox false
            # ════════════════════════════════════════════════════════════════════
            qwen3-32b-engine = pkgs'.trtllm-engine.mkEngine {
              name = "qwen3-32b-nvfp4";
              hfModel = "nvidia/Qwen3-32B-NVFP4";
              quantization = "NVFP4";
              maxBatchSize = 8;
              maxSeqLen = 16384;
              maxNumTokens = 8192;
              tensorParallelSize = 1;
            };

            # Qwen3-32B Triton server runtime
            tritonserver-qwen3 = pkgs'.trtllm-engine.mkTritonServerRuntime {
              name = "qwen3";
              tokenizerModel = "nvidia/Qwen3-32B-NVFP4";
              httpPort = 8000;
              grpcPort = 8001;
              metricsPort = 8002;
            };
          };

          devShells = {
            default = pkgs'.mkShell {
              packages = [
                pkgs'.tritonserver-trtllm
                pkgs'.openai-proxy
                pkgs'.trtllm-validate
                pkgs'.openmpi
                pkgs'.prrte
                pkgs'.python312
              ];
              shellHook = ''
                echo "trtllm-serve — TensorRT-LLM inference stack"
                echo ""
                echo "Tools:"
                echo "  openai-proxy     — Haskell AI Gateway (OpenAI proxy + tools)"
                echo "  trtllm-validate  — TRT-LLM engine validation"
                echo "  tritonserver     — NVIDIA Triton Inference Server"
                echo ""

                # TRT-LLM Python environment
                export PYTHONPATH="${pkgs'.tritonserver-trtllm}/python''${PYTHONPATH:+:$PYTHONPATH}"
                export LD_LIBRARY_PATH="/run/opengl-driver/lib:${pkgs'.tritonserver-trtllm}/lib:${pkgs'.tritonserver-trtllm}/python/tensorrt_llm/libs:${pkgs'.cuda}/lib64:${pkgs'.cudnn}/lib:${pkgs'.nccl}/lib:${pkgs'.tensorrt}/lib:${pkgs'.openmpi}/lib:${pkgs'.python312}/lib''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
                export CUDA_HOME="${pkgs'.cuda}"
              '';
            };
          };

          apps = {
            # Unified AI Gateway
            default = {
              type = "app";
              program = "${pkgs'.openai-proxy}/bin/openai-proxy-hs";
              meta.description = "Haskell AI Gateway: OpenAI proxy + tools + metrics";
            };

            openai-proxy = {
              type = "app";
              program = "${pkgs'.openai-proxy}/bin/openai-proxy-hs";
              meta.description = "Haskell AI Gateway: OpenAI proxy + tools + metrics";
            };

            trtllm-validate = {
              type = "app";
              program = "${pkgs'.trtllm-validate}/bin/trtllm-validate";
              meta.description = "TRT-LLM engine validation tool";
            };

            tritonserver = {
              type = "app";
              program = "${pkgs'.tritonserver-trtllm}/bin/tritonserver";
              meta.description = "NVIDIA Triton Inference Server with TRT-LLM backend";
            };
          };
        };

        flake = {
        overlays.default =
          final: prev:
          {
            # Haskell packages
            openai-proxy = final.callPackage ./nix/openai-proxy.nix { };
            trtllm-validate = final.haskellPackages.callPackage ./nix/trtllm-validate.nix { };

            # TRT-LLM engine building infrastructure (function set, not a package)
            trtllm-engine = final.callPackage ./nix/trtllm-engine.nix {
              tritonserver-trtllm = final.tritonserver-trtllm;
              cuda = final.cuda;
              trtllm-validate = final.trtllm-validate;
            };

            # TRT-LLM Serve (PyTorch backend with speculative decoding)
            mkTrtllmServe = args: final.callPackage ./nix/trtllm-serve.nix ({
              tritonserver-trtllm = final.tritonserver-trtllm;
              cuda = final.cuda;
            } // args);

            # Convenience aliases
            tool-server = final.openai-proxy;
          };

        # NixOS module for AI inference services
        nixosModules.default = { config, lib, pkgs, ... }:
          let
            cfg = config.services.trtllm-serve;
          in
          {
            options.services.trtllm-serve = {
              enable = lib.mkEnableOption "TensorRT-LLM inference service";

              openaiProxy = {
                enable = lib.mkEnableOption "OpenAI-compatible proxy";
                port = lib.mkOption {
                  type = lib.types.port;
                  default = 9000;
                  description = "Port for OpenAI proxy";
                };
                tritonHost = lib.mkOption {
                  type = lib.types.str;
                  default = "localhost";
                  description = "Triton server hostname";
                };
                tritonPort = lib.mkOption {
                  type = lib.types.port;
                  default = 8001;
                  description = "Triton gRPC port";
                };
              };
            };

            config = lib.mkIf cfg.enable {
              systemd.services.openai-proxy = lib.mkIf cfg.openaiProxy.enable {
                description = "OpenAI-compatible proxy for TensorRT-LLM";
                wantedBy = [ "multi-user.target" ];
                after = [ "network.target" ];

                environment = {
                  TRITON_HOST = cfg.openaiProxy.tritonHost;
                  TRITON_PORT = toString cfg.openaiProxy.tritonPort;
                  PORT = toString cfg.openaiProxy.port;
                };

                serviceConfig = {
                  ExecStart = "${pkgs.openai-proxy}/bin/openai-proxy-hs";
                  Restart = "always";
                  RestartSec = 5;
                };
              };
            };
          };
      };
    };
}
