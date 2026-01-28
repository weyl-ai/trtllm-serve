{
  description = "// straylight // triton-tensorrt-llm // TensorRT-LLM inference on Triton";

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
    flake-parts.url = "github:hercules-ci/flake-parts";

    # NVIDIA SDK provides CUDA, cuDNN, NCCL, TensorRT, Triton
    # Follow nvidia-sdk's nixpkgs to ensure cache hits
    nvidia-sdk.url = "github:weyl-ai/nvidia-sdk";
    nixpkgs.follows = "nvidia-sdk/nixpkgs";
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
            default = pkgs'.tool-server;

            # Haskell servers
            inherit (pkgs')
              openai-proxy     # OpenAI proxy for TRT-LLM
              tool-server      # Servant API with OpenAPI3
              trtllm-validate
              ;

            # Demo: full stack launcher
            demo = pkgs'.callPackage ./nix/demo.nix {
              inherit (pkgs') openai-proxy tool-server open-webui;
            };

            # Re-export from nvidia-sdk for convenience
            inherit (pkgs')
              tritonserver-trtllm
              cuda
              cudnn
              nccl
              tensorrt
              # TRT-LLM Python wrappers
              trtllm-python
              trtllm-build
              trtllm-env
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

            # ════════════════════════════════════════════════════════════════════
            # Nemotron-Nano-9B engine (small, fast)
            # Build with: nix build .#nemotron-nano-engine --option sandbox false
            # ════════════════════════════════════════════════════════════════════
            nemotron-nano-engine = pkgs'.trtllm-engine.mkEngine {
              name = "nemotron-nano-9b";
              hfModel = "nvidia/NVIDIA-Nemotron-Nano-9B-v2-NVFP4";
              quantization = "NVFP4";
              maxBatchSize = 8;
              maxSeqLen = 8192;
              maxNumTokens = 4096;
              tensorParallelSize = 1;
            };
          };

          devShells = {
            default = pkgs'.mkShell {
              packages = [
                pkgs'.tritonserver-trtllm
                pkgs'.openai-proxy
                pkgs'.tool-server
                pkgs'.trtllm-validate
                pkgs'.openmpi
                pkgs'.prrte
                pkgs'.python312
                # Language Coset compilers (for tool-server)
                pkgs'.rustc
                pkgs'.ghc
                pkgs'.dhall
              ];
              shellHook = ''
                echo "triton-tensorrt-llm — TensorRT-LLM inference on Triton"
                echo ""
                echo "Servers:"
                echo "  openai-proxy     — OpenAI-compatible proxy for TRT-LLM"
                echo "  tool-server      — Servant API with OpenAPI3 (code sandbox + attestation)"
                echo ""
                echo "Tools:"
                echo "  trtllm-validate  — TRT-LLM engine validation"
                echo "  tritonserver     — NVIDIA Triton Inference Server"
                echo ""

                # TRT-LLM Python environment
                export PYTHONPATH="${pkgs'.tritonserver-trtllm}/python''${PYTHONPATH:+:$PYTHONPATH}"
                export LD_LIBRARY_PATH="/run/opengl-driver/lib:${pkgs'.tritonserver-trtllm}/lib:${pkgs'.tritonserver-trtllm}/python/tensorrt_llm/libs:${pkgs'.cuda}/lib64:${pkgs'.cudnn}/lib:${pkgs'.nccl}/lib:${pkgs'.tensorrt}/lib:${pkgs'.openmpi}/lib:${pkgs'.python312}/lib''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
                export CUDA_HOME="${pkgs'.cuda}"
                # NixOS: Triton JIT compiler needs this to find libcuda.so
                export TRITON_LIBCUDA_PATH="/run/opengl-driver/lib"
              '';
            };
          };

          apps = {
            # Demo: full stack with Open WebUI
            default = {
              type = "app";
              program = "${self'.packages.demo}/bin/triton-trtllm-demo";
              meta.description = "Demo: OpenAI proxy + Tool Server + Open WebUI";
            };

            demo = {
              type = "app";
              program = "${self'.packages.demo}/bin/triton-trtllm-demo";
              meta.description = "Demo: OpenAI proxy + Tool Server + Open WebUI";
            };

            openai-proxy = {
              type = "app";
              program = "${pkgs'.openai-proxy}/bin/openai-proxy-hs";
              meta.description = "OpenAI-compatible proxy for TRT-LLM";
            };

            tool-server = {
              type = "app";
              program = "${pkgs'.tool-server}/bin/tool-server";
              meta.description = "Servant API with OpenAPI3: code sandbox + attestation";
            };

            open-webui = {
              type = "app";
              program = "${pkgs'.open-webui}/bin/open-webui";
              meta.description = "Open WebUI chat interface";
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

            # TRT-LLM Python environment
            trtllm-python = {
              type = "app";
              program = "${pkgs'.trtllm-python}/bin/python";
              meta.description = "Python with TensorRT-LLM environment";
            };

            trtllm-build = {
              type = "app";
              program = "${pkgs'.trtllm-build}/bin/trtllm-build";
              meta.description = "TensorRT-LLM engine build command";
            };

            # Quick start: load Qwen3-32B and chat
            qwen3 = {
              type = "app";
              program = "${pkgs'.qwen3-chat}/bin/qwen3-chat";
              meta.description = "Chat with Qwen3-32B-NVFP4";
            };
          };
        };

        flake = {
        overlays.default =
          final: prev:
          {
            # ════════════════════════════════════════════════════════════════════
            # Haskell servers
            # ════════════════════════════════════════════════════════════════════
            
            # OpenAI-compatible proxy for TRT-LLM (Warp-based)
            openai-proxy = final.callPackage ./nix/openai-proxy.nix { };
            
            # Servant API with OpenAPI3 (code sandbox + attestation)
            tool-server = final.callPackage ./nix/tool-server.nix {
              inherit (final) ast-grep git openssh gnupg;
              inherit (final) rustc ghc dhall lean4 purescript;
            };
            
            # TRT-LLM validation CLI
            trtllm-validate = final.haskellPackages.callPackage ./nix/trtllm-validate.nix { };

            # ════════════════════════════════════════════════════════════════════
            # TRT-LLM infrastructure
            # ════════════════════════════════════════════════════════════════════

            # TRT-LLM engine building infrastructure (function set, not a package)
            trtllm-engine = final.callPackage ./nix/trtllm-engine.nix {
              tritonserver-trtllm = final.tritonserver-trtllm;
              cuda = final.cuda;
              cudnn = final.cudnn;
              nccl = final.nccl;
              tensorrt = final.tensorrt;
              trtllm-validate = final.trtllm-validate;
            };

            # ════════════════════════════════════════════════════════════════════
            # TRT-LLM Python environment wrappers
            # ════════════════════════════════════════════════════════════════════
            
            # Python with TensorRT-LLM environment
            # Usage: nix run .#trtllm-python -- -c "from tensorrt_llm import LLM; print('ok')"
            trtllm-python = final.writeShellScriptBin "python" ''
              export PYTHONPATH="${final.tritonserver-trtllm}/python''${PYTHONPATH:+:$PYTHONPATH}"
              export LD_LIBRARY_PATH="/run/opengl-driver/lib:${final.tritonserver-trtllm}/lib:${final.tritonserver-trtllm}/python/tensorrt_llm/libs:${final.cuda}/lib64:${final.cudnn}/lib:${final.nccl}/lib:${final.tensorrt}/lib:${final.openmpi}/lib:${final.python312}/lib''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
              export CUDA_HOME="${final.cuda}"
              # NixOS: Triton JIT compiler needs this to find libcuda.so
              export TRITON_LIBCUDA_PATH="/run/opengl-driver/lib"
              exec ${final.python312}/bin/python "$@"
            '';

            # TRT-LLM build command wrapper
            trtllm-build = final.writeShellScriptBin "trtllm-build" ''
              export PYTHONPATH="${final.tritonserver-trtllm}/python''${PYTHONPATH:+:$PYTHONPATH}"
              export LD_LIBRARY_PATH="/run/opengl-driver/lib:${final.tritonserver-trtllm}/lib:${final.tritonserver-trtllm}/python/tensorrt_llm/libs:${final.cuda}/lib64:${final.cudnn}/lib:${final.nccl}/lib:${final.tensorrt}/lib:${final.openmpi}/lib:${final.python312}/lib''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
              export CUDA_HOME="${final.cuda}"
              # NixOS: Triton JIT compiler needs this to find libcuda.so
              export TRITON_LIBCUDA_PATH="/run/opengl-driver/lib"
              exec ${final.python312}/bin/python -m tensorrt_llm.commands.build "$@"
            '';

            # Full TRT-LLM development environment
            trtllm-env = final.buildEnv {
              name = "trtllm-env";
              paths = [
                final.trtllm-python
                final.trtllm-build
                final.tritonserver-trtllm
                final.openmpi
                final.prrte
              ];
            };

            # Quick start: chat with Qwen3-32B
            qwen3-chat = final.writeShellScriptBin "qwen3-chat" ''
              export PYTHONPATH="${final.tritonserver-trtllm}/python''${PYTHONPATH:+:$PYTHONPATH}"
              export LD_LIBRARY_PATH="/run/opengl-driver/lib:${final.tritonserver-trtllm}/lib:${final.tritonserver-trtllm}/python/tensorrt_llm/libs:${final.cuda}/lib64:${final.cudnn}/lib:${final.nccl}/lib:${final.tensorrt}/lib:${final.openmpi}/lib:${final.python312}/lib''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
              export CUDA_HOME="${final.cuda}"
              export TRITON_LIBCUDA_PATH="/run/opengl-driver/lib"
              exec ${final.python312}/bin/python -c "
from tensorrt_llm import LLM, SamplingParams
print('Loading Qwen3-32B-NVFP4...')
llm = LLM(model='nvidia/Qwen3-32B-NVFP4', tensor_parallel_size=1)
print('Ready. Type your message (Ctrl+D to exit):')
sampling = SamplingParams(max_tokens=256)
while True:
    try:
        prompt = input('> ')
        if not prompt.strip(): continue
        out = llm.generate([prompt], sampling)[0].outputs[0].text
        print(out)
    except EOFError:
        break
"
            '';

            # TRT-LLM Serve (PyTorch backend with speculative decoding)
            mkTrtllmServe = args: final.callPackage ./nix/trtllm-serve.nix ({
              tritonserver-trtllm = final.tritonserver-trtllm;
              cuda = final.cuda;
            } // args);
          };

        # NixOS module for TensorRT-LLM inference on Triton
        nixosModules.default = ./nix/modules/triton-trtllm.nix;
        nixosModules.triton-trtllm = ./nix/modules/triton-trtllm.nix;
      };
    };
}
