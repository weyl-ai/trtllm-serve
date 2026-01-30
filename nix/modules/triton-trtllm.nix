# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
#                                         // triton-tensorrt-llm NixOS Module
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
#
# Self-contained TensorRT-LLM inference on Triton for NixOS.
#
# Services:
#   - triton: NVIDIA Triton with TRT-LLM backend
#   - openai-proxy: Haskell OpenAI-compatible API
#   - tool-server: Servant API with code sandbox + attestation
#   - searxng: Privacy-respecting metasearch (native, no Docker)
#
# Usage:
#   {
#     inputs.triton-tensorrt-llm.url = "github:straylight-software/triton-tensorrt-llm";
#     
#     nixosConfigurations.myhost = nixpkgs.lib.nixosSystem {
#       modules = [
#         { nixpkgs.overlays = [ triton-tensorrt-llm.overlays.default ]; }
#         triton-tensorrt-llm.nixosModules.default
#         {
#           services.triton-trtllm = {
#             enable = true;
#             model = "qwen3-32b";
#             enginePath = /var/lib/trtllm/engines/qwen3;
#           };
#         }
#       ];
#     };
#   }
#
# The module pulls packages from the overlay. Apply overlays.default first.
#
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

{ config, lib, pkgs, ... }:

let
  cfg = config.services.triton-trtllm;
in
{
  options.services.triton-trtllm = {
    enable = lib.mkEnableOption "TensorRT-LLM inference service";

    # ──────────────────────────────────────────────────────────────────────────
    # Package Overrides
    # ──────────────────────────────────────────────────────────────────────────
    # These default to packages from the overlay. If the overlay isn't applied,
    # users must set these explicitly.

    package = {
      tritonserver = lib.mkOption {
        type = lib.types.package;
        default = pkgs.tritonserver-trtllm;
        defaultText = lib.literalExpression "pkgs.tritonserver-trtllm";
        description = "Triton server package with TRT-LLM backend";
      };

      openaiProxy = lib.mkOption {
        type = lib.types.package;
        default = pkgs.openai-proxy;
        defaultText = lib.literalExpression "pkgs.openai-proxy";
        description = "OpenAI proxy package (Haskell/Warp)";
      };

      toolServer = lib.mkOption {
        type = lib.types.package;
        default = pkgs.tool-server;
        defaultText = lib.literalExpression "pkgs.tool-server";
        description = "Tool server package (Servant + OpenAPI3)";
      };
    };

    # ──────────────────────────────────────────────────────────────────────────
    # Model Configuration
    # ──────────────────────────────────────────────────────────────────────────

    model = lib.mkOption {
      type = lib.types.str;
      default = "qwen3";
      description = "Model name for identification and service naming";
    };

    enginePath = lib.mkOption {
      type = lib.types.str;
      description = "Path to TRT-LLM engine directory";
    };

    tokenizerPath = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Path to tokenizer (defaults to enginePath)";
    };

    hfModel = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "HuggingFace model ID for tokenizer download";
    };

    # ──────────────────────────────────────────────────────────────────────────
    # Triton Server
    # ──────────────────────────────────────────────────────────────────────────

    triton = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Enable Triton inference server";
      };

      wrapper = lib.mkOption {
        type = lib.types.nullOr lib.types.package;
        default = null;
        description = ''
          Optional wrapper package (e.g., tritonserver-qwen3) that sets up
          the model repository automatically. If set, this is used instead
          of raw tritonserver with --model-repository.
        '';
      };

      httpPort = lib.mkOption {
        type = lib.types.port;
        default = 8000;
        description = "HTTP port for Triton API";
      };

      grpcPort = lib.mkOption {
        type = lib.types.port;
        default = 8001;
        description = "gRPC port for Triton API";
      };

      metricsPort = lib.mkOption {
        type = lib.types.port;
        default = 8002;
        description = "Prometheus metrics port";
      };

      extraArgs = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [];
        description = "Additional arguments for tritonserver";
      };
    };

    # ──────────────────────────────────────────────────────────────────────────
    # OpenAI Proxy
    # ──────────────────────────────────────────────────────────────────────────

    openaiProxy = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Enable OpenAI-compatible proxy";
      };

      port = lib.mkOption {
        type = lib.types.port;
        default = 9000;
        description = "Port for OpenAI API";
      };

      stripThinking = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Strip <think>...</think> blocks from responses";
      };
    };

    # ──────────────────────────────────────────────────────────────────────────
    # Tool Server (Servant + OpenAPI3)
    # ──────────────────────────────────────────────────────────────────────────

    toolServer = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Enable Tool Server (code sandbox, attestation)";
      };

      port = lib.mkOption {
        type = lib.types.port;
        default = 9001;
        description = "Port for Tool Server API";
      };

      identityDir = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Path to identity directory (for attestation signing)";
      };

      attestationDir = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Path to attestation git repository";
      };
    };

    # ──────────────────────────────────────────────────────────────────────────
    # Open WebUI
    # ──────────────────────────────────────────────────────────────────────────

    openWebUI = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Enable Open WebUI chat interface";
      };

      port = lib.mkOption {
        type = lib.types.port;
        default = 3000;
        description = "Port for Open WebUI";
      };

      host = lib.mkOption {
        type = lib.types.str;
        default = "0.0.0.0";
        description = "Host to bind Open WebUI to";
      };
    };

    # ──────────────────────────────────────────────────────────────────────────
    # SearXNG (Native - no Docker required)
    # ──────────────────────────────────────────────────────────────────────────

    searxng = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Enable SearXNG metasearch for /tools/search";
      };

      port = lib.mkOption {
        type = lib.types.port;
        default = 8888;
        description = "Port for SearXNG";
      };

      secretKeyFile = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "File containing SearXNG secret key (generated if null)";
      };
    };

    # ──────────────────────────────────────────────────────────────────────────
    # User/Group/Paths
    # ──────────────────────────────────────────────────────────────────────────

    user = lib.mkOption {
      type = lib.types.str;
      default = "trtllm";
      description = "User to run services as";
    };

    group = lib.mkOption {
      type = lib.types.str;
      default = "trtllm";
      description = "Group for services";
    };

    dataDir = lib.mkOption {
      type = lib.types.str;
      default = "/var/lib/trtllm";
      description = "Data directory for caches and state";
    };
  };

  # ════════════════════════════════════════════════════════════════════════════
  # Implementation
  # ════════════════════════════════════════════════════════════════════════════

  config = lib.mkIf cfg.enable {

    # ──────────────────────────────────────────────────────────────────────────
    # Users & Groups
    # ──────────────────────────────────────────────────────────────────────────

    users.users.${cfg.user} = lib.mkIf (cfg.user == "trtllm") {
      isSystemUser = true;
      group = cfg.group;
      home = cfg.dataDir;
      description = "TensorRT-LLM inference service";
    };

    users.groups.${cfg.group} = lib.mkIf (cfg.group == "trtllm") {};

    # ──────────────────────────────────────────────────────────────────────────
    # Directories
    # ──────────────────────────────────────────────────────────────────────────

    systemd.tmpfiles.rules = [
      "d ${cfg.dataDir} 0755 ${cfg.user} ${cfg.group} -"
      "d ${cfg.dataDir}/huggingface 0755 ${cfg.user} ${cfg.group} -"
      "d ${cfg.dataDir}/workspaces 0755 ${cfg.user} ${cfg.group} -"
    ];

    # ──────────────────────────────────────────────────────────────────────────
    # Triton Server
    # ──────────────────────────────────────────────────────────────────────────

    systemd.services."trtllm-triton-${cfg.model}" = lib.mkIf cfg.triton.enable {
      description = "Triton TensorRT-LLM Server (${cfg.model})";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" "nvidia-persistenced.service" ];
      wants = [ "nvidia-persistenced.service" ];

      environment = {
        HOME = cfg.dataDir;
        HF_HOME = "${cfg.dataDir}/huggingface";
        TRTLLM_ENGINE_PATH = cfg.enginePath;
        TRTLLM_TOKENIZER_PATH = if cfg.tokenizerPath != null then cfg.tokenizerPath else cfg.enginePath;
        XDG_RUNTIME_DIR = "/run/trtllm-triton-${cfg.model}";
        TMPDIR = "/run/trtllm-triton-${cfg.model}";
      };

      serviceConfig = {
        Type = "simple";
        # Use wrapper if provided, otherwise use raw tritonserver with model-repository
        ExecStart = if cfg.triton.wrapper != null then
          "${cfg.triton.wrapper}/bin/${cfg.triton.wrapper.pname or "tritonserver-${cfg.model}"}"
        else
          lib.concatStringsSep " " ([
            "${cfg.package.tritonserver}/bin/tritonserver"
            "--model-repository=${cfg.enginePath}"
            "--http-port=${toString cfg.triton.httpPort}"
            "--grpc-port=${toString cfg.triton.grpcPort}"
            "--metrics-port=${toString cfg.triton.metricsPort}"
          ] ++ cfg.triton.extraArgs);
        Restart = "on-failure";
        RestartSec = "10s";

        User = cfg.user;
        Group = cfg.group;

        RuntimeDirectory = "trtllm-triton-${cfg.model}";
        RuntimeDirectoryMode = "0755";

        PrivateTmp = false;
        LimitNOFILE = 65536;
        LimitMEMLOCK = "infinity";
      };
    };

    # ──────────────────────────────────────────────────────────────────────────
    # OpenAI Proxy
    # ──────────────────────────────────────────────────────────────────────────

    systemd.services."trtllm-openai-proxy-${cfg.model}" = lib.mkIf cfg.openaiProxy.enable {
      description = "OpenAI Proxy for ${cfg.model} (Haskell/Warp)";
      wantedBy = [ "multi-user.target" ];
      after = [ "trtllm-triton-${cfg.model}.service" "network.target" ]
        ++ lib.optional cfg.searxng.enable "searx.service";
      wants = [ "trtllm-triton-${cfg.model}.service" ]
        ++ lib.optional cfg.searxng.enable "searx.service";

      environment = {
        HOME = cfg.dataDir;
        OPENAI_PROXY_PORT = toString cfg.openaiProxy.port;
        TRITON_URL = "http://localhost:${toString cfg.triton.httpPort}";
        MODEL_NAME = cfg.model;
        STRIP_THINKING = if cfg.openaiProxy.stripThinking then "true" else "false";
      } // lib.optionalAttrs cfg.searxng.enable {
        SEARXNG_URL = "http://localhost:${toString cfg.searxng.port}";
      };

      serviceConfig = {
        Type = "simple";
        ExecStart = "${cfg.package.openaiProxy}/bin/openai-proxy-hs";
        Restart = "on-failure";
        RestartSec = "5s";

        # Wait for Triton to be ready
        ExecStartPre = "${pkgs.writeShellScript "wait-for-triton" ''
          for i in $(seq 1 60); do
            if ${pkgs.curl}/bin/curl -sf http://localhost:${toString cfg.triton.httpPort}/v2/health/ready > /dev/null 2>&1; then
              exit 0
            fi
            echo "Waiting for Triton... ($i/60)"
            sleep 2
          done
          echo "Triton not ready after 120s"
          exit 1
        ''}";

        User = cfg.user;
        Group = cfg.group;

        NoNewPrivileges = true;
        PrivateTmp = true;
        ProtectSystem = "strict";
        ProtectHome = "read-only";
        ReadWritePaths = [ cfg.dataDir ];
      };
    };

    # ──────────────────────────────────────────────────────────────────────────
    # Tool Server
    # ──────────────────────────────────────────────────────────────────────────

    systemd.services."trtllm-tool-server" = lib.mkIf cfg.toolServer.enable {
      description = "Tool Server (Servant + OpenAPI3)";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ]
        ++ lib.optional cfg.searxng.enable "searx.service";
      wants = lib.optional cfg.searxng.enable "searx.service";

      environment = {
        HOME = cfg.dataDir;
        TOOL_SERVER_PORT = toString cfg.toolServer.port;
      } // lib.optionalAttrs cfg.searxng.enable {
        SEARXNG_URL = "http://localhost:${toString cfg.searxng.port}";
      } // lib.optionalAttrs (cfg.toolServer.identityDir != null) {
        IDENTITY_DIR = cfg.toolServer.identityDir;
      } // lib.optionalAttrs (cfg.toolServer.attestationDir != null) {
        ATTESTATION_DIR = cfg.toolServer.attestationDir;
      };

      serviceConfig = {
        Type = "simple";
        ExecStart = "${cfg.package.toolServer}/bin/tool-server";
        Restart = "on-failure";
        RestartSec = "5s";

        User = cfg.user;
        Group = cfg.group;

        NoNewPrivileges = true;
        PrivateTmp = true;
        ProtectSystem = "strict";
        ProtectHome = "read-only";
        ReadWritePaths = [ cfg.dataDir "${cfg.dataDir}/workspaces" ]
          ++ lib.optional (cfg.toolServer.attestationDir != null) cfg.toolServer.attestationDir;
      };
    };

    # ──────────────────────────────────────────────────────────────────────────
    # Open WebUI
    # ──────────────────────────────────────────────────────────────────────────

    services.open-webui = lib.mkIf cfg.openWebUI.enable {
      enable = true;
      port = cfg.openWebUI.port;
      host = cfg.openWebUI.host;
      environment = {
        OPENAI_API_BASE_URL = "http://localhost:${toString cfg.openaiProxy.port}/v1";
        OPENAI_API_KEY = "not-needed";  # Our proxy doesn't require auth
        WEBUI_AUTH = "False";  # Disable auth for local dev
        ENABLE_SIGNUP = "False";
        DEFAULT_MODELS = cfg.model;
      };
    };

    # ──────────────────────────────────────────────────────────────────────────
    # SearXNG (uses nixpkgs services.searx - no Docker)
    # ──────────────────────────────────────────────────────────────────────────

    services.searx = lib.mkIf cfg.searxng.enable {
      enable = true;
      
      settings = {
        server = {
          port = cfg.searxng.port;
          bind_address = "127.0.0.1";
          secret_key = if cfg.searxng.secretKeyFile != null 
            then "@SEARX_SECRET_KEY@"  # Will be replaced
            else "triton-trtllm-default-key-change-me";
        };
        
        search = {
          safe_search = 0;
          autocomplete = "google";
          default_lang = "en";
        };
        
        engines = lib.mapAttrsToList (name: enabled: { inherit name; engine = name; disabled = !enabled; }) {
          google = true;
          duckduckgo = true;
          bing = true;
          github = true;
          stackoverflow = true;
          wikipedia = true;
          arxiv = true;
        };
        
        outgoing = {
          request_timeout = 5.0;
          useragent_suffix = "triton-trtllm";
        };
      };
      
      runInUwsgi = true;
      uwsgiConfig = {
        http = "127.0.0.1:${toString cfg.searxng.port}";
      };
    };

    # ──────────────────────────────────────────────────────────────────────────
    # Firewall
    # ──────────────────────────────────────────────────────────────────────────

    networking.firewall.allowedTCPPorts = 
      lib.optional cfg.openaiProxy.enable cfg.openaiProxy.port
      ++ lib.optional cfg.toolServer.enable cfg.toolServer.port
      ++ lib.optional cfg.openWebUI.enable cfg.openWebUI.port;

    # ──────────────────────────────────────────────────────────────────────────
    # Info File
    # ──────────────────────────────────────────────────────────────────────────

    environment.etc."triton-trtllm/info".text = ''
      TensorRT-LLM Serve: ${cfg.model}
      ════════════════════════════════════════════════════════════════

      ${lib.optionalString cfg.triton.enable ''
      Triton Server:
        HTTP:    http://localhost:${toString cfg.triton.httpPort}
        gRPC:    localhost:${toString cfg.triton.grpcPort}
        Metrics: http://localhost:${toString cfg.triton.metricsPort}/metrics
        Health:  http://localhost:${toString cfg.triton.httpPort}/v2/health/ready
      ''}
      ${lib.optionalString cfg.openaiProxy.enable ''
      OpenAI Proxy:
        Base:    http://localhost:${toString cfg.openaiProxy.port}/v1
        Chat:    http://localhost:${toString cfg.openaiProxy.port}/v1/chat/completions
        Models:  http://localhost:${toString cfg.openaiProxy.port}/v1/models
        Health:  http://localhost:${toString cfg.openaiProxy.port}/health
      ''}
      ${lib.optionalString cfg.toolServer.enable ''
      Tool Server:
        Base:    http://localhost:${toString cfg.toolServer.port}
        OpenAPI: http://localhost:${toString cfg.toolServer.port}/openapi.json
        Health:  http://localhost:${toString cfg.toolServer.port}/health
      ''}
      ${lib.optionalString cfg.openWebUI.enable ''
      Open WebUI:
        Chat:    http://localhost:${toString cfg.openWebUI.port}
      ''}
      ${lib.optionalString cfg.searxng.enable ''
      SearXNG:
        Search:  http://localhost:${toString cfg.searxng.port}
      ''}
    '';
  };
}
