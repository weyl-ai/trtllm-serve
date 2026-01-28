# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
#                                              // trtllm-serve NixOS Module
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
#
# TensorRT-LLM inference stack for NixOS.
#
# Services:
#   - tritonserver: NVIDIA Triton with TRT-LLM backend
#   - openai-proxy: Haskell AI Gateway (OpenAI-compatible API)
#   - searxng: Privacy-respecting metasearch (optional)
#
# Usage:
#   services.trtllm-serve = {
#     enable = true;
#     model = "qwen3-32b";
#     enginePath = "/var/lib/trtllm/engines/qwen3";
#   };
#
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

{ config, lib, pkgs, ... }:

let
  cfg = config.services.trtllm-serve;
in
{
  options.services.trtllm-serve = {
    enable = lib.mkEnableOption "TensorRT-LLM inference service";

    # ──────────────────────────────────────────────────────────────────────────
    # Model Configuration
    # ──────────────────────────────────────────────────────────────────────────

    model = lib.mkOption {
      type = lib.types.str;
      default = "qwen3";
      description = "Model name for identification";
    };

    enginePath = lib.mkOption {
      type = lib.types.path;
      description = "Path to TRT-LLM engine directory";
    };

    tokenizerPath = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
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
    # OpenAI Proxy (Haskell AI Gateway)
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
    # SearXNG (Web Search)
    # ──────────────────────────────────────────────────────────────────────────

    searxng = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Enable SearXNG metasearch for tool use";
      };

      port = lib.mkOption {
        type = lib.types.port;
        default = 8888;
        description = "Port for SearXNG";
      };

      imageDigest = lib.mkOption {
        type = lib.types.str;
        default = "sha256:5c8621cef49a936244cbcb432e0ca99b040efd43c580782dd8449bc0015f02e7";
        description = "SHA256 digest of SearXNG image";
      };
    };

    # ──────────────────────────────────────────────────────────────────────────
    # User/Group
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
      type = lib.types.path;
      default = "/var/lib/trtllm";
      description = "Data directory for caches and state";
    };
  };

  config = lib.mkIf cfg.enable {

    # ──────────────────────────────────────────────────────────────────────────
    # Users
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
    ];

    # ──────────────────────────────────────────────────────────────────────────
    # Triton Server
    # ──────────────────────────────────────────────────────────────────────────

    systemd.services."triton-${cfg.model}" = lib.mkIf cfg.triton.enable {
      description = "Triton TensorRT-LLM Server (${cfg.model})";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" "nvidia-persistenced.service" ];
      wants = [ "nvidia-persistenced.service" ];

      environment = {
        HOME = cfg.dataDir;
        HF_HOME = "${cfg.dataDir}/huggingface";
        TRTLLM_ENGINE_PATH = toString cfg.enginePath;
        TRTLLM_TOKENIZER_PATH = toString (if cfg.tokenizerPath != null then cfg.tokenizerPath else cfg.enginePath);
        XDG_RUNTIME_DIR = "/run/triton-${cfg.model}";
        TMPDIR = "/run/triton-${cfg.model}";
      };

      serviceConfig = {
        Type = "simple";
        ExecStart = "${pkgs.tritonserver-trtllm}/bin/tritonserver-${cfg.model}";
        Restart = "on-failure";
        RestartSec = "10s";

        User = cfg.user;
        Group = cfg.group;

        RuntimeDirectory = "triton-${cfg.model}";
        RuntimeDirectoryMode = "0755";

        PrivateTmp = false;
        LimitNOFILE = 65536;
        LimitMEMLOCK = "infinity";
      };
    };

    # ──────────────────────────────────────────────────────────────────────────
    # OpenAI Proxy
    # ──────────────────────────────────────────────────────────────────────────

    systemd.services."openai-proxy-${cfg.model}" = lib.mkIf cfg.openaiProxy.enable {
      description = "OpenAI Proxy for ${cfg.model} (Haskell/Warp)";
      wantedBy = [ "multi-user.target" ];
      after = [ "triton-${cfg.model}.service" "network.target" ];
      wants = [ "triton-${cfg.model}.service" ];

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
        ExecStart = "${pkgs.openai-proxy}/bin/openai-proxy-hs";
        Restart = "on-failure";
        RestartSec = "5s";

        # Wait for Triton
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
    # SearXNG
    # ──────────────────────────────────────────────────────────────────────────

    virtualisation.oci-containers.containers.searxng = lib.mkIf cfg.searxng.enable {
      image = "searxng/searxng@${cfg.searxng.imageDigest}";
      ports = [ "${toString cfg.searxng.port}:8080" ];

      environment = {
        SEARXNG_BASE_URL = "http://localhost:${toString cfg.searxng.port}";
      };

      volumes = [
        "${cfg.dataDir}/searxng:/etc/searxng:rw"
      ];
    };

    systemd.tmpfiles.rules = lib.mkIf cfg.searxng.enable [
      "d ${cfg.dataDir}/searxng 0700 root root -"
    ];

    # ──────────────────────────────────────────────────────────────────────────
    # Firewall
    # ──────────────────────────────────────────────────────────────────────────

    networking.firewall.allowedTCPPorts = lib.mkIf cfg.openaiProxy.enable [
      cfg.openaiProxy.port
    ];

    # ──────────────────────────────────────────────────────────────────────────
    # Info
    # ──────────────────────────────────────────────────────────────────────────

    environment.etc."trtllm-serve/endpoints".text = ''
      TensorRT-LLM Serve: ${cfg.model}
      ════════════════════════════════════════════════════════════════

      Triton:
        HTTP:    http://localhost:${toString cfg.triton.httpPort}
        gRPC:    localhost:${toString cfg.triton.grpcPort}
        Metrics: http://localhost:${toString cfg.triton.metricsPort}/metrics

      OpenAI API:
        Base:    http://localhost:${toString cfg.openaiProxy.port}/v1
        Chat:    http://localhost:${toString cfg.openaiProxy.port}/v1/chat/completions
        Models:  http://localhost:${toString cfg.openaiProxy.port}/v1/models
        Health:  http://localhost:${toString cfg.openaiProxy.port}/health

      ${lib.optionalString cfg.searxng.enable ''
      SearXNG:
        Web:     http://localhost:${toString cfg.searxng.port}
      ''}
    '';
  };
}
