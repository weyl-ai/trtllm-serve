# TensorRT-LLM Serve (trtllm-serve) with speculative decoding support
#
# This package provides an OpenAI-compatible server using the TRT-LLM LLM API
# with PyTorch backend, supporting features not available in the C++ Triton backend:
#   - Eagle/Eagle3 speculative decoding
#   - MTP (Multi-Token Prediction)
#   - NGram speculative decoding
#
# Usage:
#   nix run .#trtllm-serve-qwen3-235b-eagle3
#
# The PyTorch backend loads models directly from HuggingFace checkpoints,
# so no pre-built TensorRT engines are needed (but startup is slower).

{
  lib,
  writeShellApplication,
  writeTextFile,
  python312,
  tritonserver-trtllm,
  openmpi,
  cuda,

  # Model configuration
  name,                    # e.g. "qwen3-235b-eagle3"
  hfModel,                 # HuggingFace model path
  tokenizerModel ? hfModel,

  # Server configuration
  listenHost ? "0.0.0.0",
  port ? 8000,
  
  # Model configuration
  tensorParallelSize ? 1,
  pipelineParallelSize ? 1,
  expertParallelSize ? null,  # For MoE models
  maxBatchSize ? 8,
  maxSeqLen ? 32768,
  maxNumTokens ? 8192,
  kvCacheFreeGpuMemFraction ? 0.85,
  
  # Speculative decoding configuration (optional)
  speculativeConfig ? null,
  # Example speculativeConfig:
  # {
  #   type = "eagle3";
  #   speculativeModelDir = "/path/to/eagle3/checkpoint";
  #   maxDraftLen = 3;
  #   eagle3OneModel = true;  # For Eagle3
  # }
  
  # Backend selection
  backend ? "pytorch",  # "pytorch" or "tensorrt"
  
  # Extra options (YAML file content)
  extraOptions ? null,
}:

let
  python = python312;
  triton = tritonserver-trtllm;
  worldSize = tensorParallelSize * pipelineParallelSize;
  cudaDevices = lib.concatStringsSep "," (map toString (lib.range 0 (worldSize - 1)));
  
  # Generate extra options YAML if speculative config is provided
  extraOptionsFile = if speculativeConfig != null || extraOptions != null then
    writeTextFile {
      name = "extra-llm-api-config-${name}.yml";
      text = if extraOptions != null then extraOptions else ''
enable_attention_dp: false
disable_overlap_scheduler: true
enable_autotuner: false

cuda_graph_config:
    max_batch_size: 1

${lib.optionalString (speculativeConfig != null) ''
speculative_config:
    decoding_type: ${speculativeConfig.type or "Eagle"}
    max_draft_len: ${toString (speculativeConfig.maxDraftLen or 3)}
    ${lib.optionalString (speculativeConfig ? speculativeModelDir) "speculative_model_dir: ${speculativeConfig.speculativeModelDir}"}
    ${lib.optionalString (speculativeConfig.eagle3OneModel or false) "eagle3_one_model: true"}
''}

kv_cache_config:
    enable_block_reuse: false
    free_gpu_memory_fraction: ${toString kvCacheFreeGpuMemFraction}
'';
    }
  else null;

in
writeShellApplication {
  name = "trtllm-serve-${name}";
  runtimeInputs = [ python openmpi ];
  
  text = ''
    # Environment setup
    export PYTHONPATH="${triton}/python''${PYTHONPATH:+:$PYTHONPATH}"
    export LD_LIBRARY_PATH="/run/opengl-driver/lib:${triton}/lib:${triton}/tensorrt_llm/lib:${cuda}/lib64:${openmpi}/lib''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
    export CUDA_HOME="${cuda}"
    export CUDA_VISIBLE_DEVICES="${cudaDevices}"
    
    # NCCL configuration for multi-GPU
    export NCCL_DEBUG=''${NCCL_DEBUG:-WARN}
    export NCCL_IB_DISABLE=1
    export NCCL_P2P_LEVEL=PHB
    export NCCL_SHM_DISABLE=0
    
    # Suppress PyTorch warnings
    export TORCH_CUDA_ARCH_LIST=""
    export PYTORCH_CUDA_ALLOC_CONF="expandable_segments:True"
    
    echo "════════════════════════════════════════════════════════════════"
    echo "  TensorRT-LLM Serve: ${name}"
    echo "════════════════════════════════════════════════════════════════"
    echo "Model:     ${hfModel}"
    echo "Backend:   ${backend}"
    echo "TP Size:   ${toString tensorParallelSize}"
    ${lib.optionalString (expertParallelSize != null) "echo \"EP Size:   ${toString expertParallelSize}\""}
    ${lib.optionalString (speculativeConfig != null) "echo \"Speculative: ${speculativeConfig.type or "Eagle"} (draft_len=${toString (speculativeConfig.maxDraftLen or 3)})\""}
    echo ""
    echo "Endpoints:"
    echo "  OpenAI: http://localhost:${toString port}/v1/chat/completions"
    echo "  Models: http://localhost:${toString port}/v1/models"
    echo "  Health: http://localhost:${toString port}/health"
    echo ""
    echo "════════════════════════════════════════════════════════════════"
    
    exec ${python}/bin/python -m tensorrt_llm.commands.serve serve \
      "${hfModel}" \
      --host "${listenHost}" \
      --port ${toString port} \
      --backend ${backend} \
      --max_batch_size ${toString maxBatchSize} \
      --max_num_tokens ${toString maxNumTokens} \
      --max_seq_len ${toString maxSeqLen} \
      --tp_size ${toString tensorParallelSize} \
      --pp_size ${toString pipelineParallelSize} \
      ${lib.optionalString (expertParallelSize != null) "--ep_size ${toString expertParallelSize}"} \
      --kv_cache_free_gpu_memory_fraction ${toString kvCacheFreeGpuMemFraction} \
      --trust_remote_code \
      ${lib.optionalString (extraOptionsFile != null) "--extra_llm_api_options ${extraOptionsFile}"} \
      "$@"
  '';
  
  meta = {
    description = "TensorRT-LLM Serve for ${name} (${backend} backend${lib.optionalString (speculativeConfig != null) ", ${speculativeConfig.type or "Eagle"} speculative decoding"})";
    mainProgram = "trtllm-serve-${name}";
  };
}
