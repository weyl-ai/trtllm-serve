# TensorRT-LLM Engine Builder
#
# Builds TRT-LLM engines using the canonical workflow:
#   HuggingFace Model → Model.from_hugging_face() → save_checkpoint() → trtllm-build → Triton
#
# These are IMPURE builds that require GPU access (__noChroot = true).
#
# Usage:
#   engines = callPackage ./trtllm-engine.nix { };
#
#   # For pre-quantized NVFP4 models (nvidia/*)
#   qwen3-engine = engines.mkEngine {
#     name = "qwen3-32b-nvfp4";
#     hfModel = /path/to/nvidia/Qwen3-32B-NVFP4;  # or use mkHfModel
#     modelType = "qwen";
#     tensorParallelSize = 4;
#   };
#
#   qwen3-triton = engines.mkTritonRepo {
#     name = "qwen3-32b";
#     engine = qwen3-engine;
#     tokenizer = /path/to/nvidia/Qwen3-32B-NVFP4;
#   };

{
  lib,
  stdenvNoCC,
  stdenv,
  runCommand,
  writeTextFile,
  writeShellApplication,
  cacert,
  git,
  git-lfs,
  python312,
  openmpi,
  tritonserver-trtllm,
  cuda,
  trtllm-validate,
}:

let
  python = python312;
  triton = tritonserver-trtllm;

  # Model class mapping for TRT-LLM
  modelClasses = {
    qwen = "QWenForCausalLM";
    llama = "LLaMAForCausalLM";
    phi = "PhiForCausalLM";
    mixtral = "MixtralForCausalLM";
    gemma = "GemmaForCausalLM";
  };

  # Environment setup for TRT-LLM commands
  envSetup = ''
    export PYTHONPATH="${triton}/python''${PYTHONPATH:+:$PYTHONPATH}"
    export LD_LIBRARY_PATH="/run/opengl-driver/lib:${triton}/lib:${triton}/tensorrt_llm/lib:${cuda}/lib64:${openmpi}/lib:${python}/lib''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
    export CUDA_HOME="${cuda}"
    export HOME="$TMPDIR/home"
    export HF_HOME="$TMPDIR/hf_cache"
    export TLLM_LOG_LEVEL="WARNING"
    mkdir -p "$HOME" "$HF_HOME"
    
    # NCCL configuration for multi-GPU (PCIe topology)
    export NCCL_DEBUG=INFO
    export NCCL_IB_DISABLE=1          # No InfiniBand on this system
    export NCCL_P2P_LEVEL=PHB         # PCIe peer-to-peer via host bridge
    export NCCL_SHM_DISABLE=0         # Enable shared memory
  '';

in
rec {
  inherit python openmpi triton cuda envSetup;

  # ============================================================================
  # mkHfModel: Download a HuggingFace model (Fixed Output Derivation)
  # ============================================================================
  mkHfModel =
    {
      name,           # e.g. "qwen3-32b-nvfp4"
      model,          # HuggingFace model ID, e.g. "nvidia/Qwen3-32B-NVFP4"
      hash,           # NAR hash of the downloaded model
      revision ? "main",
    }:
    stdenvNoCC.mkDerivation {
      pname = "hf-model-${name}";
      version = revision;

      nativeBuildInputs = [ git git-lfs cacert ];

      outputHashAlgo = "sha256";
      outputHashMode = "recursive";
      outputHash = hash;

      SSL_CERT_FILE = "${cacert}/etc/ssl/certs/ca-bundle.crt";
      GIT_SSL_CAINFO = "${cacert}/etc/ssl/certs/ca-bundle.crt";

      buildCommand = ''
        export HOME=$TMPDIR
        git lfs install --skip-repo
        git clone --depth 1 --branch ${revision} https://huggingface.co/${model} $out
        rm -rf $out/.git
      '';

      meta = {
        description = "HuggingFace model: ${model}";
        homepage = "https://huggingface.co/${model}";
      };
    };

  # ============================================================================
  # mkEngine: Build TensorRT engine from HuggingFace model
  # ============================================================================
  #
  # Uses the LLM API (same as trtllm-bench build) which handles:
  #   - Pre-quantized models (NVFP4, FP8)
  #   - Automatic checkpoint conversion
  #   - Proper plugin configuration
  #
  mkEngine =
    {
      name,                    # e.g. "qwen3-32b-nvfp4"
      hfModel,                 # HuggingFace model ID or local path
      quantization ? null,     # null, "NVFP4", "FP8", "W8A16", "W4A16", etc.
      tensorParallelSize ? 1,
      pipelineParallelSize ? 1,
      maxBatchSize ? 8,
      maxSeqLen ? 16384,
      maxNumTokens ? 8192,
      # MoE-specific options (for Mixture of Experts models)
      # moe_tp_size * moe_ep_size must equal tensorParallelSize
      moeTensorParallelSize ? null,   # MoE tensor parallel (splits expert weights)
      moeExpertParallelSize ? null,   # MoE expert parallel (distributes whole experts)
      moePlugin ? "auto",             # MoE plugin: auto, bfloat16, float16
    }:
    let
      worldSize = tensorParallelSize * pipelineParallelSize;
      # Generate CUDA_VISIBLE_DEVICES string: "0" for 1 GPU, "0,1,2,3" for 4 GPUs
      cudaDevices = lib.concatStringsSep "," (map toString (lib.range 0 (worldSize - 1)));
      # Check if hfModel is a local path (starts with /)
      isLocalPath = lib.hasPrefix "/" hfModel;
    in
    stdenv.mkDerivation {
      pname = "trtllm-engine-${name}";
      version = "1.0.0";

      __noChroot = true;

      nativeBuildInputs = [ python openmpi ];

      buildCommand = ''
        ${envSetup}
        export CUDA_VISIBLE_DEVICES="${cudaDevices}"

        mkdir -p "$out"

        echo "════════════════════════════════════════════════════════════════"
        echo "Building TensorRT-LLM engine: ${name}"
        echo "════════════════════════════════════════════════════════════════"
        echo "Model: ${hfModel}"
        echo "Quantization: ${if quantization != null then quantization else "none (auto-detect)"}"
        echo "Tensor Parallel: ${toString tensorParallelSize}"
        echo "Pipeline Parallel: ${toString pipelineParallelSize}"
        echo "World Size: ${toString worldSize}"
        echo "CUDA Devices: ${cudaDevices}"
        echo ""
        
        # ══════════════════════════════════════════════════════════════════
        # Pre-flight check: detect embedded quantization in HF model
        # ══════════════════════════════════════════════════════════════════
        ${lib.optionalString isLocalPath ''
        HF_QUANT=$(${trtllm-validate}/bin/trtllm-validate detect-quant "${hfModel}" || echo "")
        if [[ -n "$HF_QUANT" ]]; then
          echo "Detected pre-quantized model: $HF_QUANT"
          ${lib.optionalString (quantization == null) ''
          echo "  INFO: Model has embedded $HF_QUANT quantization config"
          echo "  The LLM() API will auto-detect this for multi-GPU builds"
          ''}
          ${lib.optionalString (quantization != null) ''
          if [[ "$HF_QUANT" != "${quantization}" ]]; then
            echo "  WARNING: Specified quantization (${quantization}) differs from model ($HF_QUANT)"
          fi
          ''}
        fi
        ''}

        ${if worldSize == 1 then ''
        # Single GPU: use trtllm-bench build (simpler, handles MPI hang with timeout)
        timeout 900 mpirun -np 1 --allow-run-as-root \
          -x LD_LIBRARY_PATH -x PYTHONPATH -x CUDA_VISIBLE_DEVICES -x HOME -x HF_HOME \
          ${python}/bin/python -m tensorrt_llm.commands.bench \
            --model ${hfModel} \
            --workspace "$TMPDIR" \
            build \
            ${lib.optionalString (quantization != null) "--quantization ${quantization}"} \
            --max_batch_size ${toString maxBatchSize} \
            --max_num_tokens ${toString maxNumTokens} \
            --max_seq_len ${toString maxSeqLen} \
            --tp_size 1 \
            --pp_size 1 \
            --trust_remote_code True || true

        ENGINE_DIR=$(find "$TMPDIR" -type d -name "tmp.engine" 2>/dev/null | head -1)
        '' else ''
        # Multi-GPU: use trtllm-build with checkpoint conversion
        # The LLM API doesn't work properly for multi-GPU engine building due to MPI issues.
        # Instead, we use the two-step approach:
        # 1. Get HF model (download or use local path)
        # 2. Convert HF model to TRT-LLM checkpoint format
        # 3. Build engine with trtllm-build
        
        CHECKPOINT_DIR="$TMPDIR/checkpoint"
        ENGINE_DIR="$TMPDIR/engine"
        mkdir -p "$CHECKPOINT_DIR" "$ENGINE_DIR"
        export CHECKPOINT_DIR ENGINE_DIR
        
        ${if isLocalPath then ''
        # Using local model path
        MODEL_DIR="${hfModel}"
        export MODEL_DIR
        echo "Step 1: Using local model at $MODEL_DIR"
        ls -la "$MODEL_DIR" | head -20
        '' else ''
        MODEL_DIR="$TMPDIR/model"
        mkdir -p "$MODEL_DIR"
        export MODEL_DIR
        
        echo "Step 1: Downloading HuggingFace model..."
        ${python}/bin/python ${../scripts/trtllm-download-model.py} "${hfModel}" "$MODEL_DIR"
        ''}

        echo "Step 2+3: Building engine using LLM API (handles MoE properly)..."
        ${python}/bin/python ${../scripts/trtllm-build-engine.py} "$MODEL_DIR" "$ENGINE_DIR" \
          --tp=${toString tensorParallelSize} \
          --pp=${toString pipelineParallelSize} \
          --cuda-devices="${cudaDevices}"
        
        # Copy tokenizer files from downloaded model (but NOT config.json - that's the engine config!)
        echo "Copying tokenizer files..."
        cp "$MODEL_DIR"/tokenizer.json "$ENGINE_DIR/" 2>/dev/null || true
        cp "$MODEL_DIR"/tokenizer_config.json "$ENGINE_DIR/" 2>/dev/null || true
        cp "$MODEL_DIR"/vocab.json "$ENGINE_DIR/" 2>/dev/null || true
        cp "$MODEL_DIR"/merges.txt "$ENGINE_DIR/" 2>/dev/null || true
        cp "$MODEL_DIR"/added_tokens.json "$ENGINE_DIR/" 2>/dev/null || true
        cp "$MODEL_DIR"/special_tokens_map.json "$ENGINE_DIR/" 2>/dev/null || true
        cp "$MODEL_DIR"/*.jinja "$ENGINE_DIR/" 2>/dev/null || true
        ''}

        if [[ -z "$ENGINE_DIR" ]] || [[ ! -f "$ENGINE_DIR/rank0.engine" ]]; then
          echo "ERROR: Engine not found"
          find "$TMPDIR" -name "*.engine" -o -name "config.json" 2>/dev/null || true
          exit 1
        fi
        
        echo "Found engine at: $ENGINE_DIR"
        cp -r "$ENGINE_DIR"/* "$out/"
        
        # Verify all rank engines exist for multi-GPU
        echo ""
        echo "Engine files:"
        ls -la "$out/"
        
        for rank in $(seq 0 $((${toString worldSize} - 1))); do
          if [[ ! -f "$out/rank$rank.engine" ]]; then
            echo "ERROR: rank$rank.engine not found"
            exit 1
          fi
          echo "  rank$rank.engine: $(du -h "$out/rank$rank.engine" | cut -f1)"
        done
        
        # ══════════════════════════════════════════════════════════════════
        # Post-build validations to catch quantization failures early
        # ══════════════════════════════════════════════════════════════════
        
        echo ""
        echo "Running post-build validations..."
        
        # Validation 1: Check quant_algo in config.json matches expected
        ${lib.optionalString (quantization != null) ''
        ${trtllm-validate}/bin/trtllm-validate validate-engine "$out" "${quantization}"
        ''}
        
        # Validation 2: Check engine size is reasonable for quantization level
        ${lib.optionalString (quantization != null) ''
        ${trtllm-validate}/bin/trtllm-validate check-size "$out" "${quantization}"
        ''}
        
        # Validation 3: Quick sanity inference (catch garbage output early)
        echo ""
        echo "Running sanity inference test..."
        ${python}/bin/python ${../scripts/trtllm-sanity-check.py} "$out" --cuda-devices="${cudaDevices}"
        
        echo ""
        echo "Engine build complete: ${toString worldSize} rank(s)"
      '';

      meta.description = "TensorRT-LLM engine for ${name} (TP=${toString tensorParallelSize}, PP=${toString pipelineParallelSize})";
    };

  # ============================================================================
  # mkTritonRepo: Generate Triton model repository with tensorrtllm backend
  # ============================================================================
  mkTritonRepo =
    {
      name,           # e.g. "qwen3-32b"
      engine,         # Output of mkEngine
      tokenizer,      # Path to tokenizer (usually same as HF model)
      maxBatchSize ? 8,
      batchSchedulerPolicy ? "guaranteed_no_evict",
      kvCacheFreeGpuMemFraction ? 0.9,
      enableChunkedContext ? true,
      decodingMode ? "auto",
    }:
    let
      configPbtxt = writeTextFile {
        name = "config.pbtxt";
        text = ''
          name: "tensorrt_llm"
          backend: "tensorrtllm"
          max_batch_size: ${toString maxBatchSize}

          model_transaction_policy {
            decoupled: true
          }

          dynamic_batching {
            preferred_batch_size: [ 1, 2, 4, 8 ]
            max_queue_delay_microseconds: 100
          }

          input [
            { name: "input_ids",        data_type: TYPE_INT32, dims: [ -1 ] },
            { name: "input_lengths",    data_type: TYPE_INT32, dims: [ 1 ], reshape: { shape: [ ] } },
            { name: "request_output_len", data_type: TYPE_INT32, dims: [ 1 ], reshape: { shape: [ ] } },
            { name: "end_id",           data_type: TYPE_INT32, dims: [ 1 ], reshape: { shape: [ ] }, optional: true },
            { name: "pad_id",           data_type: TYPE_INT32, dims: [ 1 ], reshape: { shape: [ ] }, optional: true },
            { name: "streaming",        data_type: TYPE_BOOL,  dims: [ 1 ], reshape: { shape: [ ] }, optional: true },
            { name: "temperature",      data_type: TYPE_FP32,  dims: [ 1 ], reshape: { shape: [ ] }, optional: true },
            { name: "top_p",            data_type: TYPE_FP32,  dims: [ 1 ], reshape: { shape: [ ] }, optional: true },
            { name: "top_k",            data_type: TYPE_INT32, dims: [ 1 ], reshape: { shape: [ ] }, optional: true }
          ]

          output [
            { name: "output_ids",       data_type: TYPE_INT32, dims: [ -1, -1 ] },
            { name: "sequence_length",  data_type: TYPE_INT32, dims: [ -1 ] },
            { name: "cum_log_probs",    data_type: TYPE_FP32,  dims: [ -1 ] },
            { name: "output_log_probs", data_type: TYPE_FP32,  dims: [ -1, -1 ] }
          ]

          instance_group [
            { count: 1, kind: KIND_CPU }
          ]

          parameters: { key: "gpt_model_type",                value: { string_value: "inflight_fused_batching" } }
          parameters: { key: "gpt_model_path",                value: { string_value: "ENGINE_PATH_PLACEHOLDER" } }
          parameters: { key: "batch_scheduler_policy",        value: { string_value: "${batchSchedulerPolicy}" } }
          parameters: { key: "kv_cache_free_gpu_mem_fraction", value: { string_value: "${toString kvCacheFreeGpuMemFraction}" } }
          ${lib.optionalString enableChunkedContext ''parameters: { key: "enable_chunked_context", value: { string_value: "true" } }''}
          parameters: { key: "decoding_mode",                 value: { string_value: "${decodingMode}" } }
        '';
      };
    in
    runCommand "triton-repo-${name}" {} ''
      mkdir -p $out/tensorrt_llm/1

      # Copy and patch config
      sed 's|ENGINE_PATH_PLACEHOLDER|${engine}|g' ${configPbtxt} > $out/tensorrt_llm/config.pbtxt

      # Create version marker
      touch $out/tensorrt_llm/1/.keep

      # Link tokenizer
      ln -s ${tokenizer} $out/tokenizer

      echo "Triton model repository created at $out"
      echo "  Engine: ${engine}"
      echo "  Tokenizer: ${tokenizer}"
    '';

  # ============================================================================
  # mkTritonServer: Create a wrapper script to run Triton with the model repo
  # ============================================================================
  mkTritonServer =
    {
      name,
      repo,           # Output of mkTritonRepo
      httpPort ? 8000,
      grpcPort ? 8001,
      metricsPort ? 8002,
      worldSize ? 1,  # For multi-GPU: number of GPUs
    }:
    writeShellApplication {
      name = "tritonserver-${name}";
      runtimeInputs = [ triton openmpi ];
      text = ''
        export LD_LIBRARY_PATH="/run/opengl-driver/lib:${triton}/lib:${triton}/tensorrt_llm/lib:${cuda}/lib64:${openmpi}/lib''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"

        ${if worldSize > 1 then ''
        # Multi-GPU: launch with mpirun
        exec mpirun -np ${toString worldSize} --allow-run-as-root \
          ${triton}/bin/tritonserver \
            --model-repository=${repo} \
            --http-port=${toString httpPort} \
            --grpc-port=${toString grpcPort} \
            --metrics-port=${toString metricsPort} \
            "$@"
        '' else ''
        exec ${triton}/bin/tritonserver \
          --model-repository=${repo} \
          --http-port=${toString httpPort} \
          --grpc-port=${toString grpcPort} \
          --metrics-port=${toString metricsPort} \
          "$@"
        ''}
      '';
      meta = {
        description = "Triton Inference Server for ${name}";
        mainProgram = "tritonserver-${name}";
      };
    };

  # ============================================================================
  # mkTritonServerRuntime: Triton server with full ensemble (preprocessing + tensorrt_llm + postprocessing)
  # ============================================================================
  # Use this when the engine is built manually (not via Nix derivation).
  # Looks for engine in $TRTLLM_ENGINE_PATH or ~/.cache/trtllm-engines/<name>/
  #
  # This creates the full ensemble setup that accepts text input and returns text output.
  mkTritonServerRuntime =
    {
      name,                    # e.g. "qwen3" - used to find engine in cache
      tokenizerModel,          # HuggingFace model ID for tokenizer (downloaded at runtime if needed)
      httpPort ? 8000,
      grpcPort ? 8001,
      metricsPort ? 8002,
      maxBatchSize ? 8,
      batchSchedulerPolicy ? "guaranteed_no_evict",
      kvCacheFreeGpuMemFraction ? 0.9,
      enableChunkedContext ? true,
      worldSize ? 1,
    }:
    let
      # Preprocessing Python model
      preprocessingModel = writeTextFile {
        name = "preprocessing-model.py";
        text = ''
import json
import numpy as np
import triton_python_backend_utils as pb_utils
from transformers import AutoTokenizer

class TritonPythonModel:
    def initialize(self, args):
        model_config = json.loads(args['model_config'])
        tokenizer_dir = model_config['parameters']['tokenizer_dir']['string_value']
        
        self.tokenizer = AutoTokenizer.from_pretrained(
            tokenizer_dir, legacy=False, padding_side='left', trust_remote_code=True
        )
        if not self.tokenizer.pad_token:
            self.tokenizer.pad_token = self.tokenizer.eos_token
        
        self.pad_id = self.tokenizer.pad_token_id or self.tokenizer.eos_token_id

    def execute(self, requests):
        responses = []
        for request in requests:
            query = pb_utils.get_input_tensor_by_name(request, 'QUERY').as_numpy()
            request_output_len = pb_utils.get_input_tensor_by_name(request, 'REQUEST_OUTPUT_LEN').as_numpy()
            
            # Tokenize
            prompts = [q[0].decode() if isinstance(q[0], bytes) else q[0] for q in query]
            encoded = self.tokenizer(prompts, return_tensors='np', padding=True)
            input_ids = encoded['input_ids'].astype(np.int32)
            input_lengths = np.array([[len(ids)] for ids in input_ids], dtype=np.int32)
            
            # Get end/pad IDs
            end_id_input = pb_utils.get_input_tensor_by_name(request, 'END_ID')
            pad_id_input = pb_utils.get_input_tensor_by_name(request, 'PAD_ID')
            
            if end_id_input is not None:
                end_id = end_id_input.as_numpy()
            else:
                end_id = np.array([[self.tokenizer.eos_token_id or 151643]] * len(query), dtype=np.int32)
            
            if pad_id_input is not None:
                pad_id = pad_id_input.as_numpy()
            else:
                pad_id = np.array([[self.pad_id]] * len(query), dtype=np.int32)
            
            responses.append(pb_utils.InferenceResponse(output_tensors=[
                pb_utils.Tensor('INPUT_ID', input_ids),
                pb_utils.Tensor('REQUEST_INPUT_LEN', input_lengths),
                pb_utils.Tensor('REQUEST_OUTPUT_LEN', request_output_len),
                pb_utils.Tensor('OUT_END_ID', end_id),
                pb_utils.Tensor('OUT_PAD_ID', pad_id),
            ]))
        return responses

    def finalize(self):
        pass
'';
      };

      # Postprocessing Python model
      postprocessingModel = writeTextFile {
        name = "postprocessing-model.py";
        text = ''
import json
import numpy as np
import triton_python_backend_utils as pb_utils
from transformers import AutoTokenizer

class TritonPythonModel:
    def initialize(self, args):
        model_config = json.loads(args['model_config'])
        tokenizer_dir = model_config['parameters']['tokenizer_dir']['string_value']
        
        self.tokenizer = AutoTokenizer.from_pretrained(
            tokenizer_dir, legacy=False, padding_side='left', trust_remote_code=True
        )

    def execute(self, requests):
        responses = []
        for request in requests:
            tokens_batch = pb_utils.get_input_tensor_by_name(request, 'TOKENS_BATCH').as_numpy()
            sequence_lengths = pb_utils.get_input_tensor_by_name(request, 'SEQUENCE_LENGTH').as_numpy()
            
            outputs = []
            for batch_idx, beam_tokens in enumerate(tokens_batch):
                for beam_idx, tokens in enumerate(beam_tokens):
                    seq_len = sequence_lengths[batch_idx][beam_idx]
                    text = self.tokenizer.decode(tokens[:seq_len], skip_special_tokens=True)
                    outputs.append(text.encode('utf-8'))
            
            responses.append(pb_utils.InferenceResponse(output_tensors=[
                pb_utils.Tensor('OUTPUT', np.array(outputs, dtype=object))
            ]))
        return responses

    def finalize(self):
        pass
'';
      };
    in
    writeShellApplication {
      name = "tritonserver-${name}";
      runtimeInputs = [ triton openmpi python ];
      text = ''
        CACHE_DIR="''${XDG_CACHE_HOME:-$HOME/.cache}"
        ENGINE_PATH="''${TRTLLM_ENGINE_PATH:-$CACHE_DIR/trtllm-engines/${name}}"
        
        if [[ ! -d "$ENGINE_PATH" ]] || [[ ! -f "$ENGINE_PATH/rank0.engine" ]]; then
          echo "ERROR: Engine not found at $ENGINE_PATH"
          echo ""
          echo "Build the engine first using:"
          echo "  nix develop -c bash"
          echo "  # Then follow docs/TRTLLM-ENGINES.md"
          echo ""
          echo "Or set TRTLLM_ENGINE_PATH to your engine directory"
          exit 1
        fi
        
        # Get tokenizer path - use local path or download
        # shellcheck disable=SC2193 # Pattern check for absolute path vs HF model ID  
        if [[ -n "''${TRTLLM_TOKENIZER_PATH:-}" ]]; then
          TOKENIZER_PATH="$TRTLLM_TOKENIZER_PATH"
        elif [[ "${tokenizerModel}" == /* && -d "${tokenizerModel}" ]]; then
          # Local path - use directly
          TOKENIZER_PATH="${tokenizerModel}"
          echo "Using local tokenizer: $TOKENIZER_PATH"
        else
          echo "Resolving tokenizer: ${tokenizerModel}..."
          export PYTHONPATH="${triton}/python''${PYTHONPATH:+:$PYTHONPATH}"
          TOKENIZER_PATH=$(${python}/bin/python -c "from huggingface_hub import snapshot_download; print(snapshot_download('${tokenizerModel}'))")
        fi
        
        if [[ ! -d "$TOKENIZER_PATH" ]]; then
          echo "ERROR: Tokenizer not found at $TOKENIZER_PATH"
          exit 1
        fi
        
        MODEL_REPO="''${XDG_RUNTIME_DIR:-/tmp}/triton-${name}-repo"
        rm -rf "$MODEL_REPO"
        
        # ══════════════════════════════════════════════════════════════════
        # Create preprocessing model (Python backend)
        # ══════════════════════════════════════════════════════════════════
        mkdir -p "$MODEL_REPO/preprocessing/1"
        cp ${preprocessingModel} "$MODEL_REPO/preprocessing/1/model.py"
        
        cat > "$MODEL_REPO/preprocessing/config.pbtxt" << 'PBTXT'
name: "preprocessing"
backend: "python"
max_batch_size: ${toString maxBatchSize}

input [
  { name: "QUERY", data_type: TYPE_STRING, dims: [ 1 ] },
  { name: "REQUEST_OUTPUT_LEN", data_type: TYPE_INT32, dims: [ 1 ] },
  { name: "END_ID", data_type: TYPE_INT32, dims: [ 1 ], optional: true },
  { name: "PAD_ID", data_type: TYPE_INT32, dims: [ 1 ], optional: true }
]

output [
  { name: "INPUT_ID", data_type: TYPE_INT32, dims: [ -1 ] },
  { name: "REQUEST_INPUT_LEN", data_type: TYPE_INT32, dims: [ 1 ] },
  { name: "REQUEST_OUTPUT_LEN", data_type: TYPE_INT32, dims: [ -1 ] },
  { name: "OUT_END_ID", data_type: TYPE_INT32, dims: [ 1 ] },
  { name: "OUT_PAD_ID", data_type: TYPE_INT32, dims: [ 1 ] }
]

parameters: { key: "tokenizer_dir", value: { string_value: "TOKENIZER_PLACEHOLDER" } }

instance_group [ { count: 1, kind: KIND_CPU } ]
PBTXT
        sed -i "s|TOKENIZER_PLACEHOLDER|$TOKENIZER_PATH|g" "$MODEL_REPO/preprocessing/config.pbtxt"
        
        # ══════════════════════════════════════════════════════════════════
        # Create tensorrt_llm model (TensorRT-LLM backend)
        # ══════════════════════════════════════════════════════════════════
        mkdir -p "$MODEL_REPO/tensorrt_llm/1"
        touch "$MODEL_REPO/tensorrt_llm/1/.keep"
        
        cat > "$MODEL_REPO/tensorrt_llm/config.pbtxt" << 'PBTXT'
name: "tensorrt_llm"
backend: "tensorrtllm"
max_batch_size: ${toString maxBatchSize}

model_transaction_policy { decoupled: true }

dynamic_batching {
  preferred_batch_size: [ 1, 2, 4 ]
  max_queue_delay_microseconds: 100
}

input [
  { name: "input_ids", data_type: TYPE_INT32, dims: [ -1 ], allow_ragged_batch: true },
  { name: "input_lengths", data_type: TYPE_INT32, dims: [ 1 ], reshape: { shape: [ ] } },
  { name: "request_output_len", data_type: TYPE_INT32, dims: [ 1 ], reshape: { shape: [ ] } },
  { name: "end_id", data_type: TYPE_INT32, dims: [ 1 ], reshape: { shape: [ ] }, optional: true },
  { name: "pad_id", data_type: TYPE_INT32, dims: [ 1 ], reshape: { shape: [ ] }, optional: true },
  { name: "streaming", data_type: TYPE_BOOL, dims: [ 1 ], reshape: { shape: [ ] }, optional: true },
  { name: "temperature", data_type: TYPE_FP32, dims: [ 1 ], reshape: { shape: [ ] }, optional: true },
  { name: "runtime_top_p", data_type: TYPE_FP32, dims: [ 1 ], reshape: { shape: [ ] }, optional: true },
  { name: "runtime_top_k", data_type: TYPE_INT32, dims: [ 1 ], reshape: { shape: [ ] }, optional: true }
]

output [
  { name: "output_ids", data_type: TYPE_INT32, dims: [ -1, -1 ] },
  { name: "sequence_length", data_type: TYPE_INT32, dims: [ -1 ] },
  { name: "cum_log_probs", data_type: TYPE_FP32, dims: [ -1 ] },
  { name: "output_log_probs", data_type: TYPE_FP32, dims: [ -1, -1 ] }
]

instance_group [ { count: 1, kind: KIND_CPU } ]

parameters: { key: "gpt_model_type", value: { string_value: "inflight_fused_batching" } }
parameters: { key: "gpt_model_path", value: { string_value: "ENGINE_PLACEHOLDER" } }
parameters: { key: "batch_scheduler_policy", value: { string_value: "${batchSchedulerPolicy}" } }
parameters: { key: "kv_cache_free_gpu_mem_fraction", value: { string_value: "${toString kvCacheFreeGpuMemFraction}" } }
${lib.optionalString enableChunkedContext ''parameters: { key: "enable_chunked_context", value: { string_value: "true" } }''}
parameters: { key: "decoding_mode", value: { string_value: "top_k_top_p" } }
parameters: { key: "executor_worker_path", value: { string_value: "${triton}/backends/tensorrtllm/trtllmExecutorWorker" } }
parameters: { key: "tokenizer_dir", value: { string_value: "TOKENIZER_PLACEHOLDER" } }
parameters: { key: "guided_decoding_backend", value: { string_value: "" } }
parameters: { key: "xgrammar_tokenizer_info_path", value: { string_value: "" } }
PBTXT
        sed -i "s|ENGINE_PLACEHOLDER|$ENGINE_PATH|g" "$MODEL_REPO/tensorrt_llm/config.pbtxt"
        sed -i "s|TOKENIZER_PLACEHOLDER|$TOKENIZER_PATH|g" "$MODEL_REPO/tensorrt_llm/config.pbtxt"
        
        # ══════════════════════════════════════════════════════════════════
        # Create postprocessing model (Python backend)
        # ══════════════════════════════════════════════════════════════════
        mkdir -p "$MODEL_REPO/postprocessing/1"
        cp ${postprocessingModel} "$MODEL_REPO/postprocessing/1/model.py"
        
        cat > "$MODEL_REPO/postprocessing/config.pbtxt" << 'PBTXT'
name: "postprocessing"
backend: "python"
max_batch_size: ${toString maxBatchSize}

input [
  { name: "TOKENS_BATCH", data_type: TYPE_INT32, dims: [ -1, -1 ] },
  { name: "SEQUENCE_LENGTH", data_type: TYPE_INT32, dims: [ -1 ] }
]

output [
  { name: "OUTPUT", data_type: TYPE_STRING, dims: [ -1 ] }
]

parameters: { key: "tokenizer_dir", value: { string_value: "TOKENIZER_PLACEHOLDER" } }

instance_group [ { count: 1, kind: KIND_CPU } ]
PBTXT
        sed -i "s|TOKENIZER_PLACEHOLDER|$TOKENIZER_PATH|g" "$MODEL_REPO/postprocessing/config.pbtxt"
        
        # ══════════════════════════════════════════════════════════════════
        # Create ensemble model
        # ══════════════════════════════════════════════════════════════════
        mkdir -p "$MODEL_REPO/ensemble/1"
        
        cat > "$MODEL_REPO/ensemble/config.pbtxt" << 'PBTXT'
name: "ensemble"
platform: "ensemble"
max_batch_size: ${toString maxBatchSize}

input [
  { name: "text_input", data_type: TYPE_STRING, dims: [ 1 ] },
  { name: "max_tokens", data_type: TYPE_INT32, dims: [ 1 ] },
  { name: "end_id", data_type: TYPE_INT32, dims: [ 1 ], optional: true },
  { name: "pad_id", data_type: TYPE_INT32, dims: [ 1 ], optional: true },
  { name: "top_k", data_type: TYPE_INT32, dims: [ 1 ], optional: true },
  { name: "top_p", data_type: TYPE_FP32, dims: [ 1 ], optional: true },
  { name: "temperature", data_type: TYPE_FP32, dims: [ 1 ], optional: true },
  { name: "stream", data_type: TYPE_BOOL, dims: [ 1 ], optional: true }
]

output [
  { name: "text_output", data_type: TYPE_STRING, dims: [ -1 ] }
]

ensemble_scheduling {
  step [
    {
      model_name: "preprocessing"
      model_version: -1
      input_map { key: "QUERY", value: "text_input" }
      input_map { key: "REQUEST_OUTPUT_LEN", value: "max_tokens" }
      input_map { key: "END_ID", value: "end_id" }
      input_map { key: "PAD_ID", value: "pad_id" }
      output_map { key: "INPUT_ID", value: "_INPUT_ID" }
      output_map { key: "REQUEST_INPUT_LEN", value: "_REQUEST_INPUT_LEN" }
      output_map { key: "REQUEST_OUTPUT_LEN", value: "_REQUEST_OUTPUT_LEN" }
      output_map { key: "OUT_END_ID", value: "_END_ID" }
      output_map { key: "OUT_PAD_ID", value: "_PAD_ID" }
    },
    {
      model_name: "tensorrt_llm"
      model_version: -1
      input_map { key: "input_ids", value: "_INPUT_ID" }
      input_map { key: "input_lengths", value: "_REQUEST_INPUT_LEN" }
      input_map { key: "request_output_len", value: "_REQUEST_OUTPUT_LEN" }
      input_map { key: "end_id", value: "_END_ID" }
      input_map { key: "pad_id", value: "_PAD_ID" }
      input_map { key: "runtime_top_k", value: "top_k" }
      input_map { key: "runtime_top_p", value: "top_p" }
      input_map { key: "temperature", value: "temperature" }
      input_map { key: "streaming", value: "stream" }
      output_map { key: "output_ids", value: "_TOKENS_BATCH" }
      output_map { key: "sequence_length", value: "_SEQUENCE_LENGTH" }
    },
    {
      model_name: "postprocessing"
      model_version: -1
      input_map { key: "TOKENS_BATCH", value: "_TOKENS_BATCH" }
      input_map { key: "SEQUENCE_LENGTH", value: "_SEQUENCE_LENGTH" }
      output_map { key: "OUTPUT", value: "text_output" }
    }
  ]
}
PBTXT
        
        echo "════════════════════════════════════════════════════════════════"
        echo "Triton TensorRT-LLM Ensemble: ${name}"
        echo "════════════════════════════════════════════════════════════════"
        echo "Engine:    $ENGINE_PATH"
        echo "Tokenizer: $TOKENIZER_PATH"
        echo ""
        echo "Models:"
        echo "  - preprocessing  (tokenization)"
        echo "  - tensorrt_llm   (inference)"
        echo "  - postprocessing (detokenization)"
        echo "  - ensemble       (chained pipeline)"
        echo ""
        echo "Endpoints:"
        echo "  HTTP:    http://localhost:${toString httpPort}"
        echo "  GRPC:    localhost:${toString grpcPort}"
        echo "  Metrics: http://localhost:${toString metricsPort}/metrics"
        echo ""
        echo "Usage:"
        echo "  curl localhost:${toString httpPort}/v2/models/ensemble/generate \\"
        echo "    -d '{\"text_input\": \"Hello\", \"max_tokens\": 50}'"
        echo ""
        
        export LD_LIBRARY_PATH="/run/opengl-driver/lib:${triton}/lib:${triton}/tensorrt_llm/lib:${cuda}/lib64:${openmpi}/lib''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"

        ${if worldSize > 1 then ''
        # NCCL configuration for multi-GPU (PCIe topology)
        export NCCL_DEBUG=''${NCCL_DEBUG:-WARN}
        export NCCL_IB_DISABLE=1          # No InfiniBand
        export NCCL_P2P_LEVEL=PHB         # PCIe peer-to-peer via host bridge
        export NCCL_SHM_DISABLE=0         # Enable shared memory
        
        # Set CUDA devices for all GPUs
        export CUDA_VISIBLE_DEVICES=''${CUDA_VISIBLE_DEVICES:-$(seq -s, 0 $((${toString worldSize} - 1)))}
        
        echo "Multi-GPU mode: ${toString worldSize} GPUs"
        echo "CUDA_VISIBLE_DEVICES: $CUDA_VISIBLE_DEVICES"
        echo ""
        
        exec mpirun -np ${toString worldSize} --allow-run-as-root \
          -x LD_LIBRARY_PATH -x NCCL_DEBUG -x NCCL_IB_DISABLE -x NCCL_P2P_LEVEL -x NCCL_SHM_DISABLE -x CUDA_VISIBLE_DEVICES \
          ${triton}/bin/tritonserver \
            --model-repository="$MODEL_REPO" \
            --http-port=${toString httpPort} \
            --grpc-port=${toString grpcPort} \
            --metrics-port=${toString metricsPort} \
            "$@"
        '' else ''
        exec ${triton}/bin/tritonserver \
          --model-repository="$MODEL_REPO" \
          --http-port=${toString httpPort} \
          --grpc-port=${toString grpcPort} \
          --metrics-port=${toString metricsPort} \
          "$@"
        ''}
      '';
      meta = {
        description = "Triton Inference Server for ${name} with full ensemble (runtime config, worldSize=${toString worldSize})";
        mainProgram = "tritonserver-${name}";
      };
    };

  # ============================================================================
  # Convenience: buildNVFP4 - build engine for pre-quantized NVFP4 models
  # ============================================================================
  # Use this for nvidia/* NVFP4 models like nvidia/Qwen3-32B-NVFP4
  buildNVFP4 =
    {
      name,
      hfModel,
      tensorParallelSize ? 1,
      pipelineParallelSize ? 1,
      maxBatchSize ? 8,
      maxSeqLen ? 16384,
      maxNumTokens ? 8192,
    }:
    let
      engine = mkEngine {
        inherit name hfModel tensorParallelSize pipelineParallelSize
                maxBatchSize maxSeqLen maxNumTokens;
        quantization = "NVFP4";
        dtype = "auto";
      };
      tritonRepo = mkTritonRepo {
        inherit name engine maxBatchSize;
        tokenizer = hfModel;
      };
    in
    {
      inherit engine tritonRepo;
      server = mkTritonServer {
        inherit name;
        repo = tritonRepo;
        worldSize = tensorParallelSize * pipelineParallelSize;
      };
    };
}
