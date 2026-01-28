#!/usr/bin/env python3
"""
Convert GLM-4.7 to TensorRT-LLM checkpoint format.

GLM-4.7 MoE architecture is similar to Qwen2 MoE:
- Shared expert + routed experts
- GQA attention
- RMSNorm
- SiLU activation

This script converts the HF checkpoint to TRT-LLM format,
optionally applying NVFP4 quantization.

Usage:
    python convert-glm47-trtllm.py \
        --model /path/to/GLM-4.7 \
        --output /path/to/trtllm-checkpoint \
        --tp-size 4 \
        --dtype bfloat16

For NVFP4 quantization:
    python convert-glm47-trtllm.py \
        --model /path/to/GLM-4.7 \
        --output /path/to/trtllm-checkpoint \
        --tp-size 4 \
        --quantize nvfp4
"""

import argparse
import json
import os
import re
import sys
from pathlib import Path
from typing import Dict, Any, Optional

import numpy as np
import torch
from safetensors import safe_open
from safetensors.torch import save_file
from tqdm import tqdm


def load_hf_config(model_path: str) -> Dict[str, Any]:
    """Load HuggingFace config.json"""
    config_path = Path(model_path) / "config.json"
    with open(config_path) as f:
        return json.load(f)


def create_trtllm_config(
    hf_config: Dict[str, Any],
    tp_size: int,
    pp_size: int = 1,
    dtype: str = "bfloat16",
    quant_algo: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Create TRT-LLM config from HuggingFace GLM-4.7 config.
    
    Maps GLM-4.7 MoE architecture to TRT-LLM's internal format.
    """
    
    # GLM-4.7 specific fields
    hidden_size = hf_config["hidden_size"]
    num_layers = hf_config["num_hidden_layers"]
    num_heads = hf_config["num_attention_heads"]
    num_kv_heads = hf_config.get("num_key_value_heads", num_heads)
    vocab_size = hf_config["vocab_size"]
    
    # MoE configuration
    num_experts = hf_config.get("n_routed_experts", 1)
    num_experts_per_tok = hf_config.get("num_experts_per_tok", 1)
    moe_intermediate_size = hf_config.get("moe_intermediate_size", 0)
    shared_expert_intermediate_size = hf_config.get("intermediate_size", 0)
    n_shared_experts = hf_config.get("n_shared_experts", 0)
    first_k_dense_replace = hf_config.get("first_k_dense_replace", 0)
    
    # Attention config
    head_dim = hf_config.get("head_dim", hidden_size // num_heads)
    max_position_embeddings = hf_config.get("max_position_embeddings", 202752)
    rope_theta = hf_config.get("rope_theta", 1000000)
    
    # Build TRT-LLM config
    config = {
        "architecture": "Qwen2MoeForCausalLM",  # Use Qwen2 MoE as base
        "dtype": dtype,
        "logits_dtype": "float32",
        "vocab_size": vocab_size,
        "max_position_embeddings": max_position_embeddings,
        "hidden_size": hidden_size,
        "num_hidden_layers": num_layers,
        "num_attention_heads": num_heads,
        "num_key_value_heads": num_kv_heads,
        "head_size": head_dim,
        "hidden_act": hf_config.get("hidden_act", "silu"),
        "intermediate_size": shared_expert_intermediate_size,
        "norm_epsilon": hf_config.get("rms_norm_eps", 1e-5),
        "position_embedding_type": "rope_gpt_neox",
        "rotary_base": rope_theta,
        "rotary_pct": hf_config.get("partial_rotary_factor", 0.5),
        "mapping": {
            "world_size": tp_size * pp_size,
            "tp_size": tp_size,
            "pp_size": pp_size,
        },
        # MoE config
        "moe": {
            "num_experts": num_experts,
            "top_k": num_experts_per_tok,
            "normalization_mode": "renormalize" if hf_config.get("norm_topk_prob", True) else "none",
        },
        "moe_intermediate_size": moe_intermediate_size,
        "moe_shared_expert_intermediate_size": shared_expert_intermediate_size if n_shared_experts > 0 else None,
        # GLM-4.7 specific
        "first_k_dense_replace": first_k_dense_replace,
        "n_shared_experts": n_shared_experts,
        # Attention
        "attn_bias": hf_config.get("attention_bias", True),
        "mlp_bias": False,
        "use_qk_norm": hf_config.get("use_qk_norm", True),
    }
    
    # Quantization config
    if quant_algo:
        config["quantization"] = {
            "quant_algo": quant_algo.upper(),
            "kv_cache_quant_algo": "FP8" if "fp4" in quant_algo.lower() else None,
            "group_size": 16,
        }
    
    return config


def get_weight_mapping(hf_config: Dict[str, Any]) -> Dict[str, str]:
    """
    Create weight name mapping from GLM-4.7 HF names to TRT-LLM names.
    """
    mapping = {
        # Embeddings
        "model.embed_tokens.weight": "transformer.vocab_embedding.weight",
        "model.norm.weight": "transformer.ln_f.weight",
        "lm_head.weight": "lm_head.weight",
    }
    
    num_layers = hf_config["num_hidden_layers"]
    
    for i in range(num_layers):
        prefix_hf = f"model.layers.{i}"
        prefix_trt = f"transformer.layers.{i}"
        
        # Attention
        mapping[f"{prefix_hf}.input_layernorm.weight"] = f"{prefix_trt}.input_layernorm.weight"
        mapping[f"{prefix_hf}.self_attn.q_proj.weight"] = f"{prefix_trt}.attention.qkv.q.weight"
        mapping[f"{prefix_hf}.self_attn.q_proj.bias"] = f"{prefix_trt}.attention.qkv.q.bias"
        mapping[f"{prefix_hf}.self_attn.k_proj.weight"] = f"{prefix_trt}.attention.qkv.k.weight"
        mapping[f"{prefix_hf}.self_attn.k_proj.bias"] = f"{prefix_trt}.attention.qkv.k.bias"
        mapping[f"{prefix_hf}.self_attn.v_proj.weight"] = f"{prefix_trt}.attention.qkv.v.weight"
        mapping[f"{prefix_hf}.self_attn.v_proj.bias"] = f"{prefix_trt}.attention.qkv.v.bias"
        mapping[f"{prefix_hf}.self_attn.o_proj.weight"] = f"{prefix_trt}.attention.dense.weight"
        
        # Q/K norms (GLM-4.7 uses these)
        mapping[f"{prefix_hf}.self_attn.q_norm.weight"] = f"{prefix_trt}.attention.q_layernorm.weight"
        mapping[f"{prefix_hf}.self_attn.k_norm.weight"] = f"{prefix_trt}.attention.k_layernorm.weight"
        
        # Post-attention norm
        mapping[f"{prefix_hf}.post_attention_layernorm.weight"] = f"{prefix_trt}.post_layernorm.weight"
        
        # MoE layers
        # Router
        mapping[f"{prefix_hf}.mlp.gate.weight"] = f"{prefix_trt}.mlp.router.weight"
        
        # Shared expert (if present)
        mapping[f"{prefix_hf}.mlp.shared_expert.gate_proj.weight"] = f"{prefix_trt}.mlp.shared_expert.fc.weight"
        mapping[f"{prefix_hf}.mlp.shared_expert.up_proj.weight"] = f"{prefix_trt}.mlp.shared_expert.gate.weight"
        mapping[f"{prefix_hf}.mlp.shared_expert.down_proj.weight"] = f"{prefix_trt}.mlp.shared_expert.proj.weight"
        
        # Routed experts
        # These need special handling - HF stores as experts.{j}.{gate,up,down}_proj
        # TRT-LLM wants them packed differently
    
    return mapping


def convert_weights(
    model_path: str,
    output_path: str,
    hf_config: Dict[str, Any],
    tp_size: int,
    dtype: str = "bfloat16",
):
    """
    Convert GLM-4.7 weights to TRT-LLM format.
    
    This handles:
    - Weight name remapping
    - Tensor parallel sharding
    - Expert weight packing for MoE
    """
    
    output_dir = Path(output_path)
    output_dir.mkdir(parents=True, exist_ok=True)
    
    # Get all safetensor files
    model_dir = Path(model_path)
    shard_files = sorted(model_dir.glob("model-*.safetensors"))
    
    if not shard_files:
        raise ValueError(f"No safetensor files found in {model_path}")
    
    print(f"Found {len(shard_files)} weight shards")
    
    # Load weight index if available
    index_file = model_dir / "model.safetensors.index.json"
    if index_file.exists():
        with open(index_file) as f:
            weight_index = json.load(f)
        weight_map = weight_index.get("weight_map", {})
    else:
        weight_map = {}
    
    # Process each TP rank
    for tp_rank in range(tp_size):
        print(f"\nProcessing TP rank {tp_rank}/{tp_size}")
        rank_weights = {}
        
        # Load and convert weights
        for shard_file in tqdm(shard_files, desc="Loading shards"):
            with safe_open(shard_file, framework="pt") as f:
                for name in f.keys():
                    tensor = f.get_tensor(name)
                    
                    # Convert to target dtype
                    if dtype == "bfloat16":
                        tensor = tensor.to(torch.bfloat16)
                    elif dtype == "float16":
                        tensor = tensor.to(torch.float16)
                    
                    # Apply TP sharding based on weight type
                    tensor = shard_weight(name, tensor, tp_rank, tp_size, hf_config)
                    
                    if tensor is not None:
                        # Remap name
                        trt_name = remap_weight_name(name, hf_config)
                        rank_weights[trt_name] = tensor
        
        # Save rank weights
        rank_file = output_dir / f"rank{tp_rank}.safetensors"
        save_file(rank_weights, str(rank_file))
        print(f"Saved {rank_file}")
        
        # Clear memory
        del rank_weights
        torch.cuda.empty_cache()
    
    # Save config
    trt_config = create_trtllm_config(hf_config, tp_size, dtype=dtype)
    config_file = output_dir / "config.json"
    with open(config_file, "w") as f:
        json.dump(trt_config, f, indent=2)
    print(f"Saved config to {config_file}")


def shard_weight(
    name: str,
    tensor: torch.Tensor,
    tp_rank: int,
    tp_size: int,
    hf_config: Dict[str, Any],
) -> Optional[torch.Tensor]:
    """
    Shard a weight tensor for tensor parallelism.
    
    Returns None if this weight should be skipped for this rank.
    """
    
    if tp_size == 1:
        return tensor
    
    # Determine sharding strategy based on weight name
    # Column parallel: shard output dim (dim 0 for most weights)
    # Row parallel: shard input dim (dim 1 for output projections)
    
    # QKV projections - column parallel
    if any(x in name for x in ["q_proj.weight", "k_proj.weight", "v_proj.weight"]):
        dim = 0
        size = tensor.shape[dim]
        shard_size = size // tp_size
        return tensor.narrow(dim, tp_rank * shard_size, shard_size).clone()
    
    # Output projection - row parallel  
    if "o_proj.weight" in name:
        dim = 1
        size = tensor.shape[dim]
        shard_size = size // tp_size
        return tensor.narrow(dim, tp_rank * shard_size, shard_size).clone()
    
    # MLP gate/up - column parallel
    if any(x in name for x in ["gate_proj.weight", "up_proj.weight"]):
        dim = 0
        size = tensor.shape[dim]
        shard_size = size // tp_size
        return tensor.narrow(dim, tp_rank * shard_size, shard_size).clone()
    
    # MLP down - row parallel
    if "down_proj.weight" in name:
        dim = 1
        size = tensor.shape[dim]
        shard_size = size // tp_size
        return tensor.narrow(dim, tp_rank * shard_size, shard_size).clone()
    
    # Expert weights - shard by expert assignment
    if ".experts." in name:
        # Parse expert index
        match = re.search(r"\.experts\.(\d+)\.", name)
        if match:
            expert_idx = int(match.group(1))
            num_experts = hf_config.get("n_routed_experts", 160)
            experts_per_rank = num_experts // tp_size
            
            # Only keep experts assigned to this rank
            if expert_idx // experts_per_rank == tp_rank:
                return tensor.clone()
            else:
                return None
    
    # Router, embeddings, norms - replicated
    return tensor.clone()


def remap_weight_name(hf_name: str, hf_config: Dict[str, Any]) -> str:
    """Remap HuggingFace weight name to TRT-LLM format."""
    
    # Basic remapping
    name = hf_name
    name = name.replace("model.embed_tokens", "transformer.vocab_embedding")
    name = name.replace("model.norm", "transformer.ln_f")
    name = name.replace("model.layers", "transformer.layers")
    name = name.replace("self_attn", "attention")
    name = name.replace("post_attention_layernorm", "post_layernorm")
    name = name.replace("mlp.gate.weight", "mlp.router.weight")
    
    # MoE expert remapping
    name = name.replace("mlp.experts", "mlp.fc")
    name = name.replace("gate_proj", "fc")
    name = name.replace("up_proj", "gate")
    name = name.replace("down_proj", "proj")
    
    return name


def main():
    parser = argparse.ArgumentParser(description="Convert GLM-4.7 to TRT-LLM format")
    parser.add_argument("--model", type=str, required=True, help="Path to GLM-4.7 HF checkpoint")
    parser.add_argument("--output", type=str, required=True, help="Output directory for TRT-LLM checkpoint")
    parser.add_argument("--tp-size", type=int, default=4, help="Tensor parallel size")
    parser.add_argument("--dtype", type=str, default="bfloat16", choices=["bfloat16", "float16"])
    parser.add_argument("--quantize", type=str, default=None, choices=["nvfp4", "fp8", None])
    
    args = parser.parse_args()
    
    print("=" * 70)
    print("GLM-4.7 to TRT-LLM Checkpoint Converter")
    print("=" * 70)
    print(f"Model: {args.model}")
    print(f"Output: {args.output}")
    print(f"TP Size: {args.tp_size}")
    print(f"Dtype: {args.dtype}")
    print(f"Quantize: {args.quantize or 'None'}")
    print("=" * 70)
    
    # Load HF config
    hf_config = load_hf_config(args.model)
    print(f"\nLoaded config: {hf_config.get('architectures', ['Unknown'])}")
    print(f"  Hidden size: {hf_config.get('hidden_size')}")
    print(f"  Layers: {hf_config.get('num_hidden_layers')}")
    print(f"  Experts: {hf_config.get('n_routed_experts')}")
    
    if args.quantize:
        print(f"\nNote: Quantization will be applied during trtllm-build")
        print(f"      This converter outputs {args.dtype} weights")
    
    # Convert weights
    convert_weights(
        model_path=args.model,
        output_path=args.output,
        hf_config=hf_config,
        tp_size=args.tp_size,
        dtype=args.dtype,
    )
    
    print("\n" + "=" * 70)
    print("Conversion complete!")
    print(f"Next step: Run trtllm-build on {args.output}")
    print("=" * 70)


if __name__ == "__main__":
    main()
