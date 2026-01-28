# NVIDIA NVFP4 Pre-quantized Models
#
# These models are pre-quantized by NVIDIA using ModelOpt and work with TensorRT-LLM.
# NVFP4 = 4-bit floating point quantization with FP8 KV cache
#
# VRAM estimates are for inference with reasonable batch size and context.
# Actual usage depends on batch size, sequence length, and KV cache.

{
  # ════════════════════════════════════════════════════════════════════════════
  # Qwen3 Family
  # ════════════════════════════════════════════════════════════════════════════

  qwen3-8b = {
    hfModel = "nvidia/Qwen3-8B-NVFP4";
    params = "8B";
    vramMin = "8GB";      # Single GPU minimum
    recommended = {
      tp1 = "RTX 4090, RTX 5090, A100-40GB";
      tp2 = null;
    };
    context = 32768;
    notes = "Fast, good for simple tasks";
  };

  qwen3-14b = {
    hfModel = "nvidia/Qwen3-14B-NVFP4";
    params = "14B";
    vramMin = "12GB";
    recommended = {
      tp1 = "RTX 4090, RTX 5090, A100-40GB, RTX PRO 6000";
      tp2 = "2x RTX 4080";
    };
    context = 32768;
    notes = "Good balance of speed and quality";
  };

  qwen3-32b = {
    hfModel = "nvidia/Qwen3-32B-NVFP4";
    params = "32B";
    vramMin = "24GB";
    recommended = {
      tp1 = "RTX PRO 6000 (96GB), A100-80GB, H100";
      tp2 = "2x RTX 4090, 2x A100-40GB";
      tp4 = "4x RTX 4090, 4x RTX PRO 6000";
    };
    context = 32768;
    notes = "Reasoning model with <think> tags. Our primary test model.";
  };

  qwen3-30b-a3b = {
    hfModel = "nvidia/Qwen3-30B-A3B-NVFP4";
    params = "30B (3B active MoE)";
    vramMin = "20GB";
    recommended = {
      tp1 = "RTX 4090, A100-40GB";
    };
    context = 32768;
    notes = "Mixture of Experts - only 3B params active per token";
  };

  qwen3-235b-a22b = {
    hfModel = "nvidia/Qwen3-235B-A22B-NVFP4";
    params = "235B (22B active MoE)";
    vramMin = "160GB";
    recommended = {
      tp4 = "4x A100-80GB, 4x H100";
      tp8 = "8x A100-40GB";
    };
    context = 32768;
    notes = "Largest Qwen3 MoE model";
  };

  # ════════════════════════════════════════════════════════════════════════════
  # Llama Family  
  # ════════════════════════════════════════════════════════════════════════════

  llama-3_1-8b = {
    hfModel = "nvidia/Llama-3.1-8B-Instruct-NVFP4";
    params = "8B";
    vramMin = "8GB";
    recommended = {
      tp1 = "RTX 4090, RTX 5090, A100-40GB";
    };
    context = 131072;  # 128K context
    notes = "Long context Llama";
  };

  llama-3_3-70b = {
    hfModel = "nvidia/Llama-3.3-70B-Instruct-NVFP4";
    params = "70B";
    vramMin = "48GB";
    recommended = {
      tp1 = "H100-80GB, A100-80GB";
      tp2 = "2x RTX PRO 6000 (96GB), 2x A100-40GB";
      tp4 = "4x RTX 4090, 4x A100-40GB";
    };
    context = 131072;
    notes = "Best open Llama model";
  };

  llama-3_1-405b = {
    hfModel = "nvidia/Llama-3.1-405B-Instruct-NVFP4";
    params = "405B";
    vramMin = "280GB";
    recommended = {
      tp8 = "8x H100-80GB, 8x A100-80GB";
    };
    context = 131072;
    notes = "Largest Llama - requires serious hardware";
  };

  llama-4-scout-17b = {
    hfModel = "nvidia/Llama-4-Scout-17B-16E-Instruct-NVFP4";
    params = "17B (16 experts MoE)";
    vramMin = "16GB";
    recommended = {
      tp1 = "RTX 4090, A100-40GB";
    };
    context = 131072;
    notes = "Llama 4 MoE variant";
  };

  # ════════════════════════════════════════════════════════════════════════════
  # DeepSeek Family
  # ════════════════════════════════════════════════════════════════════════════

  deepseek-r1 = {
    hfModel = "nvidia/DeepSeek-R1-NVFP4";
    params = "671B MoE";
    vramMin = "400GB";
    recommended = {
      tp8 = "8x H100-80GB";
      # B200 with 192GB would need fewer GPUs
    };
    context = 65536;
    notes = "Reasoning model. 80 safetensor shards. Needs multi-node for most setups.";
  };

  deepseek-v3 = {
    hfModel = "nvidia/DeepSeek-V3-0324-NVFP4";
    params = "671B MoE";
    vramMin = "400GB";
    recommended = {
      tp8 = "8x H100-80GB";
    };
    context = 65536;
    notes = "V3 base model";
  };

  deepseek-v3_1 = {
    hfModel = "nvidia/DeepSeek-V3.1-NVFP4";
    params = "671B MoE";
    vramMin = "400GB";
    recommended = {
      tp8 = "8x H100-80GB";
    };
    context = 65536;
    notes = "V3.1 update";
  };

  deepseek-v3_2 = {
    hfModel = "nvidia/DeepSeek-V3.2-NVFP4";
    params = "671B MoE";
    vramMin = "400GB";
    recommended = {
      tp8 = "8x H100-80GB";
    };
    context = 65536;
    notes = "Latest V3.2";
  };

  # ════════════════════════════════════════════════════════════════════════════
  # Nemotron (NVIDIA's own models)
  # ════════════════════════════════════════════════════════════════════════════

  nemotron-nano-9b = {
    hfModel = "nvidia/NVIDIA-Nemotron-Nano-9B-v2-NVFP4";
    params = "9B";
    vramMin = "8GB";
    recommended = {
      tp1 = "RTX 4090, RTX 5090";
    };
    context = 32768;
    notes = "NVIDIA's efficient small model";
  };

  nemotron-super-49b = {
    hfModel = "nvidia/Llama-3_3-Nemotron-Super-49B-v1_5-NVFP4";
    params = "49B";
    vramMin = "32GB";
    recommended = {
      tp1 = "A100-80GB, H100";
      tp2 = "2x RTX PRO 6000, 2x A100-40GB";
    };
    context = 131072;
    notes = "Llama-based Nemotron, very capable";
  };

  # ════════════════════════════════════════════════════════════════════════════
  # Phi (Microsoft via NVIDIA)
  # ════════════════════════════════════════════════════════════════════════════

  phi-4-reasoning = {
    hfModel = "nvidia/Phi-4-reasoning-plus-NVFP4";
    params = "14B";
    vramMin = "12GB";
    recommended = {
      tp1 = "RTX 4090, A100-40GB";
    };
    context = 16384;
    notes = "Microsoft Phi-4 with reasoning";
  };

  phi-4-multimodal = {
    hfModel = "nvidia/Phi-4-multimodal-instruct-NVFP4";
    params = "14B";
    vramMin = "12GB";
    recommended = {
      tp1 = "RTX 4090, A100-40GB";
    };
    context = 16384;
    notes = "Vision + language";
  };

  # ════════════════════════════════════════════════════════════════════════════
  # Vision-Language Models
  # ════════════════════════════════════════════════════════════════════════════

  qwen2_5-vl-7b = {
    hfModel = "nvidia/Qwen2.5-VL-7B-Instruct-NVFP4";
    params = "7B";
    vramMin = "8GB";
    recommended = {
      tp1 = "RTX 4090, RTX 5090";
    };
    context = 32768;
    notes = "Vision-language model";
  };

  nemotron-nano-12b-vl = {
    hfModel = "nvidia/NVIDIA-Nemotron-Nano-12B-v2-VL-NVFP4-QAD";
    params = "12B";
    vramMin = "10GB";
    recommended = {
      tp1 = "RTX 4090, A100-40GB";
    };
    context = 32768;
    notes = "NVIDIA vision-language model";
  };

  # ════════════════════════════════════════════════════════════════════════════
  # Kimi (Moonshot AI via NVIDIA)
  # ════════════════════════════════════════════════════════════════════════════

  kimi-k2-thinking = {
    hfModel = "nvidia/Kimi-K2-Thinking-NVFP4";
    params = "1T MoE (32B active)";
    vramMin = "600GB";
    recommended = {
      tp8 = "8x H100-80GB (tight), 8x B200";
    };
    context = 131072;
    notes = "Massive 1T param MoE reasoning model. B200 recommended.";
  };
}
