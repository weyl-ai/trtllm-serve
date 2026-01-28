#!/usr/bin/env python3
"""Build TensorRT-LLM engine using the LLM API.

Usage:
    trtllm-build-engine.py <model-dir> <engine-dir> --tp=<n> [--cuda-devices=0,1,2,3]
"""

import os
import sys
import argparse


def main():
    parser = argparse.ArgumentParser(description="Build TRT-LLM engine")
    parser.add_argument("model_dir", help="Path to HuggingFace model")
    parser.add_argument("engine_dir", help="Path to output engine directory")
    parser.add_argument("--tp", type=int, default=1, help="Tensor parallel size")
    parser.add_argument("--pp", type=int, default=1, help="Pipeline parallel size")
    parser.add_argument("--cuda-devices", default="0", help="CUDA_VISIBLE_DEVICES")
    args = parser.parse_args()

    os.environ["CUDA_VISIBLE_DEVICES"] = args.cuda_devices

    from tensorrt_llm import LLM

    print(f"Building engine from: {args.model_dir}")
    print(f"  TP: {args.tp}, PP: {args.pp}")
    sys.stdout.flush()

    # PyTorch backend LLM API - auto-detects NVFP4 from hf_quant_config.json
    # Just pass the model path and TP size, like nvidia docs show
    print("Creating LLM with PyTorch backend (auto-config)...")
    sys.stdout.flush()

    llm = LLM(
        model=args.model_dir,
        tensor_parallel_size=args.tp,
        pipeline_parallel_size=args.pp,
    )

    # Save the engine
    print(f"\nSaving engine to: {args.engine_dir}")
    sys.stdout.flush()
    llm.save(args.engine_dir)

    print("\nEngine build complete!")
    print("Files:", os.listdir(args.engine_dir))
    sys.stdout.flush()


if __name__ == "__main__":
    main()
