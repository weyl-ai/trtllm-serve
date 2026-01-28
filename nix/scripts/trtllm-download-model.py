#!/usr/bin/env python3
"""Download HuggingFace model for TensorRT-LLM engine building.

Usage:
    trtllm-download-model.py <model-id> <output-dir>
"""

import os
import sys
import argparse


def main():
    parser = argparse.ArgumentParser(description="Download HuggingFace model")
    parser.add_argument("model_id", help="HuggingFace model ID (e.g., nvidia/Qwen3-32B-NVFP4)")
    parser.add_argument("output_dir", help="Directory to download model to")
    args = parser.parse_args()

    try:
        from huggingface_hub import snapshot_download
    except ImportError as e:
        print(f"ERROR: Failed to import huggingface_hub: {e}", file=sys.stderr)
        sys.exit(1)

    try:
        print(f"Downloading {args.model_id} to {args.output_dir}...")
        local_dir = snapshot_download(args.model_id, local_dir=args.output_dir)
        print(f"Model downloaded to: {local_dir}")
        print("Files:", os.listdir(local_dir))
    except Exception as e:
        print(f"ERROR: Failed to download model: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
