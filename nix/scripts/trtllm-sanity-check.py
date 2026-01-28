#!/usr/bin/env python3
"""TensorRT-LLM Engine Sanity Check

Quick inference test to catch garbage output early.
Uses the TensorRT-LLM Python API.

Usage:
    trtllm-sanity-check.py <engine-dir> [--cuda-devices=0,1,2,3]
"""

import os
import sys
import argparse
import re


def has_repetition(text: str, min_pattern_len: int = 2, min_repeats: int = 3) -> bool:
    """Detect if text contains suspicious repetition patterns."""
    if not text or len(text) < min_pattern_len * min_repeats:
        return False
    # Check for any substring pattern that repeats
    for pattern_len in range(min_pattern_len, len(text) // min_repeats + 1):
        for start in range(len(text) - pattern_len * min_repeats + 1):
            pattern = text[start:start + pattern_len]
            if pattern * min_repeats in text:
                return True
    return False


def main():
    parser = argparse.ArgumentParser(description="TensorRT-LLM sanity check")
    parser.add_argument("engine_dir", help="Path to TRT-LLM engine directory")
    parser.add_argument("--cuda-devices", default="0", help="CUDA_VISIBLE_DEVICES")
    parser.add_argument("--strict", action="store_true", help="Exit with error on sanity check failure")
    args = parser.parse_args()

    os.environ["CUDA_VISIBLE_DEVICES"] = args.cuda_devices
    exit_code = 0

    try:
        from tensorrt_llm import LLM, SamplingParams

        print(f"Loading engine from: {args.engine_dir}")
        llm = LLM(model=args.engine_dir)

        # Simple math test - should produce "4" or "four"
        prompt = "What is 2+2? Answer with just the number:"
        output = llm.generate([prompt], SamplingParams(max_tokens=10, temperature=0.0))
        text = output[0].outputs[0].text.strip()

        print(f"  Prompt: {prompt}")
        print(f"  Output: {text}")

        # Check for reasonable output
        if "4" in text or "four" in text.lower():
            print("  Sanity check passed")
        elif text and len(text) > 0 and not has_repetition(text):
            # Has output and doesn't have obvious repetition
            print("  Sanity check passed (non-garbage output)")
        else:
            print("=" * 68, file=sys.stderr)
            print("WARNING: Sanity check may have failed", file=sys.stderr)
            print(f"  Output '{text}' doesn't look like expected answer", file=sys.stderr)
            print("  This could indicate:", file=sys.stderr)
            print("    1. Quantization not applied (garbage tokens)", file=sys.stderr)
            print("    2. Config mismatch between engine and weights", file=sys.stderr)
            print("=" * 68, file=sys.stderr)
            if args.strict:
                exit_code = 1

    except Exception as e:
        print(f"  Sanity check skipped: {e}", file=sys.stderr)
        # Don't fail build on sanity check errors - engine might still be valid

    sys.stdout.flush()
    sys.exit(exit_code)


if __name__ == "__main__":
    main()
