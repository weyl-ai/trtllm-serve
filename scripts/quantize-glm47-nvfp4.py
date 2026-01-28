#!/usr/bin/env python3
"""
Quantize GLM-4.7 to NVFP4 format using NVIDIA Model Optimizer.

This script performs optimal partial quantization (AutoQuantize) similar to
how Salyut1/GLM-4.7-NVFP4 was created.

Usage:
    python quantize-glm47-nvfp4.py --tp-size 2 --output /path/to/output
    python quantize-glm47-nvfp4.py --tp-size 4 --output /path/to/output
"""

import argparse
import gc
import os
import sys
from pathlib import Path

import torch
from datasets import load_dataset
from transformers import AutoModelForCausalLM, AutoTokenizer

# ModelOpt imports
import modelopt.torch.quantization as mtq
from modelopt.torch.export import export_tensorrt_llm_checkpoint


def get_calib_dataloader(
    tokenizer,
    batch_size: int = 1,
    calib_size: int = 512,
    max_seq_len: int = 2048,
):
    """Create calibration dataloader from CNN/DailyMail dataset."""
    print(f"Loading calibration dataset (size={calib_size}, max_seq_len={max_seq_len})...")
    
    # Use same dataset as Salyut1
    dataset = load_dataset("abisee/cnn_dailymail", "3.0.0", split="train")
    dataset = dataset.shuffle(seed=42).select(range(min(calib_size * 2, len(dataset))))
    
    def tokenize(examples):
        # Concatenate article and highlights for calibration
        texts = [f"{a}\n\n{h}" for a, h in zip(examples["article"], examples["highlights"])]
        return tokenizer(
            texts,
            truncation=True,
            max_length=max_seq_len,
            padding="max_length",
            return_tensors="pt",
        )
    
    # Tokenize
    tokenized = dataset.map(
        tokenize,
        batched=True,
        batch_size=batch_size,
        remove_columns=dataset.column_names,
    )
    
    # Create dataloader
    from torch.utils.data import DataLoader
    
    def collate_fn(batch):
        input_ids = torch.stack([torch.tensor(b["input_ids"]) for b in batch])
        attention_mask = torch.stack([torch.tensor(b["attention_mask"]) for b in batch])
        return {"input_ids": input_ids, "attention_mask": attention_mask}
    
    dataloader = DataLoader(
        tokenized.select(range(min(calib_size, len(tokenized)))),
        batch_size=batch_size,
        collate_fn=collate_fn,
    )
    
    print(f"Calibration dataloader ready: {len(dataloader)} batches")
    return dataloader


def quantize_model(
    model_path: str,
    output_path: str,
    tp_size: int = 4,
    calib_size: int = 512,
    max_seq_len: int = 2048,
):
    """Quantize GLM-4.7 to NVFP4 and export for TensorRT-LLM."""
    
    print(f"=" * 70)
    print(f"GLM-4.7 NVFP4 Quantization")
    print(f"=" * 70)
    print(f"Model: {model_path}")
    print(f"Output: {output_path}")
    print(f"TP Size: {tp_size}")
    print(f"Calib Size: {calib_size}")
    print(f"Max Seq Len: {max_seq_len}")
    print(f"=" * 70)
    
    # Load tokenizer
    print("\nLoading tokenizer...")
    tokenizer = AutoTokenizer.from_pretrained(model_path, trust_remote_code=True)
    if tokenizer.pad_token is None:
        tokenizer.pad_token = tokenizer.eos_token
    
    # Load model
    print("\nLoading model (this may take a while for 358B params)...")
    model = AutoModelForCausalLM.from_pretrained(
        model_path,
        torch_dtype=torch.bfloat16,
        device_map="auto",  # Distribute across available GPUs
        trust_remote_code=True,
    )
    model.eval()
    
    print(f"Model loaded on devices: {set(p.device for p in model.parameters())}")
    
    # Get calibration data
    calib_dataloader = get_calib_dataloader(
        tokenizer,
        batch_size=1,
        calib_size=calib_size,
        max_seq_len=max_seq_len,
    )
    
    # Define forward loop for calibration
    def forward_loop(model):
        for i, batch in enumerate(calib_dataloader):
            if i % 50 == 0:
                print(f"  Calibration batch {i}/{len(calib_dataloader)}")
            input_ids = batch["input_ids"].to(model.device)
            attention_mask = batch["attention_mask"].to(model.device)
            with torch.no_grad():
                model(input_ids=input_ids, attention_mask=attention_mask)
    
    # Quantize with NVFP4
    # Use NVFP4_KV_CFG for FP4 weights + FP8 KV cache (better for Blackwell)
    print("\nApplying NVFP4 quantization...")
    print("  Config: NVFP4_KV_CFG (FP4 weights, FP8 KV cache)")
    
    mtq.quantize(
        model,
        mtq.NVFP4_KV_CFG,
        forward_loop=forward_loop,
    )
    
    print("\nQuantization complete!")
    
    # Export to TensorRT-LLM checkpoint format
    print(f"\nExporting to TensorRT-LLM checkpoint (TP={tp_size})...")
    
    output_dir = Path(output_path)
    output_dir.mkdir(parents=True, exist_ok=True)
    
    export_tensorrt_llm_checkpoint(
        model,
        decoder_type="glm4_moe",  # GLM-4.7 MoE architecture
        dtype=torch.bfloat16,
        export_dir=str(output_dir),
        inference_tensor_parallel=tp_size,
        inference_pipeline_parallel=1,
    )
    
    # Copy tokenizer files
    print("\nCopying tokenizer files...")
    tokenizer.save_pretrained(output_dir)
    
    # Copy any additional config files
    import shutil
    for fname in ["config.json", "generation_config.json", "chat_template.jinja"]:
        src = Path(model_path) / fname
        if src.exists():
            shutil.copy(src, output_dir / fname)
    
    print(f"\n{'=' * 70}")
    print(f"Quantization complete!")
    print(f"Output: {output_dir}")
    print(f"{'=' * 70}")
    
    # Cleanup
    del model
    gc.collect()
    torch.cuda.empty_cache()


def main():
    parser = argparse.ArgumentParser(description="Quantize GLM-4.7 to NVFP4")
    parser.add_argument(
        "--model",
        type=str,
        default="/fxy/data/0x02/models/GLM-4.7",
        help="Path to GLM-4.7 model",
    )
    parser.add_argument(
        "--output",
        type=str,
        required=True,
        help="Output directory for quantized checkpoint",
    )
    parser.add_argument(
        "--tp-size",
        type=int,
        default=4,
        choices=[1, 2, 4, 8],
        help="Tensor parallel size",
    )
    parser.add_argument(
        "--calib-size",
        type=int,
        default=512,
        help="Number of calibration samples",
    )
    parser.add_argument(
        "--max-seq-len",
        type=int,
        default=2048,
        help="Max sequence length for calibration",
    )
    
    args = parser.parse_args()
    
    # Validate model path
    if not Path(args.model).exists():
        print(f"Error: Model path does not exist: {args.model}")
        sys.exit(1)
    
    quantize_model(
        model_path=args.model,
        output_path=args.output,
        tp_size=args.tp_size,
        calib_size=args.calib_size,
        max_seq_len=args.max_seq_len,
    )


if __name__ == "__main__":
    main()
