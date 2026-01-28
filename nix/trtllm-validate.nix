# TensorRT-LLM Engine Validation Tool
#
# Proper Haskell CLI for validating TRT-LLM engine builds.
# Replaces shell heredocs with type-safe JSON parsing.
#
# Commands:
#   detect-quant <path>     - Detect quantization from hf_quant_config.json
#   validate-engine <path> <expected-quant>  - Validate engine config.json
#   check-size <path> <quant> - Check engine size is reasonable

{ lib
, haskellPackages
}:

haskellPackages.mkDerivation {
  pname = "trtllm-validate";
  version = "0.1.0";
  
  src = ./trtllm-validate;
  
  isLibrary = false;
  isExecutable = true;
  
  executableHaskellDepends = with haskellPackages; [
    aeson
    bytestring
    directory
    filepath
    text
  ];
  
  license = lib.licenses.mit;
  description = "TensorRT-LLM engine validation tool";
  mainProgram = "trtllm-validate";
}
