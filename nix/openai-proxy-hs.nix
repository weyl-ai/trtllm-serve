# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
#                                                          // openai-proxy-hs.nix
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
#
# Haskell OpenAI-compatible proxy for TensorRT-LLM backend.
#
# Features:
#   - Warp-based server with explicit state machine
#   - Megaparsec grammar for Qwen3 chat template
#   - QuickCheck property tests for protocol compliance
#
# Usage:
#   nix run .#openai-proxy-hs
#   nix build .#openai-proxy-hs.tests.proxy-proptest
#
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

{ lib
, haskellPackages
}:

haskellPackages.mkDerivation {
  pname = "openai-proxy-hs";
  version = "0.1.0";
  
  src = ./openai-proxy-hs;
  
  isLibrary = true;
  isExecutable = true;
  
  libraryHaskellDepends = with haskellPackages; [
    megaparsec
    text
  ];
  
  executableHaskellDepends = with haskellPackages; [
    aeson
    bytestring
    http-client
    http-client-tls
    http-types
    megaparsec
    stm
    text
    time
    uuid
    wai
    warp
  ];
  
  testHaskellDepends = with haskellPackages; [
    aeson
    async
    bytestring
    http-client
    http-types
    process
    QuickCheck
    text
    time
    vector
    wai
    warp
  ];
  
  # Tests require running proxy - run separately with nix run .#proxy-proptest
  doCheck = false;
  
  license = lib.licenses.mit;
  description = "OpenAI-compatible proxy for TensorRT-LLM (Haskell/Warp) with QuickCheck tests";
  mainProgram = "openai-proxy-hs";
}
