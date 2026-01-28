# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
#                                                                // tool-server.nix
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
#
# Servant-based tool server with OpenAPI3 for AI agents.
#
# Features:
#   - Code sandbox: workspace management, AST ops, compilation
#   - Attestation: Ed25519 identity, signed git commits
#   - Tools: web search, URL reading
#   - Auto-generated OpenAPI 3.0 spec at /openapi.json
#
# Usage:
#   nix build .#tool-server
#   nix run .#tool-server
#
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

{ lib
, haskellPackages
, makeWrapper
, ast-grep
, git
, openssh
, gnupg
  # Compilers for the "Language Coset"
, rustc
, ghc
, lean4
, dhall
, purescript
}:

let
  # Build the tool-server package
  toolServer = haskellPackages.mkDerivation {
    pname = "tool-server";
    version = "0.1.0";
    
    src = ../tool-server;
    
    isLibrary = true;
    isExecutable = true;
    
    libraryHaskellDepends = with haskellPackages; [
      aeson
      containers
      directory
      filepath
      process
      stm
      temporary
      text
      time
    ];
    
    executableHaskellDepends = with haskellPackages; [
      aeson
      http-client
      http-client-tls
      openapi3
      servant
      servant-openapi3
      servant-server
      text
      warp
    ];
    
    testHaskellDepends = with haskellPackages; [
      aeson
      bytestring
      directory
      hspec
      http-client
      servant-client
      text
    ];
    
    license = lib.licenses.mit;
    description = "Servant-based tool server with OpenAPI3 for AI agents";
    mainProgram = "tool-server";
  };

in
  # Wrap with runtime dependencies
  haskellPackages.buildPackages.runCommand "tool-server-wrapped" {
    nativeBuildInputs = [ makeWrapper ];
    meta = toolServer.meta // {
      mainProgram = "tool-server";
    };
  } ''
    mkdir -p $out/bin
    
    makeWrapper ${toolServer}/bin/tool-server $out/bin/tool-server \
      --prefix PATH : ${lib.makeBinPath [
        ast-grep
        git
        openssh
        gnupg
        # Compilers
        rustc
        ghc
        lean4
        dhall
        purescript
      ]}
  ''
