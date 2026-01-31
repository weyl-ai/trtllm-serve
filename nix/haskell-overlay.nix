# nix/haskell-overlay.nix
#
# Haskell package overrides for grapesy (gRPC) and CAS client.
#
# NOTE: The grapesy stack has tricky interdependencies. We use fetchFromGitHub
# and callCabal2nix to avoid the cabal2nix→tls cycle.
#
# For now, we skip grapesy and use a simplified local CAS implementation.
# When ready to use NativeLink, import this overlay from straylight/aleph
# which has the full grapesy stack working with GHC 9.12.
#
final: prev:
let
  haskell-lib = prev.haskell.lib;
  dont-check = haskell-lib.dontCheck;
in
{
  haskellPackages = prev.haskellPackages.override {
    overrides = _hself: hsuper: {
      # ────────────────────────────────────────────────────────────────────────
      # cryptonite - skip flaky test
      # ────────────────────────────────────────────────────────────────────────
      cryptonite = dont-check hsuper.cryptonite;
    };
  };
}
