# test-matrix.nix
# ─────────────────────────────────────────────────────────────────────────────
# Runs `cabal test` for different packagesets of GHC on nixpkgs 
# Build everything with:  nix-build ./ghc-tests.nix -A all --keep-going
# Build one version only: nix-build ./ghc-tests.nix -A checks.ghc966
# ─────────────────────────────────────────────────────────────────────────────

{ pkgs ? import <nixpkgs> {} }:

let
  lib = pkgs.lib;

  supportedGhcs = [
    "ghc94"
    "ghc96"
    "ghc98"
    "ghc910"
    "ghc912"
  ];

  # Build the package + run its full test suite under one specific GHC
  railroadFor = ghcAttr:
    let
      hp = pkgs.haskell.packages.${ghcAttr} or (throw "GHC ${ghcAttr} not found in your nixpkgs!");
      pkg = pkgs.haskell.lib.doJailbreak (hp.callCabal2nix "railroad" ./. {});
    in
      pkgs.haskell.lib.doCheck pkg;   # ← this is what actually runs `cabal test`

  # One derivation per GHC (cached independently, parallelisable)
  perGhcChecks = lib.genAttrs supportedGhcs railroadFor;

  # One single derivation that depends on ALL checks.
  # Building this runs the entire matrix.
  all = pkgs.symlinkJoin {
    name = "railroad-multi-ghc-test-matrix";
    paths = lib.attrValues perGhcChecks;
    meta.description = "All railroad tests passing on every supported GHC";
  };

in
{
  inherit all;
  checks = perGhcChecks;   # for selective builds: -A checks.ghc910
}
