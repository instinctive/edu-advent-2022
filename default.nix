{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc902" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callCabal2nix "advent" (./.) {}
