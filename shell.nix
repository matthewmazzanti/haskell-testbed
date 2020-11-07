{ pkgs ? import <nixpkgs> {}, ... }:
with pkgs;
let
  ghcWithPackages = pkgs.ghc.withHoogle (hsPkgs: with hsPkgs; [
    QuickCheck
    comonad
    freer-simple
    free
    mtl
    numeric-prelude
  ]);
in pkgs.mkShell {
  buildInputs = with pkgs; [
    ghcWithPackages
  ];
}
