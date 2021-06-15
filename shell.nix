# This file is controlled by /etc/dotfiles/README.org
{ pkgs ? import <nixpkgs> { } }:

with pkgs;

let
  nixBin = writeShellScriptBin "nix" ''
    ${nixFlakes}/bin/nix --option experimental-features "nix-command flakes" "$@"
  '';

in mkShell {
  buildInputs = [
    git
  ];
  shellHook = ''
    export FLAKE="$(pwd)"
    export PATH="$FLAKE/bin:${nixBin}/bin:$PATH"
  '';
}
