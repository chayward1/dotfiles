# This file is controlled by /etc/dotfiles/README.org
{ pkgs ? import <nixpkgs> { } }:

with pkgs;

let
  myNix = writeShellScriptBin "nix" ''
    exec ${nixFlakes}/bin/nix --option experimental-features "nix-command flakes" "$@"
  '';

in mkShell {
  buildInputs = [
    git
    myNix
  ];
  shellHook = ''
    export DOTFILES="$(pwd)"
  '';
}
