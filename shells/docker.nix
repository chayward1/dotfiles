# This file is controlled by /etc/dotfiles/README.org
{ pkgs ? import <nixpkgs> { } }:

with pkgs;
mkShell {
  buildInputs = [
    docker-compose
    docker-machine
  ];
  shellHook = ''
  '';
}
