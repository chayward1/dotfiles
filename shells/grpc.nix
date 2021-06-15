# This file is controlled by /etc/dotfiles/README.org
{ pkgs ? import <nixpkgs> { } }:

with pkgs;
mkShell {
  buildInputs = [
    grpc
    grpc-tools
    grpcui
    grpcurl
  ];
  shellHook = ''
  '';
}
