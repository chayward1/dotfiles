# This file is controlled by /etc/dotfiles/README.org
{ pkgs ? import <nixpkgs> { } }:

with pkgs;
mkShell {
  buildInputs = [
    buf
    grpc
    grpcui
    grpcurl
    grpc-tools
    grpc-gateway
  ];
  shellHook = ''
  '';
}
