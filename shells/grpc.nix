# This file is controlled by /etc/dotfiles/README.org
{ pkgs ? import <nixpkgs> { } }:

with pkgs;
mkShell {
  buildInputs = [
    grpc
    grpcui
    grpcurl

    # FIXME: Broken on nixpkgs/unstable.
    # grpc-tools
  ];
  shellHook = ''
  '';
}
