# This file is controlled by /etc/dotfiles/README.org
{ pkgs ? import <nixpkgs> { } }:

with pkgs;
mkShell {
  buildInputs = [
    gdb
    ccls
    cmake
    boost
    gnumake
    gcc-unwrapped
  ];
  shellHook = ''
  '';
}
