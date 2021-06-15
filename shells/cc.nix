# This file is controlled by /etc/dotfiles/README.org
{ pkgs ? import <nixpkgs> { } }:

with pkgs;
mkShell {
  buildInputs = [
    ccls
    gnumake
    libstdcxx5
    gcc-unwrapped
  ];
  shellHook = ''
  '';
}
