# This file is controlled by /etc/dotfiles/README.org
{ pkgs ? import <nixpkgs> { } }:

with pkgs;
mkShell {
  buildInputs = [
    gdb
    ccls
    cmake
    gnumake
    libstdcxx5
    gcc-unwrapped
    llvmPackages.libcxx
  ];
  shellHook = ''
  '';
}
