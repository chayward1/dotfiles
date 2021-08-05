# This file is controlled by /etc/dotfiles/README.org
{ pkgs ? import <nixpkgs> { } }:

with pkgs;
mkShell {
  buildInputs = [
    # openjdk8  # Legacy Java 8 VM.
    # openjdk11 # Current LTS version of OpenJDK.
    openjdk14   # Current version of OpenJDK.
  ];
  shellHook = ''
  '';
}
