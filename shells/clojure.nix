# This file is controlled by /etc/dotfiles/README.org
{ pkgs ? import <nixpkgs> { } }:

with pkgs;

let
  myLein = pkgs.writeShellScriptBin "lein" ''
    HOME=~/.local/share/lein ${pkgs.leiningen}/bin/lein
  '';

in mkShell {
  buildInputs = [
    myLein
  ];
  shellHook = ''
  '';
}
