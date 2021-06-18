# This file is controlled by /etc/dotfiles/README.org
{ pkgs ? import <nixpkgs> { } }:

with pkgs;
mkShell {
  buildInputs = [
    python38Packages.pip
    python38Packages.pip-tools
    python38Packages.pyls-mypy
    python38Packages.pyls-isort
    python38Packages.pyls-black
  ];
  shellHook = ''
  '';
}
