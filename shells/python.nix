# This file is controlled by /etc/dotfiles/README.org
{ pkgs ? import <nixpkgs> { } }:

with pkgs;
mkShell {
  buildInputs = [
    python39Packages.pip
    python39Packages.pip-tools
    python39Packages.pyls-mypy
    python39Packages.pyls-isort
    python39Packages.pyls-black
  ];
  shellHook = ''
  '';
}
