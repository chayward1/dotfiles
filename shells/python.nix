# This file is controlled by /etc/dotfiles/README.org
{ pkgs ? import <nixpkgs> { } }:

with pkgs;
mkShell {
  buildInputs = [
    python310Packages.pip
    python310Packages.pip-tools
    python310Packages.python-lsp-black
    python310Packages.python-lsp-server
    python310Packages.python-lsp-jsonrpc
    # python310Packages.pyls-mypy
    # python310Packages.pyls-isort
    # python310Packages.pyls-black
  ];
  shellHook = ''
  '';
}
