# This file is controlled by /etc/dotfiles/README.org
{ pkgs ? import <nixpkgs> { } }:

with pkgs;
mkShell {
  buildInputs = [
    python310Packages.pip
    python310Packages.pip-tools
    # python310Packages.python-lsp-black    #TODO: Marked broken.
    # python310Packages.python-lsp-server   #TODO: Marked broken.
    # python310Packages.python-lsp-jsonrpc  #TODO: Marked broken.
  ];
  shellHook = ''
  '';
}
