# This file is controlled by /etc/dotfiles/README.org
{ pkgs ? import <nixpkgs> { } }:

with pkgs;
mkShell {
  buildInputs = [
    nodejs
    yarn
  ];
  shellHook = ''
    export NPM_CONFIG_TMP="$XDG_RUNTIME_DIR/npm"
    export NPM_CONFIG_CACHE="$XDG_CACHE_HOME/npm"
    export NPM_CACHE_PREFIX="$XDG_CACHE_HOME/npm"
    export PATH="$(yarn global bin):$PATH"
  '';
}
