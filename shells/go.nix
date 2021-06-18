# This file is controlled by /etc/dotfiles/README.org
{ pkgs ? import <nixpkgs> { } }:

with pkgs;
mkShell {
  buildInputs = [
    go
    gopls
  ];
  shellHook = ''
    export GO111MODULE=on
    export GOPATH=$XDG_DATA_HOME/go
    export PATH=$GOPATH/bin:$PATH
  '';
}
