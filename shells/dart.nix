#This file is controlled by /etc/dotfiles/README.org
{ pkgs ? import <nixpkgs> { } }:

with pkgs;
mkShell {
  buildInputs = [
    dart
    flutter
  ];
  shellHook = ''
    export FLUTTER_SDK_DIR=${flutter}/bin/cache/dart-sdk/
    export PATH="$HOME/.pub-cache/bin":"$PATH"
  '';
}
