{ pkgs ? import <nixpkgs> { } }:

with pkgs;
mkShell {
  buildInputs = [
    docker-compose
    docker-machine
  ];
  shellHook = ''
  '';
}
