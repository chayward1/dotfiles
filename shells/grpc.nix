{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  shellHook = ''
    export DOTFILES_SHELL_GRPC="1"
  '';

  buildInputs = with pkgs; [
    grpc      # C based gRPC (C++, Python, Ruby, Obj-C, PHP, C#).
    grpcui    # Interactive web UI for gRPC, like postman.
    grpcurl   # Like cURL, but for gRPC servers.
    protobuf  # gRPC data interchange format.
    protobufc # C bindings for gRPC.
  ];
}