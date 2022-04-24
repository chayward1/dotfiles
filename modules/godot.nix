# This file is controlled by /etc/dotfiles/README.org
{ config, pkgs, ... }:

{
  environment.systemPackages = [
    tiled
    godot
    godot-server
    godot-headless
  ];
}
