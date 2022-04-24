# This file is controlled by /etc/dotfiles/README.org
{ config, pkgs, ... }:

{
  environment.systemPackages = [
    pkgs.tiled
    pkgs.godot
    pkgs.godot-server
    pkgs.godot-headless
    pkgs.python310Packages.gdtoolkit
  ];
}
