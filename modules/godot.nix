# This file is controlled by /etc/dotfiles/README.org
{ config, options, lib, pkgs, ... }:

with lib;
with lib.types;
let
  cfg = config.modules.godot;

in {
  options.modules.godot = {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [
      pkgs.tiled
      pkgs.godot
      pkgs.godot-server
      pkgs.godot-headless
      pkgs.gdtoolkit
    ];
  };
}
