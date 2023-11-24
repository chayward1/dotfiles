# This file is controlled by /etc/dotfiles/README.org
{ config, options, lib, pkgs, ... }:

with lib;
with lib.types;
let
  cfg = config.modules.cachix;

in {
  options.modules.cachix = {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    nix = {
      settings = {
        substituters = [
          "https://nix-community.cachix.org"
        ];
        trusted-public-keys = [
          "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        ];
      };
    };
  };
}
