# This file is controlled by /etc/dotfiles/README.org
{ config, options, lib, pkgs, inputs, ... }:

with lib;
with lib.types;
let cfg = config.modules.flakes;
in {
  options.modules.flakes = {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    nix = {
      package = pkgs.nixUnstable;
      extraOptions = ''
        experimental-features = nix-command flakes
      '';
    };

    nixpkgs = {
      config = { allowUnfree = true; };
      overlays = [ inputs.emacs-overlay.overlay ];
    };
  };
}
