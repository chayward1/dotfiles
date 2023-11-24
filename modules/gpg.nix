# This file is controlled by /etc/dotfiles/README.org
# This module MUST be included within home manager
{ config, options, lib, pkgs, ... }:

with lib;
with lib.types;
let
  cfg = config.modules.gpg;
in {
  options.modules.gpg = {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    services.gpg-agent = {
      enable = true;
      defaultCacheTtl = 1800;
      enableSshSupport = true;
      pinentryFlavor = "gtk2";
    };
  };
}
