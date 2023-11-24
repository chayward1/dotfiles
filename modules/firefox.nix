# This file is controlled by /etc/dotfiles/README.org
{ config, options, lib, pkgs, ... }:

with lib;
with lib.types;
let
  cfg = config.modules.firefox;
  myFirefox = pkgs.writeShellScriptBin "firefox" ''
    HOME=~/.local/share/mozilla ${pkgs.firefox-bin}/bin/firefox
  '';

in {
  options.modules.firefox = {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    # NOTE: Use the binary until module is developed.
    environment.systemPackages = [
      myFirefox
    ];
  };
}
