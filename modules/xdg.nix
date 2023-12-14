<<file-warning>
{ config, options, lib, pkgs, ... }:

with lib;
with lib.types;
let cfg = config.modules.xdg;
in {
  options.modules.xdg = {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    environment.variables = {
      XDG_DESKTOP_DIR = "$HOME/";
      XDG_CACHE_HOME = "$HOME/.cache";
      XDG_CONFIG_HOME = "$HOME/.config";
      XDG_DATA_HOME = "$HOME/.local/share";
      XDG_BIN_HOME = "$HOME/.local/bin";
    };
  };
}
