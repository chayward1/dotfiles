{ lib, pkgs, config, ...}:

with lib;
let
  cfg = config.services.hello;

in {
  options.services.hello = {
    enable = mkEnableOption "Hello service.";
    greeter = mkOption {
      type = types.str;
      default = "world";
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [
      (pkgs.writeShellScriptBin "hello" ''
        echo "'Hello, ${escapeShellArg cfg.greeter}!'"
      '');
    ];
  };
}
