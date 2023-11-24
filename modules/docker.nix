# This file is controlled by /etc/dotfiles/README.org
{ config, options, lib, pkgs, ... }:

with lib;
with lib.types;
let cfg = config.modules.docker;
in {
  options.modules.docker = {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    # Enable the docker virutalization platform.
    virtualisation.docker = {
      enable = true;
      enableOnBoot = true;
      autoPrune.enable = true;
    };
    
    # Required for the `docker' command.
    users.users.chris.extraGroups = [ "docker" ];
    
    # Add docker extensions.
    environment.systemPackages = [
      pkgs.docker-compose
      pkgs.docker-machine
    ];
  };
}
