# This file is controlled by /etc/dotfiles/README.org
{ config, options, lib, pkgs, ... }:

with lib;
with lib.types;
let cfg = config.modules.docker;

    myDockerNuke = pkgs.writeShellScriptBin "docker-nuke" ''
      docker stop $(docker ps -aq)
      docker rm $(docker ps -aq)

      docker network prune -f
      docker rmi -f $(docker images --filter dangling=true -qa)
      docker volume rm $(docker volume ls --filter dangling=true -q)
      docker rmi -f $(docker images -qa)
    '';
    
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
      myDockerNuke
      pkgs.docker-compose
      pkgs.docker-machine
    ];
  };
}
