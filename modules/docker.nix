# This file is controlled by /etc/dotfiles/README.org
{ config, pkgs, ... }:

{
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
}
