# This file is controlled by /etc/dotfiles/README.org
{ config, ... }:

{
  imports = [
    ./configuration.nix
    ./hardware.nix
  ];

  modules.x11.enable = true;
  modules.xdg.enable = true;
  modules.ssh.enable = true;
  modules.hugo.enable = true;
  modules.flakes.enable = true;
  modules.cachix.enable = true;
  modules.docker.enable = true;
  modules.firefox.enable = true;
}
