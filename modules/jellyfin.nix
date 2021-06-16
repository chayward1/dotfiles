# This file is controlled by /etc/dotfiles/README.org
{ config, pkgs, ... }:

{
  services.jellyfin = {
    enable = true;
  };
}
