# This file is controlled by /etc/dotfiles/README.org
{ config, pkgs, ... }:

{
  services.openssh = {
    enable = true;
    permitRootLogin = false;
    passwordAuthentication = false;
  };
}
