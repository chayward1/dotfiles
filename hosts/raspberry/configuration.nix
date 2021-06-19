# This file is controlled by /etc/dotfiles/README.org
{ config, pkgs, ... }:

{
  networking.hostName = "raspberry";
  networking.firewall.enable = false;
  networking.wireless.enable = true;
  networking.wireless.userControlled.enable = true;

  

  environment.systemPackages = [
    pkgs.libraspberrypi
    pkgs.raspberrypi-eeprom
  ];

  programs.fish.enable = true;
  programs.gnupg.agent.enable = true;

  users.users.chris = {
    shell = pkgs.fish;
    isnormaluser = true;
    extragroups = [ "wheel" ];
  };
}
