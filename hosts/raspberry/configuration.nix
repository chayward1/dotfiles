# This file is controlled by /etc/dotfiles/README.org
{ config, pkgs, ... }:

{
  networking.hostName = "raspberry";
  networking.firewall.enable = false;
  networking.networkmanager.enable = true;
  networking.interfaces.eth0.useDHCP = true;
  networking.interfaces.wlan0.useDHCP = true;

  environment.systemPackages = [
    pkgs.libraspberrypi
    pkgs.raspberrypi-eeprom
  ];

  programs.fish.enable = true;
  programs.gnupg.agent.enable = true;

  users.users.chris = {
    shell = pkgs.fish;
    isNormalUser = true;
    extraGroups = [ "wheel" ];
  };
}
