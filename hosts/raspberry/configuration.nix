# This file is controlled by /etc/dotfiles/README.org
{ config, pkgs, ... }:

{
  time.timeZone = "America/Toronto";

  networking.hostName = "raspberry";
  networking.firewall.enable = false;
  networking.networkmanager.enable = true;
  networking.interfaces.eth0.useDHCP = true;
  networking.interfaces.wlan0.useDHCP = true;

  networking.hosts = {
    "192.168.3.105" = [ "gamingpc" ];
    "192.168.3.136" = [ "acernitro" ];
    "192.168.3.163" = [ "acernitro_" ];
    "192.168.3.182" = [ "raspberry" ];
    "192.168.3.123" = [ "raspberry_" ];
    "192.168.3.183" = [ "homecloud" ];
    # "" = [ "homecloud_" ];
    # "" = [ "zero-one" ];
    # "" = [ "zero-two" ];
    # "" = [ "android" ];
  };

  environment.systemPackages = [
    pkgs.libraspberrypi
    pkgs.raspberrypi-eeprom
  ];

  programs.fish.enable = true;
  programs.gnupg.agent.enable = true;

  users.users.chris = {
    shell = pkgs.fish;
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" ];
  };
}
