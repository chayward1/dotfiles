# This file is controlled by /etc/dotfiles/README.org
{ config, pkgs, ... }:

{
  networking.hostName = "raspberry";
  networking.firewall.enable = false;
  networking.wireless.enable = true;
  networking.wireless.userControlled.enable = true;

  # Pre-configured wireless networks.
  networking.wireless.networks.MyWiFi_5C1870.pskRaw =
    "409b3c85fef1c5737f284d2f82f20dc6023e41804e862d4fa26265ef8193b326";

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
