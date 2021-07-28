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
    "192.168.3.163" = [ "acernitro" ];
    "192.168.3.182" = [ "raspberry" ];
    "192.168.3.183" = [ "homecloud" ];
  };
  users.users.chris.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIO4wka/LfG3pto15DIm9LIRbb6rWr7/ipCRiCdAKSlY4 chris@chrishayward.xyz"
  ];

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
