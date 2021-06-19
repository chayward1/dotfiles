# This file is controlled by /etc/dotfiles/README.org
{ config, pkgs, inputs, ... }:

{
  time.timeZone = "America/Toronto";

  networking.hostName = "nixos";
  networking.useDHCP = false;
  networking.firewall.enable = false;
  networking.interfaces.ens3.useDHCP = true;

  networking.hosts = {
    "192.168.3.105" = [ "gamingpc" ];
    # "" = [ "acernitro" ];
    # "" = [ "acernitro_" ];
    "192.168.3.182" = [ "raspberry" ];
    "192.168.3.123" = [ "raspberry_" ];
    # "" = [ "homecloud" ];
    # "" = [ "homecloud_" ];
    # "" = [ "zero-one" ];
    # "" = [ "zero-two" ];
    # "" = [ "android" ];
  };

  programs.mtr.enable = true;
  programs.fish.enable = true;
  programs.gnupg.agent.enable = true;

  users.users.chris = {
    shell = pkgs.fish;
    isNormalUser = true;
    extraGroups = [ "wheel" ];
  };
}
