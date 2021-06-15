# This file is controlled by /etc/dotfiles/README.org
{ config, pkgs, inputs, ... }:

{
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";

  time.timeZone = "America/Toronto";

  networking.hostName = "nixos";
  networking.useDHCP = false;
  networking.firewall.enable = false;
  networking.interfaces.ens3.useDHCP = true;

  programs.mtr.enable = true;
  programs.fish.enable = true;
  programs.gnupg.agent.enable = true;

  users.users.chris = {
    shell = pkgs.fish;
    isNormalUser = true;
    extraGroups = [ "wheel" ];
  };
}
