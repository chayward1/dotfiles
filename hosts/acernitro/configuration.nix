# This file is controlled by /etc/dotfiles/README.org
{ config, pkgs, inputs, ... }:

{
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.efiSupport = true;
  boot.loader.grub.useOsProber = true;
  boot.loader.grub.device = "/dev/sda";

  time.timeZone = "America/Toronto";

  networking.hostName = "acernitro";
  networking.useDHCP = false;
  networking.firewall.enable = false;
  networking.interfaces.ens3.useDHCP = true;
  networking.wireless.enable = true;
  networking.wireless.userControlled.enable = true;

  programs.mtr.enable = true;
  programs.fish.enable = true;
  programs.gnupg.agent.enable = true;

  users.users.chris = {
    shell = pkgs.fish;
    isNormalUser = true;
    extraGroups = [ "wheel" ];
  };
}
