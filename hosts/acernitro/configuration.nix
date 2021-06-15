# This file is controlled by /etc/dotfiles/README.org
{ config, pkgs, inputs, ... }:

{
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  time.timeZone = "America/Toronto";

  networking.hostName = "acernitro";
  networking.firewall.enable = false;
  networking.wireless.enable = true;
  networking.wireless.userControlled.enable = true;
  networking.useDHCP = false;
  networking.interfaces.enp6s0f1.useDHCP = true;
  networking.interfaces.wlp0s20f3.useDHCP = true;

  sound.enable = true;
  services.openssh.enable = true;
  services.printing.enable = true;

  programs.mtr.enable = true;
  programs.fish.enable = true;
  programs.gnupg.agent.enable = true;

  users.users.chris = {
    shell = pkgs.fish;
    isNormalUser = true;
    extraGroups = [ "wheel" ];
  };
}
