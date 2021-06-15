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

  # Pre-configured wireless networks.
  networking.wireless.networks.MyWiFi_5C1870.pskRaw =
    "409b3c85fef1c5737f284d2f82f20dc6023e41804e862d4fa26265ef8193b326";

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
