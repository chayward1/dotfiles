# This file is controlled by /etc/dotfiles/README.org
{ config, pkgs, lib, inputs, ... }:

{
  # imports = [
  #   inputs.nixos-hardware.nixosModules.raspberry-pi-4
  # ];

  boot.kernelPackages = pkgs.linuxPackages_rpi4;
  boot.tmpOnTmpfs = true;
  boot.initrd.availableKernelModules = [ "usbhid" "usb_storage" ];
  # boot.kernelParams = [
  #   "8250.nr_uarts=1"
  #   "console=ttyAMA0,115200"
  #   "console=tty1"
  #   "cma=128M"
  # ];

  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;
  # boot.loader.raspberryPi = {
  #   enable = true;
  #   version = 4;
  #   firmwareConfig = ''
  #     dtparam=sd_poll_once=on
  #     dtparam=audio=on
  #   '';
  # };

  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.enableRedistributableFirmware = true;
  # hardware.raspberry-pi."4".fkms-3d.enable = true;

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/44444444-4444-4444-8888-888888888888";
      fsType = "ext4";
      options = [ "noatime" ];
    };
  };

  powerManagement.cpuFreqGovernor = "ondemand";
}
