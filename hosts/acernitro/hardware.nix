# This file is controlled by /etc/dotfiles/README.org
{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [ (modulesPath + "/installer/scan/not-detected.nix")
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/2f548eb9-47ce-4280-950f-9c6d1d162852";
      fsType = "ext4"; 
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/5BC3-73F3";
      fsType = "vfat";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/bef7bf62-d26f-45b1-a1f8-1227c2f8b26a"; }
    ];

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
