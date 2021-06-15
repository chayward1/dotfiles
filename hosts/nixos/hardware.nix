# This file is controlled by /etc/dotfiles/README.org
{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [ (modulesPath + "/profiles/qemu-guest.nix")
    ];

  boot.initrd.availableKernelModules = [ "ata_piix" "floppy" "sd_mod" "sr_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/fddc37ff-a442-41fa-afc4-abf878be7c5a";
      fsType = "ext4";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/5fc0e3df-e796-4fe2-8482-c6acaed9d36f"; }
    ];
}
