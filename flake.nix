# This file is controlled by /etc/dotfiles/README.org
{
  description = "Immutable NixOS dotfiles.";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    nixpkgs-unstable.url = "nixpkgs/master"; 
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    nix-on-droid.url = "github:t184256/nix-on-droid/master";
    nix-on-droid.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs @ { self, nixpkgs, nixpkgs-unstable, ... }: {
    nixosConfigurations = {
      nixos = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          ./hosts/nixos
          ./modules/x11.nix
          ./modules/flakes.nix
          ./modules/cachix.nix
          inputs.home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.chris = {
              imports = [
                ./modules/git.nix
                ./modules/gpg.nix
                ./modules/vim.nix
                ./modules/gtk.nix
                ./modules/emacs.nix
              ];
            };
          }
        ];
      };
      acernitro = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          ./hosts/acernitro
          ./modules/x11.nix
          ./modules/flakes.nix
          ./modules/cachix.nix
          ./modules/nvidia.nix
          ./modules/firefox.nix
          ./modules/moonlight.nix
          ./modules/teamviewer.nix
          inputs.home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.chris = {
              imports = [
                ./modules/git.nix
                ./modules/gpg.nix
                ./modules/vim.nix
                ./modules/gtk.nix
                ./modules/emacs.nix
              ];
            };
          }
        ];
      };
      android = (inputs.nix-on-droid.lib.aarch64-linux.nix-on-droid {
        config = ./hosts/android/nix-on-droid.nix;
      }).activationPackage;
      homecloud = nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          ./hosts/homecloud
          ./modules/flakes.nix
          ./modules/cachix.nix
          ./modules/docker.nix
          ./modules/jellyfin.nix
        ];
      };
      raspberry = nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          ./hosts/raspberry
          ./modules/flakes.nix
          ./modules/cachix.nix
          inputs.home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.chris = {
              imports = [
                ./modules/git.nix
                ./modules/gpg.nix
                ./modules/vim.nix
                ./modules/gtk.nix
              ];
            };
          }
        ];
      };
      zero-one = nixpkgs.lib.nixosSystem {
        system = "armv7l-linux";
        specialArgs = { inherit inputs; };
        modules = [
          ./hosts/zero-one
          ./modules/flakes.nix
          ./modules/cachix.nix
        ];
      };
      zero-two = nixpkgs.lib.nixosSystem {
        system = "armv7l-linux";
        specialArgs = { inherit inputs; };
        modules = [
          ./hosts/zero-one
          ./modules/flakes.nix
          ./modules/cachix.nix
        ];
      };
    };
  };
}
