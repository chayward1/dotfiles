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
          ./modules/ssh.nix
          ./modules/hugo.nix
          ./modules/godot.nix
          ./modules/flakes.nix
          ./modules/cachix.nix
          ./modules/firefox.nix
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
    };
    nixOnDroidConfigurations = {
      android = {
        device = inputs.nix-on-droid.lib.nixOnDroidConfiguration {
          config = ./hosts/android/nix-on-droid.nix;
          system = "aarch64-linux";
          specialArgs = { inherit inputs; };
        };
      };
    };
  };
}
