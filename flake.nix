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
      # NOTE: Work In Progress!
      # acernitro = nixpkgs.lib.nixosSystem {
      #   system = "x86_64-linux";
      #   specialArgs = { inherit inputs; };
      #   modules = [
      #     ./hosts/acernitro
      #     ./modules/x11.nix
      #     ./modules/flakes.nix
      #     ./modules/cachix.nix
      #     inputs.home-manager.nixosModules.home-manager {
      #       home-manager.useGlobalPkgs = true;
      #       home-manager.useUserPackages = true;
      #       home-manager.users.chris = {
      #         imports = [
      #           ./modules/git.nix
      #           ./modules/gpg.nix
      #           ./modules/vim.nix
      #           ./modules/gtk.nix
      #           ./modules/emacs.nix
      #         ];
      #       };
      #     }
      #   ];
      # };
    };
  };
}
