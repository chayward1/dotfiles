# This file is controlled by /etc/dotfiles/README.org
{
  description = "Immutable NixOS dotfiles.";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    nixpkgs-unstable.url = "nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
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
          ./modules/x11.nix
          ./modules/ssh.nix
          ./modules/hugo.nix
          ./modules/docker.nix
          ./modules/flakes.nix
          ./modules/cachix.nix
          ./modules/firefox.nix
          inputs.home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.chris = {
              home.stateVersion = "23.05";
              
              imports = [
                ./modules/git.nix
                ./modules/gpg.nix
                ./modules/vim.nix
                ./modules/gtk.nix
                ./modules/emacs.nix
              ];
              
              modules.git = {
                enable = true;
                name = "Christopher James Hayward";
                email = "chris@chrishayward.xyz";
                key = "37AB1CB72B741E478CA026D43025DCBD46F81C0F";
              };
          
              modules.gpg.enable = true;
              modules.vim.enable = true;
              modules.gtk.enable = true;
              modules.emacs.enable = true;
            };
          }
        ];
      };
    };
  } //
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = inputs.nixpkgs.legacyPackages.${system};
      in
        rec {
          devShells = {
            default = import ./shell.nix { inherit pkgs; };
            cc = import ./shells/cc.nix { inherit pkgs; };
            go = import ./shells/go.nix { inherit pkgs; };
            dart = import ./shells/dart.nix { inherit pkgs; };
            grpc = import ./shells/grpc.nix { inherit pkgs; };
            java = import ./shells/java.nix { inherit pkgs; };
            node = import ./shells/node.nix { inherit pkgs; };
            python = import ./shells/python.nix { inherit pkgs; };
            rust = import ./shells/rust.nix { inherit pkgs; };
          };
        }
    );
}
