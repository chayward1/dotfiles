{
  description = "Immutable NixOS dotfiles.";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    nixpkgs-unstable.url = "nixpgs/master";
    emacs-overlay.url = "github:nix-comunity/emacs-overlay";
    nixos-hardwre.url = "github:nixos/nixos-hardware";
  };

  outputs = inputs @ { self, nixpkgs, ... }: {
    nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = { inherit inputs; };
      modules = [
        ./hosts/nixos/configuration.nix
        ./hosts/nixos/hardware.nix
        ./config/cachix.nix
      ];
    };
  };
}
