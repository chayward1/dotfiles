# This file is controlled by /etc/dotfiles/README.org
{ config, pkgs, inputs, ... }:

{
  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  nixpkgs = {
    config = { allowUnfree = true; };
    overlays = [ inputs.emacs-overlay.overlay ];
  };
}
