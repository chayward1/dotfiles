# This file is controlled by /etc/dotfiles/README.org
{ pkgs, ... }:

{
  # NOTE: Use the binary until module is developed.
  home.packages = [
    pkgs.firefox-bin 
  ];
}
