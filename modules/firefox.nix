# This file is controlled by /etc/dotfiles/README.org
{ config, pkgs, ... }:

let
  myFirefox = pkgs.writeShellScriptBin "firefox" ''
    HOME=~/.local/share/mozilla ${pkgs.firefox-bin}/bin/firefox
  '';

in {
  # NOTE: Use the binary until module is developed.
  environment.systemPackages = [
    myFirefox
  ];
}
