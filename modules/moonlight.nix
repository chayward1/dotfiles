# This file is controlled by /etc/dotfiles/README.org
{ pkgs, ... }:

{
  environment.systemPackages = [
    pkgs.moonlight-qt
  ];
}
