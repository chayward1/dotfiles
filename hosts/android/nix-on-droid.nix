# This file is controlled by /etc/dotfiles/README.org
{ pkgs, ... }:

{
  environment.packages = [
    pkgs.git
    pkgs.vim
    pkgs.pass
    pkgs.gnupg
    pkgs.openssh
  ];
}
