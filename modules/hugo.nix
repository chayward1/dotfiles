# This file is controlled by /etc/dotfiles/README.org
{ config, pkgs, ... }:

let
  myUpdateSite = pkgs.writeShellScriptBin "update-site" ''
    rsync -aP /etc/dotfiles/docs/public/ ubuntu@chrishayward.xyz:/var/www/chrishayward
  '';

in {
  environment.systemPackages = [
    pkgs.hugo
    myUpdateSite
  ];
}
