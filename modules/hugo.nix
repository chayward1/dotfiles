# This file is controlled by /etc/dotfiles/README.org
{ config, pkgs, ... }:

let
  mySiteDir = "/etc/dotfiles/docs/public/";
  mySiteTgt = "unbuntu@chrishayward.xyz:/var/www/wedding";
  mySiteUpdate = pkgs.writeShellScriptBin "my-site-update" ''
    ${pkgs.rsync}/bin/rsync -aP ${mySiteDir} ${mySiteTgt}
  '';

in {
  environment.systemPackages = [
    pkgs.hugo
    mySiteUpdate
  ];
}
