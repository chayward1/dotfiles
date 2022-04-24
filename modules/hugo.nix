# This file is controlled by /etc/dotfiles/README.org
{ config, pkgs, ... }:

let
  mySiteDir = "/etc/dotfiles/docs/public/";
  mySiteTgt = "ubuntu@chrishayward.xyz:/var/www/wedding";
  mySiteBuild = pkgs.writeShellScriptBin "my-site-build" ''
    pushd ${mySiteDir}../ && ${pkgs.hugo}/bin/hugo -v ; popd
  '';
  mySiteUpdate = pkgs.writeShellScriptBin "my-site-update" ''
    ${pkgs.rsync}/bin/rsync -aP ${mySiteDir} ${mySiteTgt}
  '';

in {
  environment.systemPackages = [
    mySiteBuild
    mySiteUpdate
  ];
}
