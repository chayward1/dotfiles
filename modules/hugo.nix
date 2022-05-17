# This file is controlled by /etc/dotfiles/README.org
{ config, pkgs, ... }:

let
  mySiteDir = "/etc/dotfiles/docs/public/";
  mySiteTgt = "ubuntu@chrishayward.xyz:/var/www/chrishayward";
  mySiteBuild = pkgs.writeShellScriptBin "site-build" ''
    pushd ${mySiteDir}../ > /dev/null &&
    ${pkgs.hugo}/bin/hugo -v ;
    popd > /dev/null
  '';
  mySiteUpdate = pkgs.writeShellScriptBin "site-update" ''
    ${pkgs.rsync}/bin/rsync -aP ${mySiteDir} ${mySiteTgt}
  '';

in {
  environment.systemPackages = [
    mySiteBuild
    mySiteUpdate
  ];
}
