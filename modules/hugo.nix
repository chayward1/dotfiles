# This file is controlled by /etc/dotfiles/README.org
{ config, options, lib, pkgs, ... }:

with lib;
with lib.types;
let
  cfg = config.modules.hugo;
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
  options.modules.hugo = {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };
  
  config = mkIf cfg.enable {
    environment.systemPackages = [
      pkgs.hugo
      mySiteBuild
      mySiteUpdate
    ];
  };
}
