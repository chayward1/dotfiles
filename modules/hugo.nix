{ config, pkgs, ... }:

let
  myUpdateSite = pkgs.writeShellScriptBin "dotfiles-update-site" ''
    rsync -aP /etc/dotfiles/docs/public ubuntu@chrishayward.xyz:/var/www/chrishayward
  '';

in {
  environment.systemPackages = [
    pkgs.hugo
    myUpdateSite
  ];
}
