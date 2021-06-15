# This file is controlled by /etc/dotfiles/README.org
{ pkgs, ... }:

{
  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 1800;
    enableSshSupport = true;
    pinentryFlavor = "gtk2";
  };
}
