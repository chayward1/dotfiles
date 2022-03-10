# This file is controlled by /etc/dotfiles/README.org
# This module MUST be included within home manager
{ pkgs, ... }:

{
  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 1800;
    enableSshSupport = true;
    pinentryFlavor = "gtk2";
  };
}
