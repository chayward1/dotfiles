# This file is controlled by /etc/dotfiles/README.org
{ config, pkgs, ... }:

{
  environment.variables = {
    XDG_CACHE_HOME = "~/.cache/"
    XDG_CONIG_DIR = "~/.config/"
    XDG_DATA_HOME = "~/.local/share/"
  };
}
