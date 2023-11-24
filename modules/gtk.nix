# This file is controlled by /etc/dotfiles/README.org
# This module MUST be included within home manager
{ config, options, lib, pkgs, ... }:

with lib;
with lib.types;
let cfg = config.modules.gtk;
in {
  options.modules.gtk = {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    home.packages = [
      pkgs.nordic
      pkgs.arc-icon-theme
      pkgs.lxappearance
    ];

    home.file.".gtkrc-2.0" = {
      text = ''
        gtk-theme-name="Nordic-darker"
        gtk-icon-theme-name="Arc"
        gtk-font-name="Iosevka 11"
        gtk-cursor-theme-size=0
        gtk-toolbar-style=GTK_TOOLBAR_BOTH_HORIZ
        gtk-toolbar-icon-size=GTK_ICON_SIZE_LARGE_TOOLBAR
        gtk-button-images=0
        gtk-menu-images=0
        gtk-enable-event-sounds=1
        gtk-enable-input-feedback-sounds=1
        gtk-xft-antialias=1
        gtk-xft-hinting=1
        gtk-xft-hintstyle="hintmedium"
      '';
    };

    home.file.".config/gtk-2.0/gtkfilechooser.ini" = {
      text = ''
        [Filechooser Settings]
        LocationMode=path-bar
        ShowHidden=false
        ShowSizeColumn=true
        GeometryX=442
        GeometryY=212
        GeometryWidth=1036
        GeometryHeight=609
        SortColumn=name
        SortOrder=ascending
        StartupMode=recent
      '';
    };
  
    home.file.".config/gtk-3.0/settings.ini" = {
      text = ''
        [Settings]
        gtk-theme-name=Nordic-darker
        gtk-icon-theme-name=Arc
        gtk-font-name=Iosevka 11
        gtk-cursor-theme-size=0
        gtk-toolbar-style=GTK_TOOLBAR_BOTH_HORIZ
        gtk-toolbar-icon-size=GTK_ICON_SIZE_LARGE_TOOLBAR
        gtk-button-images=0
        gtk-menu-images=0
        gtk-enable-event-sounds=1
        gtk-enable-input-feedback-sounds=1
        gtk-xft-antialias=1
        gtk-xft-hinting=1
        gtk-xft-hintstyle=hintmedium
      '';
    };
  };
}
