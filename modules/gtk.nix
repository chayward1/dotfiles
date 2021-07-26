# This file is controlled by /etc/dotfiles/README.org
{ pkgs, ... }:

{
  home.packages = [
    pkgs.nordic
    pkgs.arc-icon-theme
    pkgs.lxappearance
  ];

  home.file.".gtkrc-2.0" = {
    text = ''
      gtk-theme-name="Nordic-Polar"
      gtk-icon-theme-name="Arc"
      gtk-font-name="Sans 10"
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
}
