# This file is controlled by /etc/dotfiles/README.org
{ config, pkgs, ... }:

{
  services.xserver.enable = true;
  services.xserver.layout = "us";
  services.xserver.libinput.enable = true;
  services.xserver.displayManager.startx.enable = true;

  environment = {
    systemPackages = with pkgs; [
      pkgs.sqlite
    ];
    extraInit = ''
      export XAUTHORITY=/tmp/Xauthority
      [ -e ~/.Xauthority ] && mv -f ~/.Xauthority "$XAUTHORITY"
    '';
  };

  services.picom.enable = true;
  services.openssh.enable = true;
  services.printing.enable = true;

  fonts.fonts = with pkgs; [
    iosevka
    emacs-all-the-icons-fonts
  ];
}