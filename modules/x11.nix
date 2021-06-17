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
      pkgs.pfetch
      pkgs.cmatrix
      pkgs.asciiquarium
    ];
    extraInit = ''
      export XAUTHORITY=/tmp/Xauthority
      export xserverauthfile=/tmp/xserverauth
      [ -e ~/.Xauthority ] && mv -f ~/.Xauthority "$XAUTHORITY"
      [ -e ~/.serverauth.* ] && mv -f ~/.serverauth.* "$xserverauthfile"
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
