# This file is controlled by /etc/dotfiles/README.org
{ pkgs, ... }:

{
  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    extraConfig = ''
      set number relativenumber
      set nobackup
    '';
    extraPackages = [
      pkgs.nixfmt
    ];
    plugins = with pkgs.vimPlugins; [
      vim-nix
      vim-airline
      vim-polyglot
    ];
  };
}
