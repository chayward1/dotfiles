# This file is controlled by /etc/dotfiles/README.org
# This module MUST be included within home manager
{ config, options, lib, pkgs, ... }:

with lib;
with lib.types;
let cfg = config.modules.vim;
in {
  options.modules.vim = {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
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
  };
}
