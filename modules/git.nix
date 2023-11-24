# This file is controlled by /etc/dotfiles/README.org
# This module MUST be included within home manager
{ config, options, lib, pkgs, ... }:

with lib;
with lib.types;
let
  cfg = config.modules.git;
  
  # Fix any corruptions in the local copy.
  myGitFix = pkgs.writeShellScriptBin "git-fix" ''
    if [ -d .git/objects/ ]; then
      find .git/objects/ -type f -empty | xargs rm -f
      git fetch -p
      git fsck --full
    fi
    exit 1
  '';

in {
  options.modules.git = {
    enable = mkOption {
      type = bool;
      default = false;
    };

    name = mkOption {
      type = str;
      default = "Anon";
    };

    email = mkOption {
      type = str;
      default = "anon@devnull.com";
    };

    key = mkOption {
      type = str;
      default = "ABCD1234";
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ myGitFix ];
    
    programs.git = {
      enable = true;
      userName = cfg.name;
      userEmail = cfg.email;
      
      signing = {
        key = cfg.key;
        signByDefault = true;
      };
    };
  };
}
