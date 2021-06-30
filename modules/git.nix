# This file is controlled by /etc/dotfiles/README.org
{ pkgs, ... }:

let
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
  home.packages = [ myGitFix ];

  programs.git = {
    enable = true;
    userName = "Christopher James Hayward";
    userEmail = "chris@chrishayward.xyz";

    signing = {
      key = "37AB1CB72B741E478CA026D43025DCBD46F81C0F";
      signByDefault = true;
    };
  };
}
