# This file is controlled by /etc/dotfiles/README.org
{ pkgs, ... }:

{
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
