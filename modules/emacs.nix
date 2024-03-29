# This file is controlled by /etc/dotfiles/README.org
# This module MUST be included within home manager
{ config, options, lib, pkgs, ... }:

with lib;
with lib.types;
let
  cfg = config.modules.emacs;
  
  myEmacs = pkgs.emacsWithPackagesFromUsePackage {
    config = ../README.org;
    package = pkgs.emacs-unstable;
    alwaysEnsure = true;
    alwaysTangle = true;
    extraEmacsPackages = epkgs: [
      # Required packages...
      epkgs.exwm
      epkgs.evil
      epkgs.evil-collection
      epkgs.evil-surround
      epkgs.evil-nerd-commenter
      epkgs.general
      epkgs.which-key

      # Optional packages.
      epkgs.org
      epkgs.org-roam
      epkgs.org-roam-ui
      epkgs.websocket
      epkgs.simple-httpd
      epkgs.org-drill
      epkgs.org-pomodoro
      epkgs.writegood-mode
      epkgs.ob-http
      epkgs.ox-hugo
      epkgs.password-store
      epkgs.docker
      epkgs.dockerfile-mode
      epkgs.mu4e
      epkgs.mu4e-alert
      epkgs.dired-single
      epkgs.nerd-icons
      epkgs.all-the-icons
      epkgs.all-the-icons-dired
      epkgs.all-the-icons-ivy-rich
      epkgs.emojify
      epkgs.eshell-prompt-extras
      epkgs.vterm
      epkgs.multi-vterm
      epkgs.magit
      epkgs.hydra
      epkgs.ligature
      epkgs.elfeed
      epkgs.nix-mode
      epkgs.projectile
      epkgs.lsp-mode
      epkgs.lsp-ui
      epkgs.company
      epkgs.ccls
      epkgs.go-mode
      epkgs.dart-mode
      epkgs.lsp-dart
      epkgs.hover
      epkgs.pretty-mode
      epkgs.rustic
      epkgs.protobuf-mode
      epkgs.typescript-mode
      epkgs.yaml-mode
      epkgs.plantuml-mode

      # User interface packages.
      epkgs.neotree
      epkgs.ivy
      epkgs.counsel
      epkgs.ivy-rich
      epkgs.ivy-posframe
      epkgs.ivy-prescient
      epkgs.desktop-environment
      epkgs.doom-themes
      epkgs.doom-modeline
    ];
  };

in {
  options.modules.emacs = {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    home.packages = [
      pkgs.arandr
      pkgs.nitrogen
      pkgs.autorandr
      (pkgs.writeShellScriptBin "g" ''
        pushd $HOME
        startx
        popd /dev/null
      '')
      pkgs.pass
      (pkgs.writeShellScriptBin "pass-init" ''
        ${pkgs.git}/bin/git clone git@git.chrishayward.xyz:chris/passwords /home/chris/.password-store
        ${pkgs.pass}/bin/pass init
      '')
      pkgs.mu
      pkgs.isync
      (pkgs.writeShellScriptBin "mail-init" ''
        ${pkgs.mu}/bin/mu init --maildir="/home/chris/.cache/mail" --my-address="chris@chrishayward.xyz"
        ${pkgs.mu}/bin/mu index
      '')
      (pkgs.writeShellScriptBin "mail-sync" ''
        ${pkgs.isync}/bin/mbsync -a
      '')
      pkgs.aspell
      pkgs.aspellDicts.en
      pkgs.aspellDicts.en-science
      pkgs.aspellDicts.en-computers
      # pkgs.texlive.combined.scheme-full
      pkgs.brightnessctl
      pkgs.plantuml
      pkgs.nixfmt
      pkgs.rnix-lsp
      (pkgs.writeShellScriptBin "dotfiles-theme" ''
        ${myEmacs}/bin/emacsclient --no-wait --eval '(json-encode (dotfiles/theme))' | sed "s/\\\\//g" | sed -e 's/^"//' -e 's/"$//'
      '')
    ];

    programs.emacs = {
      enable = true;
      package = myEmacs;
    };

    xsession = {
      enable = true;
      windowManager.command = ''
        ${pkgs.nitrogen}/bin/nitrogen --restore
        ${myEmacs}/bin/emacs --daemon -f exwm-enable
        ${myEmacs}/bin/emacsclient -c
      '';
    };
    home.file.".xinitrc" = {
      text = ''
        exec ./.xsession
      '';
    };
    # Deploy the authinfo file.
    home.file.".authinfo.gpg".source = ../config/authinfo.gpg;
    
    # Deploy the isync configuration file.
    home.file.".mbsyncrc" = {
      text = ''
        IMAPStore xyz-remote
        Host mail.chrishayward.xyz
        User chris@chrishayward.xyz
        PassCmd "pass chrishayward.xyz/chris"
        SSLType IMAPS
        
        MaildirStore xyz-local
        Path ~/.cache/mail/
        Inbox ~/.cache/mail/inbox
        SubFolders Verbatim
        
        Channel xyz
        Far :xyz-remote:
        Near :xyz-local:
        Patterns * !Archives
        Create Both
        Expunge Both
        SyncState *
      '';
    };
  };
}
