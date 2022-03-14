# This file is controlled by /etc/dotfiles/README.org
# This module MUST be included within home manager
{ pkgs, ... }:

let
  myEmacs = pkgs.emacsWithPackagesFromUsePackage {
    config = ../README.org;
    package = pkgs.emacsGcc;
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
      epkgs.mu4e-alert
      epkgs.dired-single
      epkgs.all-the-icons
      epkgs.all-the-icons-dired
      epkgs.all-the-icons-ivy-rich
      epkgs.emojify
      epkgs.eshell-prompt-extras
      epkgs.vterm
      epkgs.magit
      epkgs.hydra
      epkgs.elfeed
      epkgs.nix-mode
      epkgs.projectile
      epkgs.lsp-mode
      epkgs.lsp-ui
      epkgs.company
      epkgs.ccls
      epkgs.go-mode
      epkgs.pretty-mode
      epkgs.rustic
      epkgs.plantuml-mode
      epkgs.protobuf-mode

      # User interface packages.
      epkgs.ivy
      epkgs.counsel
      epkgs.ivy-rich
      epkgs.ivy-posframe
      epkgs.ivy-prescient
      # TODO: Add from package repository.
      # epkgs.pulsar
      epkgs.desktop-environment
      epkgs.doom-themes
      epkgs.doom-modeline
    ];
  };

in {
  home.packages = [
    pkgs.nitrogen
    pkgs.autorandr
    pkgs.pass
    pkgs.mu
    pkgs.isync
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
}
