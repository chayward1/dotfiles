# This file is controlled by /etc/dotfiles/README.org
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
      epkgs.org-roam-server
      epkgs.org-drill
      epkgs.org-pomodoro
      epkgs.writegood-mode
      epkgs.ox-hugo
      epkgs.ox-reveal
      epkgs.password-store
      epkgs.mu4e-alert
      epkgs.dired-single
      epkgs.all-the-icons
      epkgs.all-the-icons-dired
      epkgs.emojify
      epkgs.eshell-prompt-extras
      epkgs.vterm
      epkgs.magit
      epkgs.elfeed
      epkgs.nix-mode
      epkgs.projectile
      epkgs.lsp-mode
      epkgs.lsp-ui
      epkgs.company
      epkgs.go-mode
      epkgs.pretty-mode
      epkgs.plantuml-mode
      epkgs.ivy
      epkgs.counsel
      epkgs.ivy-rich
      epkgs.ivy-posframe
      epkgs.ivy-prescient
      epkgs.doom-themes
      epkgs.doom-modeline
    ];
  };

in {
  home.packages = [
    pkgs.nitrogen
    pkgs.autorandr
    pkgs.hugo
    pkgs.pass
    pkgs.mu
    pkgs.isync
    pkgs.aspell
    pkgs.aspellDicts.en
    pkgs.aspellDicts.en-science
    pkgs.aspellDicts.en-computers
    pkgs.plantuml
    pkgs.nixfmt
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
