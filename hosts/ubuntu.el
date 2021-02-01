(setenv "BROWSER" "flatpak run org.mozilla.firefox")

(defvar dotfiles/modules '(core
                           desktop
                           writing
                           projects
                           interface))

(defvar dotfiles/cache "~/.cache/emacs")
(defvar dotfiles/home user-emacs-directory)

(defvar dotfiles/idle 0.0)

(defvar dotfiles/leader-key "SPC")
(defvar dotfiles/leader-key-global "C-SPC")

(defvar dotfiles/src "~/.local/source/")

(defvar dotfiles/secrets (concat dotfiles/src "secrets/"))
(defvar dotfiles/passwords (concat dotfiles/src "passwords/"))
