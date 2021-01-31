(defvar dotfiles/modules '(core
                           desktop
                           writing
                           projects
                           interface))

(defvar dotfiles/cache "~/.cache/emacs")

(defvar dotfiles/idle 0.0)

(defvar dotfiles/leader-key "SPC")
(defvar dotfiles/leader-key-global "C-SPC")

(defvar dotfiles/src "~/.local/source/")

(defvar dotfiles/brain (concat dotfiles/src "brain/"))
(defvar dotfiles/notes (concat dotfiles/brain "notes/"))
(defvar dotfiles/bib (concat dotfiles/brain "resources.bib"))

(defvar dotfiles/secrets (concat dotfiles/src "secrets/"))
(defvar dotfiles/passwords (concat dotfiles/src "passwords/"))
