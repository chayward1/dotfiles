(defconst dotfiles/modules-p
  '(org trash keys evil dired magit
    shell mu4e elfeed eshell vterm
    gpg pass x11 exwm roam agenda
    spelling grammar reveal hugo
    capture projects docker lsp dap
    cc go uml conf python fonts ivy
    themes modeline dashboard) 
  "All of the available modules.")

(defvar dotfiles/modules dotfiles/modules-p
  "All of the enable modules, default value equal to `dotfiles/modules-p'.")

(defvar dotfiles/language (getenv "LANG")
  "Default system dictionary language.")

(defvar dotfiles/font "Fira Code"
  "Unified system font family.")

(defvar dotfiles/font-size 96
  "Unified system font size.")

(defvar dotfiles/idle 0.0
  "Delay time before offering suggestions and completions.")

(defvar dotfiles/leader-key "SPC"
  "The all-powerful leader key.")

(defvar dotfiles/leader-key-global
  (concat "C-" dotfiles/leader-key)
  "Global prefix for the all-powerful leader key.")

(defvar dotfiles/projects
  (or (getenv "DOTFILES_PROJECTS")
      (expand-file-name "~/.local/source"))
  "Location of source code projects.")

(defvar dotfiles/passwords
  (or (getenv "DOTFILES_PASSWORDS")
      (expand-file-name "~/.password-store"))
  "Location of the local password store.")

(defvar dotfiles/public-key "37AB1CB72B741E478CA026D43025DCBD46F81C0F"
  "GPG kley to encrpy org files for/to.")
