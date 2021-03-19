;; All of the options available in the framework.
;; Please see README.org for more information.

(defvar dotfiles/home user-emacs-directory 
  "Original value of `user-emacs-directory'.")

(defvar dotfiles/cache (expand-file-name "~/.cache/emacs") 
  "Redirection target of `user-emacs-directory'.")

(defvar dotfiles/browser (getenv "BROWSER") 
  "Default system web browser.")

(defvar dotfiles/language (getenv "LANG") 
  "Default system dictionary language.")

(defconst dotfiles/modules-p 
  '(core 
    editor
    shell
    email 
    terminal
    encryption 
    desktop
    writing 
    presentations
    website 
    capture
    projects
    development 
    interface 
    dashboard) 
  "All of the available modules.")

(defvar dotfiles/modules dotfiles/modules-p 
  "All of the enabled modules.")

(defvar dotfiles/font "Fira Code" 
  "Unified system font family.")

(defvar dotfiles/font-size 96 
  "Unified system font size.")

(defvar dotfiles/idle 0.0 
  "Delay time before offering suggestions and completions.")

(defvar dotfiles/leader-key "SPC" 
  "All powerful leader key.")

(defvar dotfiles/leader-key-global 
  (concat "C-" dotfiles/leader-key) 
  "Global prefix for the leader key.")

(defvar dotfiles/projects 
  (expand-file-name "~/.local/source/") 
  "Location of source code projects.")

(defvar dotfiles/passwords 
  (expand-file-name "~/.password-store/") 
  "Location of local password store.")

(defvar dotfiles/public-key 
  "37AB1CB72B741E478CA026D43025DCBD46F81C0F" 
  "GPG key to encrypt org files for.")
