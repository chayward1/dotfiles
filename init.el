(defvar dotfiles/font
  "Fira Code"
  "Unified system font family, used on all font faces.")

(defvar dotfiles/font-size
  96
  "Unified font size, of which all variations are relative to.")

(defvar dotfiles/browser
  (getenv "BROWSER")
  "The default browser used by the system.")

(defvar dotfiles/language
  (getenv "LANG")
  "The default system language.")

(defconst dotfiles/modules-available
  '(core editor desktop writing projects interface)
  "All of the available modules for hosts to load.")

(defvar dotfiles/modules
  dotfiles/modules-available
  "Enabled modules, modify this in your host configuration.")

(defvar dotfiles/home
  user-emacs-directory
  "Original value of `user-emacs-directory'.")

(defvar dotfiles/cache
  (expand-file-name "~/.cache/emacs")
  "Where `user-emacs-directory' will be redirected.")

(defvar dotfiles/idle
  0.0
  "Length of time to wait before offering completions.")

(defvar dotfiles/leader-key
  "SPC"
  "Custom leader key for custom actions.")

(defvar dotfiles/leader-key-global
  (concat "C-" dotfiles/leader-key)
  "Global leader key available everywhere.")

(defvar dotfiles/projects
  (expand-file-name "~/.local/source/")
  "Location where source code projects are stored.")

(defvar dotfiles/passwords
  (expand-file-name "~/.password-store/")
  "Directory containing the password store.")

;; Load the host configuration.
(let ((host-file (concat dotfiles/home "/hosts/" system-name ".el")))
  (when (file-exists-p host-file)
    (load-file host-file)))

;; Load the enabled modules.
(dolist (m dotfiles/modules)
  (let ((mod-file (concat dotfiles/home "/modules/" (symbol-name m) ".el")))
    (when (file-exists-p mod-file)
      (load-file mod-file))))
