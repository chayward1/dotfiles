;; Options

;; Here's a complete list of all of the options configurable for each host, and their default values. If a host configuration does not exist, the default values will remain.

;; Configure the system font with a single ~font-family~ and define the size, of which variations to the font size are relative to this value.


(defvar dotfiles/font
  "Fira Code"
  "Unified system font family, used on all font faces.")

(defvar dotfiles/font-size
  96
  "Unified font size, of which all variations are relative to.")



;; Used by the desktop module to find the appropriate browser.


(defvar dotfiles/browser
  (getenv "BROWSER")
  "The default browser used by the system.")



;; Used by the writing module to determine the system language.


(defvar dotfiles/language
  (getenv "LANG")
  "The default system language.")



;; All of the available modules defined in the ~dotfiles/modules-available~ constant.


(defconst dotfiles/modules-available
  '(core editor desktop writing projects interface)
  "All of the available modules for hosts to load.")



;; Add the modules you want to initialize to the ~dotfiles/modules~ variable.


(defvar dotfiles/modules
  dotfiles/modules-available
  "Enabled modules, modify this in your host configuration.")



;; Specify the emacs home, and the cache directory.


(defvar dotfiles/home
  user-emacs-directory
  "Original value of `user-emacs-directory'.")



;; Used to seperate the immutable configuration from the stateful package files.


(defvar dotfiles/cache
  (expand-file-name "~/.cache/emacs")
  "Where `user-emacs-directory' redirects to.")



;; Functionality like =completion= and =hints= delayed to avoid popups for common manuevers.


(defvar dotfiles/idle
  0.0
  "Length of time to wait before offering completions.")



;; Required for the all powerful leader key.


(defvar dotfiles/leader-key
  "SPC"
  "Custom leader key for custom actions.")



;; The desktop module requires the global leader key set.


(defvar dotfiles/leader-key-global
  (concat "C-" dotfiles/leader-key)
  "Global leader key available everywhere.")



;; Define where the source repositories exist on disk, for integration with the projects module.


(defvar dotfiles/projects
  (expand-file-name "~/.local/source/")
  "Location where source code projects exist on disk.")



;; Where the password store exists on disk.


(defvar dotfiles/passwords
  (expand-file-name "~/.password-store/")
  "Directory containing the password store.")



;; Configure the public GPG key that Emacs will encrypt files to.


(defvar dotfiles/public-key
  "37AB1CB72B741E478CA026D43025DCBD46F81C0F"
  "Public PGP key that Emacs will encrypt files to.")

;; Startup

;; The host configuration loads (if it exist) using the systems name.


(let ((host-file (concat dotfiles/home "/hosts/" system-name ".el")))
  (when (file-exists-p host-file)
    (load-file host-file)))



;; Load all of the enabled modules:


(dolist (m dotfiles/modules)
  (let ((mod-file (concat dotfiles/home "/modules/" (symbol-name m) ".el")))
    (when (file-exists-p mod-file)
      (load-file mod-file))))
