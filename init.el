;; Options

;; Here's a complete list of all of the options configurable for each host, and their default values. All variables prefixed with ~dotfiles/~. If you need to make configurations to another variable, consider creating a new option. 


(defvar dotfiles/font 
  "Fira Code" 
  "Unified system font family.")

(defvar dotfiles/font-size 
  96 
  "Unified system font size.")

(defvar dotfiles/browser 
  (getenv "BROWSER") 
  "Default system web browser.")

(defvar dotfiles/language 
  (getenv "LANG") 
  "Default system dictionary language.")

(defconst dotfiles/modules-p 
  '(core 
    editor
    email
    encryption
    desktop
    writing
    website
    capture
    projects
    interface) 
  "All of the available modules.")

(defvar dotfiles/modules 
  dotfiles/modules-p 
  "All of the enabled modules.")

(defvar dotfiles/home 
  user-emacs-directory 
  "Original value of `user-emacs-directory'.")

(defvar dotfiles/cache 
  (expand-file-name "~/.cache/emacs") 
  "Redirection target of `user-emacs-directory'.")

(defvar dotfiles/idle 
  0.0 
  "Delay time before offering suggestions and completions.")

(defvar dotfiles/leader-key 
  "SPC" 
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

;; Startup

;; This project makes heavy use of modern features and libraries. Since *Babel's* used in initialization, *Org* must load prior to importing any of custom modules. This introduces a unique *chicken before the egg* problem. My solution included some initialization code in *Emacs Lisp* called before using any *Babel* APIs.


(load-file "~/.emacs.d/bin/cleanup.el")
(load-file "~/.emacs.d/bin/packages.el")

;; Hosts

;; Each host machines configuration loaded immediately after declaring the options, before applying any configuration. This allows system to system control while remaining immutable. Override any of the available options configurations in a host file. Here's some examples to get started:

;; + [[file:hosts/localhost.org][Termux]]
;; + [[file:hosts/raspberry.org][Raspberry]]
;; + [[file:hosts/acernitro.org][Acernitro]]
;; + [[file:hosts/virtualbox.org][Virtualbox]]

;; Begin the process by loading any host specific overrides. The host configuration tangles, and loads (if it exist) using the systems name.


(let ((host-file (concat dotfiles/home "/hosts/" system-name ".org")))
  (when (file-exists-p host-file)
    (org-babel-load-file host-file)))

;; Module

;; Breaking down the project into logical units or chapters to keep the code more maintainable and organized. This is also a fundamental requirement to achieve the goal of modularity. Here are all of the available modules, also listed in the variable ~dotfiles/modules-p~. 

;; + [[file:modules/core.org][Core]]
;; + [[file:modules/editor.org][Editor]]
;; + [[file:modules/email.org][Email]]
;; + [[file:modules/encryption.org][Encryption]]
;; + [[file:modules/desktop.org][Desktop]]
;; + [[file:modules/writing.org][Writing]]
;; + [[file:modules/website.org][Website]]
;; + [[file:modules/capture.org][Capture]]
;; + [[file:modules/projects.org][Projects]]
;; + [[file:modules/interface.org][Interface]]

;; By default all of the modules will load, override the variable ~dotfiles/modules~ in a host configuration to override this.


(dolist (m dotfiles/modules)
  (let ((mod-file (concat dotfiles/home "/modules/" (symbol-name m) ".org")))
    (when (file-exists-p mod-file)
      (org-babel-load-file mod-file))))
