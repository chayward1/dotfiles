;; Options

;; Here's a complete list of all of the options configurable for each host, and their default values. Override any of these configurations in a host file. All of the values are prefixed with ~dotfiles/~. If you need to make configurations to another variable, consider creating a new option. Here are a few examples to get started:

;; + [[file:hosts/localhost.org][Termux]]
;; + [[file:hosts/raspberry.org][Raspberry]]
;; + [[file:hosts/acernitro.org][Acernitro]]
;; + [[file:hosts/virtualbox.org][Virtualbox]]

;; | Name                       | Description                                       |
;; |----------------------------+---------------------------------------------------|
;; | dotfiles/font              | Unified system font family                        |
;; | dotfiles/font-size         | System wide base font size                        |
;; | dotfiles/browser           | Browser to open URL links                         |
;; | dotfiles/language          | Dictionary language to load                       |
;; | dotfiles/modules-p         | Immutable list of all available modules           |
;; | dotfiles/modules           | Enabled custom modules                            |
;; | dotfiles/home              | Original value of `user-emacs-directory'          |
;; | dotfiles/cache             | Redirection target of `user-emacs-directory       |
;; | dotfiles/idle              | Delay time before offering completions            |
;; | dotfiles/leader-key        | All powerful keybinding prefix for custom actions |
;; | dotfiles/leader-key-global | Like the leader-key, but EVERYWHERE!              |
;; | dotfiles/projects          | Location of source code projects                  |
;; | dotfiles/passwords         | Location of the system password store             |
;; | dotfiles/public-key        | Public GPG key to encrypt files for               |


(defvar dotfiles/font "Fira Code")
(defvar dotfiles/font-size 96)
(defvar dotfiles/browser (getenv "BROWSER"))
(defvar dotfiles/language (getenv "LANG"))
(defconst dotfiles/modules-p '(core editor desktop writing projects interface))
(defvar dotfiles/modules dotfiles/modules-p)
(defvar dotfiles/home user-emacs-directory)
(defvar dotfiles/cache (expand-file-name "~/.cache/emacs"))
(defvar dotfiles/idle 0.0)
(defvar dotfiles/leader-key "SPC")
(defvar dotfiles/leader-key-global (concat "C-" dotfiles/leader-key))
(defvar dotfiles/projects (expand-file-name "~/.local/source/"))
(defvar dotfiles/passwords (expand-file-name "~/.password-store/"))
(defvar dotfiles/public-key "37AB1CB72B741E478CA026D43025DCBD46F81C0F")

;; Startup

;; The host configuration tangles, and loads (if it exist) using the systems name.


(let ((host-file (concat dotfiles/home "/hosts/" system-name ".org")))
  (when (file-exists-p host-file)
    (org-babel-load-file host-file)))



;; Breaking down the project into logical units or chapters to keep the code more maintainable and organized. This is also a fundamental requirement to achieve the goal of modularity. 


(dolist (m dotfiles/modules)
  (let ((mod-file (concat dotfiles/home "/modules/" (symbol-name m) ".org")))
    (when (file-exists-p mod-file)
      (org-babel-load-file mod-file))))
