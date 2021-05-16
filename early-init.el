;; This file is controlled by README.org
;; Please make any modifications there.

;; The original value of `user-emacs-directory' prior to redirection.
(defconst dotfiles/home
  (or (getenv "DOTFILES_HOME")
      (expand-file-name user-emacs-directory)))

;; The redirection target of `user-emacs-directory' during initialization.
(defconst dotfiles/cache
  (or (getenv "DOTFILES_CACHE")
      (expand-file-name "~/.cache/emacs")))

;; Make sure `dotfiles/cache' is a valid directory.
(unless (file-exists-p dotfiles/cache)
  (make-directory dotfiles/cache t))

;; Redirect the value of `user-emacs-directory'.
(setq user-emacs-directory dotfiles/cache)

;; Disable error messages for packages that don't support native-comp.
(setq comp-async-report-warnings-errors nil)

;; Disable unwanted features.
(setq make-backup-files nil
      create-lockfiles nil)

;; Apply the configurations prior to bootstrapping the package manager.
(setq straight-repository-branch "master"
      straight-use-package-by-default t
      package-enable-at-startup nil)

;; Bootstrap the package manager.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Integrate with `use-package' by installing it via `straight'.
(straight-use-package 'use-package)

;; Specify core package sources.
(straight-use-package 'no-littering)
(straight-use-package '(org :local-repo nil))

;; All of the modules available sorted in their default load order.
(defconst dotfiles/modules-p
  '(org trash keys evil dired magit
    shell mu4e elfeed eshell vterm
    gpg pass x11 exwm roam agenda
    spelling grammar reveal hugo
    capture projects docker lsp dap
    cc go uml conf python fonts ivy
    themes modeline dashboard))

;; All of the enabled modules.
(defvar dotfiles/modules dotfiles/modules-p)

;; The default system language.
(defvar dotfiles/language (getenv "LANG"))

;; Configure a unified system font.
(defvar dotfiles/font "Fira Code")

;; Default system font size.
(defvar dotfiles/font-size 96)

;; Delay time before offering suggestions and completions.
(defvar dotfiles/idle 0.0)

;; The all powerful leader key.
(defvar dotfiles/leader-key "SPC")

;; Global prefix for the leader key under X11 windows.
(defvar dotfiles/leader-key-global
  (concat "C-" dotfiles/leader-key))

;; The location on disk of source code projects.
(defvar dotfiles/projects
  (or (getenv "DOTFILES_PROJECTS")
      (expand-file-name "~/.local/source")))

;; The location on disk of the local copy of the password store.
(defvar dotfiles/passwords
  (or (getenv "DOTFILES_PASSWORDS")
      (expand-file-name "~/.password-store")))

;; The public GPG key to encrpyt files, and emails for / to / with.
(defvar dotfiles/public-key "37AB1CB72B741E478CA026D43025DCBD46F81C0F")

;; Load a host configuration.
(defun dotfiles/load-host (host-name)
  "Load the host configuration file for the system `host-name'."
  (interactive)
  (let ((host-file (concat dotfiles/home "/hosts/" host-name ".org")))
    (when (file-exists-p host-file)
      (org-babel-load-file host-file))))

;; Load a module definition.
(defun dotfiles/load-modules (modules)
  "Load the `modules' in sequential order."
  (interactive)
  (dolist (m modules)
    (let ((mod-file (concat dotfiles/home "/modules/" (symbol-name m) ".org")))
      (when (file-exists-p mod-file)
	(org-babel-load-file mod-file)))))
