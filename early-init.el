;; Emacs creates a lot of files relative to `user-emacs-directory'.
;; These files are not part of this immutable configuration and do not belong in the emacs directory.

(defconst dotfiles/home
  (or (getenv "DOTFILES_HOME")
      (expand-file-name user-emacs-directory)))

(defconst dotfiles/cache
  (or (getenv "DOTFILES_CACHE")
      (expand-file-name "~/.cache/emacs")))

;; How can we solve this issue?

(unless (file-exists-p dotfiles/cache)
  (make-directory dotfiles/cache t))

;; Shortly after initialization, before most packages load, we change the value to `dotfiles/cache'.
;; I elaborate more on the technique in my post https://chrishayward.xyz/immutable-emacs/.

(setq user-emacs-directory dotfiles/cache)

;; Disable error messages for packages that do not yet support native compilation.

(setq comp-async-report-warnings-errors nil)

;; Because this project uses version-control, we can disable more unwanted features:

;; + Lock files
;; + Backup files

(setq make-backup-files nil
      create-lockfiles nil)

;; Download and instll packages using https://github.com/raxod502/straight.el
;; It's a functional package manager that integrates with https://github.com/jwiegley/use-package

;; + Use the development branch
;; + Integrate with use-package

;; Apply the configurations prior to bootstrapping the package manager.

(setq straight-repository-branch "develop"
      straight-use-package-by-default t
      package-enable-at-startup nil)

;; Bootstrap the package manager.
;; Download, Install, or Configuring depending on the state of the configuration.
;; All packages build from source, pinned to specific git commit hashes.

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

;; Integrate with use-package by installing it with straight. Override some package sources to
;; avoid the default package shipped with Emacs.

(straight-use-package 'use-package)
(straight-use-package 'no-littering)
(straight-use-package '(org :local-repo nil))
