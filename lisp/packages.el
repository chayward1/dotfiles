;; Download and instll packages using https://github.com/raxod502/straight.el
;; It's a functional package manager that integrates with https://github.com/jwiegley/use-package

;; + Use the development branch
;; + Integrate with use-package

;; Apply the configurations prior to bootstrapping the package manager.

(setq straight-repository-branch "master"
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
