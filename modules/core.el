(setq user-emacs-directory dotfiles/cache)

(setq create-lockfiles nil
      make-backup-files nil)

(setq straight-repository-branch "develop"
      straight-use-package-by-default t)

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

(straight-use-package 'use-package)

(use-package no-littering)

(setq inhibit-startup-message t
      initial-scratch-message "")

(global-prettify-symbols-mode)

(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tooltip-mode -1))

(setq gc-cons-treshold most-positive-fixnum
      gnutls-min-prime-bits 4096)

(use-package org
  :hook (org-mode . (lambda ()
          (org-indent-mode)
          (visual-line-mode 1)
          (variable-pitch-mode 1)))
  :custom (org-ellipsis " ▾")
          (org-log-done 'time)
          (org-log-into-drawer t)
          (org-image-actual-width nil)
          (org-directory dotfiles/home)
          (org-src-preserve-indentation t)
          (org-todo-keywords '((sequence "TODO" "START" "WAIT" "DONE")))
  :config (require 'org-tempo)
          (add-to-list 'org-structure-template-alist '("s" . "src"))
          (add-to-list 'org-structure-template-alist '("q" . "quote"))
          (add-to-list 'org-structure-template-alist '("e" . "example"))
          (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
          (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
          (org-babel-do-load-languages 'org-babel-load-languages '((shell . t)
                                                                   (emacs-lisp . t))))

(defun dotfiles/tangle (dir)
  "Recursively tangle the Org files within a directory."
  (let ((org-files (directory-files-recursively dir "org")))
    (dolist (f org-files)
      (org-babel-tangle-file f))))
