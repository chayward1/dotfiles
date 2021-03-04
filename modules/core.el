;; Startup

;; Emacs creates a lot of files relative to ~user-emacs-directory~, these files are not part of this immutable configuration and do not belong in the emacs directory. How can we solve this issue? Shortly after initialization, before most packages load, we change the value to ~dotfiles/cache~. I elaborate more on the technique in my post [[https://chrishayward.xyz/posts/immutable-emacs/][Immutable Emacs]].


(setq user-emacs-directory dotfiles/cache)



;; Because this project uses version-control, we can disable more unwanted features:
;; + Lock files
;; + Backup files


(setq create-lockfiles nil
      make-backup-files nil)

;; Packages

;; Download and install packages using [[https://github.com/raxod502/straight.el][straight.el]], a functional package manager that integrates with =use-package=, giving us more control over sourcing our packages.

;; + Use the development branch
;; + Integrate with ~use-package~
  
;; Apply the configurations prior to bootstrapping the package manager, by setting (writing) to the variables that =straight= will ultimately read from.
  

(setq straight-repository-branch "develop"
      straight-use-package-by-default t)



;; Bootstrap the package manager, downloading, installing, or configuring depending on the state of the configuration. All packages build from source, pinned to specific git commit hashes.
  

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



;; Complete the integration with ~use-package~ by installing it with =straight=.
  

(straight-use-package 'use-package)

;; Cleanup

;; Despite having our *stateful* and *immutable* configurations seperate, it's good practice to make efforts to reduce the trash created by Emacs. Install [[https://github.com/emacscollective/no-littering][no-littering]] to reduce the files created by Emacs.


(use-package no-littering)



;; Emacs' default user interface is *horrendous*, let's do something about that.


(setq inhibit-startup-message t
      initial-scratch-message "")

(global-prettify-symbols-mode)

(when (window-system)
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1)))



;; Emacs has a long history of running on machines without gigabytes of available memory, let it realize its full potential! Just kidding, it just smashes *CPU0*.


(setq gc-cons-treshold most-positive-fixnum
      gnutls-min-prime-bits 4096)

;; Babel

;; *Organize your plain life in plain text*

;; [[https://orgmode.org][Org-mode]] is one of the hallmark features of Emacs, and provides the basis for my Literate Programming platform. It's essentially a markdown language with rich features for project management, scheduling, development, and writing. It's hard to convey everything within its capabilities.

;; + [[https://orgmode.org/worg/org-contrib/babel/languages/index.html][Babel languages]]
;; + [[https://orgmode.org/manual/Structure-Templates.html][Structure templates]]


(use-package org
  :hook (org-mode . (lambda ()
          (org-indent-mode)
          (visual-line-mode 1)
          (variable-pitch-mode 1)))
  :custom (org-ellipsis " ▾")
          (org-log-done 'time)
          (org-log-into-drawer t)
          (org-return-follows-link t)
          (org-image-actual-width nil)
          (org-directory dotfiles/home)
          (org-src-fontify-natively t)
          (org-src-tab-acts-natively t)
          (org-src-preserve-indentation t)
          (org-confirm-babel-evaluate nil)
          (org-todo-keywords '((sequence "TODO" "START" "WAIT" "DONE")))
  :config (require 'org-tempo)
          (add-to-list 'org-structure-template-alist '("s" . "src"))
          (add-to-list 'org-structure-template-alist '("q" . "quote"))
          (add-to-list 'org-structure-template-alist '("e" . "example"))
          (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
          (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
          (org-babel-do-load-languages 'org-babel-load-languages '((shell . t)
                                                                   (emacs-lisp . t))))



;; Build all of the =org= files within a given directory.


(defun dotfiles/tangle (dir)
  "Recursively tangle the Org files within a directory."
  (let ((org-files (directory-files-recursively dir "org")))
    (dolist (f org-files)
      (org-babel-tangle-file f))))
