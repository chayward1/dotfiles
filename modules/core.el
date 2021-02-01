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

(setq inhibit-startup-message t)
(global-prettify-symbols-mode)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

(use-package org
  :hook
  (org-mode . (lambda ()
		      (org-indent-mode)
		      (visual-line-mode 1)
		      (variable-pitch-mode 1)))
  :config
  (setq org-ellipsis " ▾"
	      org-log-done 'time
	      org-log-into-drawer t
        org-directory dotfiles/home
	      org-src-preserve-indentation t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (python . t)
     (emacs-lisp . t)))

  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("s" . "src"))
  (add-to-list 'org-structure-template-alist '("q" . "quote"))
  (add-to-list 'org-structure-template-alist '("e" . "example"))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))

(defun dotfiles/tangle (dir)
  "Recursively tangle the Org files within a directory."
  (interactive)
  (let ((org-files (directory-files-recursively dir "org")))
    (dolist (f org-files)
      (org-babel-tangle-file f))))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package which-key
  :diminish which-key-mode
  :init (which-key-mode)
  :config (setq which-key-idle-delay dotfiles/idle))

(use-package general
  :config
  (general-create-definer dotfiles/leader
    :states '(normal motion)
    :keymaps 'override
    :prefix dotfiles/leader-key
    :global-prefix dotfiles/leader-key-global))

(use-package hydra)

(use-package evil
  :init (setq evil-want-integration t
	            evil-want-keybinding nil)
  :config (evil-mode 1))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package evil-surround
  :config (global-evil-surround-mode 1))

(use-package evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

(dotfiles/leader
  "." '(find-file :which-key "Files")
  "," '(switch-to-buffer :which-key "Buffers")
  "c" '(kill-buffer-and-window :which-key "Close"))

(dotfiles/leader
  "h" '(:ignore t :which-key "Help")
  "hp" '(describe-package :which-key "Package")
  "hv" '(describe-variable :which-key "Variable")
  "hf" '(describe-function :which-key "Function"))

(dotfiles/leader
  "q" '(:ignore t :which-key "Quit")
  "qq" '(save-buffers-kill-emacs :which-key "Save")
  "qw" '(kill-emacs :which-key "Now")
  "qf" '(delete-frame :which-key "Frame"))

(dotfiles/leader
  "w" '(:ignore t :which-key "Window")
  "ww" '(window-swap-states :which-key "Swap")
  "wc" '(delete-window :which-key "Close")
  "wh" '(windmove-left :which-key "Left")
  "wj" '(windmove-down :which-key "Down")
  "wk" '(windmove-up :which-key "Up")
  "wl" '(windmove-right :which-key "Right")
  "ws" '(:ignore t :which-key "Split")
  "wsj" '(split-window-below :which-key "Down")
  "wsl" '(split-window-right :which-key "Right"))

(dotfiles/leader
  "t" '(:ignore t :which-key "Tweaks"))

(use-package magit
  :custom (magit-display-buffer-function
           #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge)

(dotfiles/leader
  "g" '(magit-status :which-key "Magit"))

(use-package eshell-prompt-extras
  :config (setq eshell-highlight-prompt nil
	            eshell-prompt-function 'epe-theme-lambda))

(dotfiles/leader
  "e" '(eshell :which-key "Shell"))

(use-package all-the-icons)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(require 'dired-x)

(use-package dired-single
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(dotfiles/leader
  "d" '(dired-jump :which-key "Dired"))
