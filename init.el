(defun dotfiles/tangle (dir)
  "Recursively tangle the Org files within a directory."
  (let ((org-files (directory-files-recursively dir "org")))
    (dolist (f org-files)
      (org-babel-tangle-file f))))

(defvar dotfiles/font "Fira Code")
(defvar dotfiles/font-size 96)

(defvar dotfiles/idle 0.0)

(defvar dotfiles/leader-key "SPC")

(defvar dotfiles/src "~/.local/source/")
(defvar dotfiles/brain (concat dotfiles/src "brain"))

(defvar dotfiles/home user-emacs-directory)
(defvar dotfiles/cache "~/.cache/emacs")

(setq create-lockfiles nil
      make-backup-files nil
      user-emacs-directory dotfiles/cache)

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

(set-face-attribute 'default nil :font dotfiles/font :height dotfiles/font-size)
(set-face-attribute 'fixed-pitch nil :font dotfiles/font :height dotfiles/font-size)
(set-face-attribute 'variable-pitch nil :font dotfiles/font :height dotfiles/font-size)

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
    :prefix dotfiles/leader-key))

(use-package hydra)

(use-package evil
  :init (setq evil-want-integration t
	            evil-want-keybinding nil)
  :config (evil-mode 1))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

(defhydra hydra-text-scale (:timeout 4)
  "Scale"
  ("j" text-scale-increase "Increase")
  ("k" text-scale-decrease "Decrease")
  ("f" nil "Finished" :exit t))

(dotfiles/leader
  "f" '(hydra-text-scale/body :which-key "Font"))

(dotfiles/leader
  "," '(switch-to-buffer :which-key "Buffer")
  "." '(find-file :which-key "File"))

(dotfiles/leader
  "q" '(:ignore t :which-key "Quit")
  "qq" '(save-buffers-kill-emacs :which-key "Save")
  "qw" '(kill-emacs :which-key "Now"))

(dotfiles/leader
  "w" '(:ignore t :which-key "Window")
  "ww" '(window-swap-states :which-key "Swap")
  "wd" '(kill-buffer-and-window :which-key "Delete")
  "wc" '(delete-window :which-key "Close")
  "wh" '(windmove-left :which-key "Left")
  "wj" '(windmove-down :which-key "Down")
  "wk" '(windmove-up :which-key "Up")
  "wl" '(windmove-right :which-key "Right")
  "ws" '(:ignore t :which-key "Split")
  "wsj" '(split-window-below :which-key "Down")
  "wsl" '(split-window-right :which-key "Right"))

(use-package linum-relative
  :init (setq linum-relative-backend
	      'display-line-numbers-mode)
  :config (linum-relative-global-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package magit
  :custom (magit-display-buffer-function
           #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge)

(dotfiles/leader
  "g" '(magit-status :which-key "Magit"))

(use-package all-the-icons)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(require 'dired-x)

(dotfiles/leader
  "d" '(dired-jump :which-key "Dired"))

(use-package eshell-prompt-extras
  :config (setq eshell-highlight-prompt nil
	            eshell-prompt-function 'epe-theme-lambda))

(dotfiles/leader
  "e" '(eshell :which-key "Shell"))

(use-package doom-themes
  :init (load-theme 'doom-moonlight t))

(dotfiles/leader
  "t" '(load-theme t nil :which-key "Theme"))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 16)))

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

(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

(use-package lsp-mode
  :commands lsp
  :config
  (setq gc-cons-threshold 100000000
        lsp-completion-provider 'company-capf
        lsp-idle-delay dotfiles/idle))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom (lsp-ui-doc-position 'at-point))

(use-package dap-mode)

(use-package company-lsp
  :commands company-lsp
  :custom (company-minimum-prefix-length 1))

(use-package python-mode
  :hook (python-mode . lsp)
  :config (require 'dap-python)
  :custom (python-shell-interpreter "python3") ;; Required if "python" is not python 3.
          (dap-python-executable "python3")    ;; Same as above.
          (dap-python-debugger 'debugpy))

(use-package org-roam
  :hook (after-init . org-roam-mode)
  :custom (org-roam-directory dotfiles/brain))

(use-package org-roam-server
  :hook (org-roam-mode . org-roam-server-mode))

(dotfiles/leader
  "r" '(:ignore t :which-key "Roam")
  "rf" '(org-roam-find-file :which-key "Find")
  "rb" '(org-roam-buffer-toggle-display :which-key "Buffer")
  "rc" '(org-roam-capture :which-key "Capture")
  "rd" '(:ignore t :which-key "Dailies")
  "rdd" '(org-roam-dailies-find-date :which-key "Date")
  "rdt" '(org-roam-dailies-find-today :which-key "Today")
  "rdm" '(org-roam-dailies-find-tomorrow :which-key "Tomorrow")
  "rdy" '(org-roam-dailies-find-yesterday :which-ley "Yesterday"))

(setq org-roam-capture-templates
      '(("d" "default" plain (function org-roam-capture--get-point)
         "%?"
         :file-name "${slug}"
         :head "#+TITLE: ${title}\n"
         :unnarrowed t)))

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry (function org-roam-capture--get-point)
         "* %?"
         :file-name "daily/%<%Y-%m-%d>"
         :head "#+TITLE: %<%Y-%m-%d>\n")))

(use-package ox-reveal
  :custom (org-reveal-root "https://cdn.jsdelivr.net/reveal.js/3.9.2/"))
