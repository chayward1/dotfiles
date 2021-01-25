(defun dotfiles/tangle (dir)
  "Recursively tangle the Org files within a directory."
  (interactive)
  (let ((org-files (directory-files-recursively dir "org")))
    (dolist (f org-files)
      (org-babel-tangle-file f))))

(defvar dotfiles/home user-emacs-directory)

(defvar dotfiles/cache "~/.cache/emacs")

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

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(defvar dotfiles/idle 0.0)

(use-package which-key
  :diminish which-key-mode
  :init (which-key-mode)
  :config (setq which-key-idle-delay dotfiles/idle))

(defvar dotfiles/leader-key "SPC")
(defvar dotfiles/leader-key-global "C-SPC")

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

(defvar dotfiles/font "Fira Code")
(defvar dotfiles/font-size 96)

(set-face-attribute 'default nil :font dotfiles/font :height dotfiles/font-size)
(set-face-attribute 'fixed-pitch nil :font dotfiles/font :height dotfiles/font-size)
(set-face-attribute 'variable-pitch nil :font dotfiles/font :height dotfiles/font-size)

(defhydra hydra-text-scale (:timeout 4)
  "Scale"
  ("j" text-scale-increase "Increase")
  ("k" text-scale-decrease "Decrease")
  ("f" nil "Finished" :exit t))

(dotfiles/leader
  "f" '(hydra-text-scale/body :which-key "Font"))

(use-package linum-relative
  :init (setq linum-relative-backend
	      'display-line-numbers-mode)
  :config (linum-relative-global-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package doom-themes
  :init (load-theme 'doom-moonlight t))

(dotfiles/leader
  "t" '(load-theme t nil :which-key "Theme"))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 16)))

(defun dotfiles/run (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun dotfiles/set-wallpaper (path)
  (interactive)
  (when (file-exists-p path)
    (let ((command (concat "feh --bg-scale " path)))
      (start-process-shell-command "feh" nil command))))

(defun dotfiles/init-hook ()
  (exwm-workspace-switch-create 1)
  (setq display-time-and-date t)
  (display-battery-mode 1)
  (display-time-mode 1))

(defun dotfiles/update-display ()
  (dotfiles/run "autorandr --change --force")
  ;; (dotfiles/set-wallpaper "TODO")
)

(use-package exwm
  :config
  (require 'exwm-randr)
  (exwm-randr-enable)
  (add-hook 'exwm-init-hook #'dotfiles/init-hook)
  (add-hook 'exwm-randr-screen-change-hook #'dotfiles/update-display)
  (dotfiles/update-display)
  (setq exwm-input-prefix-keys
        '(?\M-x
          ?\C-g
          ?\C-\ )
        exwm-input-global-keys
        `(([?\s-r] . exwm-reset)
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "λ ")))
                       (start-process-shell-command command nil command)))
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 1 9))))
  (exwm-enable))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

(use-package mu4e
  :config
  (setq mu4e-change-filenames-when-moving t
        mu4e-update-interval (* 5 60) ;; Every 5 minutes.
        mu4e-get-mail-command "mbsync -a"
        mu4e-maildir "~/.cache/mail"
        mu4e-compose-signature 
          (concat "Chris Hayward\n"
                  "https://chrishayward.xyz\n"))

  ;; Ensure plain text scales for all devices.
  (setq mu4e-compose-format-flowed t)

  ;; GPG signing key for outbound mail.
  (setq mml-secure-openpgp-signers '("37AB1CB72B741E478CA026D43025DCBD46F81C0F"))
  (add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime)

  (setq message-send-mail-function 'smtpmail-send-it)  

  ;; Configure mail account(s).
  (setq mu4e-contexts
    (list
      ;; Main
      ;; chris@chrishayward.xyz
      (make-mu4e-context
        :name "Main"
        :match-func
          (lambda (msg)
            (when msg 
              (string-prefix-p "/Main" (mu4e-message-field msg :maildir))))
        :vars
          '((user-full-name . "Christopher James Hayward")
            (user-mail-address . "chris@chrishayward.xyz")
            (smtpmail-smtp-server . "mail.chrishayward.xyz")
            (smtpmail-smtp-service . 587)
            (smtpmail-stream-type . starttls))))))

(dotfiles/leader
  "m" '(mu4e :which-key "Mail"))

(use-package org-roam
  :hook (after-init . org-roam-mode)
  :custom (org-roam-directory "~/.local/source/brain"))

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
  "rdy" '(org-roam-dailies-find-yesterday :which-key "Yesterday"))

(setq org-roam-capture-templates
      '(("d" "Default" plain (function org-roam-capture--get-point)
         "%?"
         :file-name "${slug}"
         :head "#+TITLE: ${title}\n"
         :unnarrowed t)))

(setq org-roam-dailies-capture-templates
      '(("d" "Default" entry (function org-roam-capture--get-point)
         "* %?"
         :file-name "daily/%<%Y-%m-%d>"
         :head "#+TITLE: %<%Y-%m-%d>\n")))

(defvar dotfiles/bib "~/.local/source/brain/resources.bib")
(defvar dotfiles/notes "~/.local/source/brain/notes/")

(use-package org-noter
  :after org
  :config
  (setq org-noter-always-create-frame nil
        org-noter-notes-search-path dotfiles/notes))

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter-pdftools
  :after org-noter
  :config
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-active-handler-functions #'org-noter-pdftools-jump-to-note)))

(setq bibtex-completion-notes-path dotfiles/notes
      bibtex-completion-bibliography dotfiles/bib
      bibtex-completion-pdf-field "file"
      bibtex-completion-notes-template-multiple-files
      (concat
        "#+TITLE: ${title}\n"
        "#+ROAM_KEY: cite:${=key=}\n"
        "#* TODO Notes\n"
        ":PROPERTIES:\n"
        ":CUSTOM_ID: ${=key}\n"
        ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
        ":AUTHOR: ${author-abbrev}\n"
        ":JOURNAL: ${journaltitle}\n"
        ":DATE: ${date}\n"
        ":YEAR: ${year}\n"
        ":DOI: ${doi}\n"
        ":URL: ${url}\n"
        ":END:\n\n"))

(use-package org-ref
  :config
  (setq org-ref-completion-library 'org-ref-helm-cite
        org-ref-get-pdf-filename-function 'org-refg-get-pdf-filename-helm-bibtex
        org-ref-default-bibliography dotfiles/bib
        org-ref-bibliography-notes dotfiles/notes
        org-ref-notes-directory dotfiles/notes
        org-ref-notes-function 'orb-edit-notes
        org-ref-note-title-format "* TODO %y - %t\n:PROPERTIES:\n:CUSTOM_ID: %k\n:NOTER_DOCUMENT: %F\n:ROAM_KEY: cite:%k\n:AUTHOR: %9a\n:JOURNAL: %j\n:YEAR: %y\n:VOLUME: %v\n:PAGES: %p\n:DOI: %D\n:URL: %U\n:END:\n\n"))

(use-package org-roam-bibtex
  :after (org-roam)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq orb-preformat-keywords
        '("=key=" "title" "url" "file" "author-or-editor" "keywords")))

(add-to-list 'org-roam-capture-templates
             '("n" "Notes" plain (function org-roam-capture--get-point)
               ""
               :file-name "notes/${slug}"
               :head "#+TITLE: ${=key=}: ${title}\n#+ROAM_KEY:${ref}\n\n* ${title} :PROPERTIES:\n:CUSTOM_ID: ${=key=}\n:URL: ${url}\n:AUTHOR: ${author-or-editor}\n:NOTER_DOCUMENT:%(orb-process-file-field \"${=key=}\")\n:NOTER_PAGE:\n:END:\n\n"))

(setq org-agenda-files '("~/.local/source/brain/daily/"
                         "~/.local/source/secrets/org/"))

(dotfiles/leader
  "a" '(org-agenda :which-key "Agenda"))

(use-package ox-hugo 
  :after ox)

(add-to-list 'org-roam-capture-templates
             '("b" "Blogging" plain (function org-roam-capture--get-point)
               "%?"
               :file-name "posts/${slug}"
               :head "#+TITLE: ${title}\n#+HUGO_BASE_DIR: ~/.local/source/website\n#+HUGO_SECTION: posts\n"))

(use-package gif-screencast
  :custom
  (gif-screencast-output-directory "~/.local/source/brain/screen/"))

(dotfiles/leader
  "s" '(:ignore t :which-key "Screencast")
  "ss" '(gif-screencast-start-or-stop :which-key "Start / Stop")
  "sp" '(gif-screencast-toggle-pause :which-key "Pause"))

(use-package ox-reveal
  :after ox
  :custom (org-reveal-root "https://cdn.jsdelivr.net/reveal.js/3.9.2/"))

(add-to-list 'org-roam-capture-templates
             '("p" "Presentation" plain (function org-roam-capture--get-point)
               "%?"
               :file-name "slides/${slug}"
               :head "#+TITLE: ${title}\n"))

(use-package lsp-mode
  :custom (gc-cons-threshold 1000000000)
          (lsp-idle-delay 0.500))

(use-package lsp-ui
  :custom (lsp-ui-doc-position 'at-point)
          (lsp-ui-doc-delay 0.500))

(use-package password-store
  :custom (password-store-dir "~/.local/source/passwords"))

(use-package dap-mode)

(use-package company)
(use-package company-lsp)

(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))

(use-package python-mode
  :hook (python-mode . lsp)
  :config (require 'dap-python)
  :custom (python-shell-interpreter "python3") ;; Required if "python" is not python 3.
          (dap-python-executable "python3")    ;; Same as above.
          (dap-python-debugger 'debugpy))

(use-package rustic)

(use-package go-mode
  :hook (go-mode . lsp))

(defun dotfiles/go-hook ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'go-mode-hook #'dotfiles/go-hook)
