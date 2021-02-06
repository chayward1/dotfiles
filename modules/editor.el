(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package which-key
  :diminish which-key-mode
  :custom (which-key-idle-delay dotfiles/idle)
  :config (which-key-mode))

(use-package evil
  :custom (evil-want-integration t)  ;; Required for `evil-collection'.
          (evil-want-keybinding nil) ;; Same as above
  :config (evil-mode 1))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package evil-surround
  :config (global-evil-surround-mode 1))

(use-package evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

(use-package general
  :config
  (general-create-definer dotfiles/leader
    :states '(normal motion)
    :keymaps 'override
    :prefix dotfiles/leader-key
    :global-prefix dotfiles/leader-key-global))

(use-package hydra)

(dotfiles/leader
  "h" '(:ignore t :which-key "Help")
  "hp" '(describe-package :which-key "Package")
  "hv" '(describe-variable :which-key "Variable")
  "hf" '(describe-function :which-key "Function"))

(use-package all-the-icons)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(require 'dired-x)

(use-package dired-single
  :config (evil-collection-define-key 'normal 'dired-mode-map
            "h" 'dired-single-up-directory
            "l" 'dired-single-buffer))

(dotfiles/leader
  "d" '(dired-jump :which-key "Dired"))

(use-package eshell-prompt-extras
  :custom (eshell-highlight-prompt nil)
	        (eshell-prompt-function 'epe-theme-lambda))

(dotfiles/leader
  "e" '(eshell :which-key "Shell"))

(use-package magit
  :custom (magit-display-buffer-function
           #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge)

(dotfiles/leader
  "g" '(magit-status :which-key "Magit"))

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
  "." '(find-file :which-key "Files")
  "c" '(kill-buffer-and-window :which-key "Close"))

(dotfiles/leader
  "q" '(:ignore t :which-key "Quit")
  "qq" '(save-buffers-kill-emacs :which-key "Save")
  "qw" '(kill-emacs :which-key "Now")
  "qf" '(delete-frame :which-key "Frame"))

(dotfiles/leader
  "t" '(:ignore t :which-key "Tweaks"))
