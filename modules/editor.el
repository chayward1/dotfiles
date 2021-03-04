;; Keys

;; Make the =ESC= key quit (most) prompts, instead of the default =C-g=.


(global-set-key (kbd "<escape>") 'keyboard-escape-quit)



;; Download [[https://github.com/justbur/emacs-which-key][which-key]], a package that displays the current incomplete keybinding input in a mini-buffer, showing available completion options.


(use-package which-key
  :diminish which-key-mode
  :custom (which-key-idle-delay dotfiles/idle)
  :config (which-key-mode))



;; Turn Emacs into Vim with [[https://evil.readthedocs.io/en/latest/index.html][evil-mode]], the extensible VI layer for Emacs.


(use-package evil
  :custom (evil-want-integration t)  ;; Required for `evil-collection'.
          (evil-want-keybinding nil) ;; Same as above
  :config (evil-mode 1))



;; Unfortunately the default keybindings are *lacking*, but there is a community curated package [[https://github.com/emacs-evil/evil-collection][evil-collection]], which does a much better job implementing keybindings you would expect to find.


(use-package evil-collection
  :after evil
  :config (evil-collection-init))



;; Surround text with functions, quotations, and any other symbols using the [[https://github.com/emacs-evil/evil-surround][evil-surround]] package.


(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))



;; Toggle block comments using [[https://github.com/redguardtoo/evil-nerd-commenter][evil-nerd-commentor]] and =M-;=.


(use-package evil-nerd-commenter
  :after evil
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))



;; Implement the *leader* key using [[https://github.com/noctuid/general.el][general.el]], letting us easily configure prefixed keybindings in a much cleaner manner than the default methods.


(use-package general
  :after evil
  :config
  (general-create-definer dotfiles/leader
    :states '(normal motion)
    :keymaps 'override
    :prefix dotfiles/leader-key
    :global-prefix dotfiles/leader-key-global))

 

;; Use [[https://github.com/abo-abo/hydra][hydra]] for transient keybindings sharing a common prefix.


(use-package hydra
  :defer t)

;; Help

;; Use the built-in ~describe-*~ functionality of Emacs to quickly access documentation for packages, variables, and functions. Run helper functions with =SPC h=.

;; + Packages =p=
;; + Variables =v=
;; + Functions =f=


(dotfiles/leader
  "h" '(:ignore t :which-key "Help")
  "hp" '(describe-package :which-key "Package")
  "hv" '(describe-variable :which-key "Variable")
  "hf" '(describe-function :which-key "Function"))

;; Files

;; For file navigation I use =dired=, included with Emacs by default. Dired feels more modern with prioritized icon fonts using [[https://github.com/domtronn/all-the-icons.el][all-the-icons]]. This makes navigation and visually parsing directories much faster, given that file types are quickly identified by their corresponding icons.


(use-package all-the-icons)



;; Integration with =dired= comes from the [[https://github.com/jtbm37/all-the-icons-dired][all-the-icons-dired]] package.


(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))



;; When opening =dired=, I don't want to have to press =RET= twice to navigate to the current directory. Avoid this with ~dired-jump~, included in the =dired-x= package shipped with =dired= and Emacs.


(require 'dired-x)



;; By default =dired= will create a new buffer everytime you press =RET= over a directory. This leads to unwanted =dired= buffers needing closure. Avoid this behaviour with [[https://github.com/crocket/dired-single][dired-single]], reusing the same dired buffer.

;; + Move up a directory with =h=
;; + Open a single buffer with =l=


(use-package dired-single
  :config (evil-collection-define-key 'normal 'dired-mode-map
            "h" 'dired-single-up-directory
            "l" 'dired-single-buffer))



;; Open a dired buffer with =SPC d=.


(dotfiles/leader
  "d" '(dired-jump :which-key "Dired"))

;; Shell

;; While not a traditional terminal emulator, =eshell= provides me with all of the functionality I expect and require from one. Some users may be wanting more, I would recommend they look into =vterm= included in the destkop module. Configure the infamous lambda prompt using [[https://github.com/zwild/eshell-prompt-extras][eshell-prompt-extras]] package.


(use-package eshell-prompt-extras
  :custom (eshell-highlight-prompt nil)
	        (eshell-prompt-function 'epe-theme-lambda))



;; Open an =eshell= buffer with =SPC e=.


(dotfiles/leader
  "e" '(eshell :which-key "Shell"))

;; Source

;; #+ATTR_ORG: :width 420px
;; #+ATTR_HTML: :width 420px
;; #+ATTR_LATEX: :width 420px
;; [[./docs/images/2021-02-13-example-magit.gif]]

;; Another hallmark feature is [[https://github.com/magit/magit][Magit]], a complete git porcelain within Emacs.


(use-package magit
  :commands magit-status
  :custom (magit-display-buffer-function
           #'magit-display-buffer-same-window-except-diff-v1))



;; Work directly with github issues / pull requests using [[https://github.com/magit/forge][Forge]].

;; + Requires a valid ~$GITHUB_TOKEN~


(use-package forge
  :after magit)



;; Open the *status* page for the current repository with =SPC g=.


(dotfiles/leader
  "g" '(magit-status :which-key "Magit"))

;; Windows

;; Window management with =SPC w=.
;; + Swap with =w=
;; + Close with =c=
;; + Motions with =h,j,k,l=
;; + Split with =s + <MOTION>=


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

;; Shortcuts

;; Implement shortcut bindings, cherry picked from Doom emacs.

;; + Close buffers with =SPC c=
;; + Find files with =SPC . (period)=


(dotfiles/leader
  "." '(find-file :which-key "Files")
  "c" '(kill-buffer-and-window :which-key "Close"))



;; Quit emacs with =SPC q=.
;; + Saving =q=
;; + Without =w=
;; + Frame (daemon) =f=


(dotfiles/leader
  "q" '(:ignore t :which-key "Quit")
  "qq" '(save-buffers-kill-emacs :which-key "Save")
  "qw" '(kill-emacs :which-key "Now")
  "qf" '(delete-frame :which-key "Frame"))



;; Place runtime tweaks behind =SPC t=.


(dotfiles/leader
  "t" '(:ignore t :which-key "Tweaks"))
