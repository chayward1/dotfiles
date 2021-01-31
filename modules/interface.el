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
  "tf" '(hydra-text-scale/body :which-key "Font"))

(use-package linum-relative
  :init (setq linum-relative-backend
	      'display-line-numbers-mode)
  :config (linum-relative-global-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package doom-themes
  :init (load-theme 'doom-moonlight t))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 16)))

(dotfiles/leader
  "tt" '(load-theme t t :which-key "Theme"))

(setenv "BROWSER" "flatpak run org.mozilla.firefox")

;; (use-package fira-code-mode
;;   :config
;;   (global-fira-code-mode))

;; (use-package fira-code-mode
;;   :hook prog-mode)

(use-package dashboard
  :config
  (setq dashboard-center-content t
        dashboard-set-init-info t
        dashboard-set-file-icons t
        dashboard-set-heading-icons t
        dashboard-startup-banner 'logo
        dashboard-projects-backend 'projectile
        dashboard-items '((projects . 5)
                          (recents . 5)
                          (agenda . 5 )))
  (dashboard-setup-startup-hook))

(setq initial-buffer-choice
      (lambda ()
        (get-buffer "*dashboard*")))
