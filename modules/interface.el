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
  :custom (linum-relative-backend 'display-line-numbers-mode)
  :config (linum-relative-global-mode))

(dotfiles/leader
  "tl" '(linum-relative-global-mode :which-key "Lines"))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package doom-themes
  :init (load-theme 'doom-moonlight t))

(use-package doom-modeline
  :custom (doom-modeline-height 16)
  :config (doom-modeline-mode 1))

(dotfiles/leader
  "tt" '(load-theme t t :which-key "Theme"))

(use-package fira-code-mode
  :hook (prog-mode org-mode))

(dotfiles/leader
  "tg" '(global-fira-code-mode :which-key "Ligatures"))

(use-package dashboard
  :custom (dashboard-center-content t)
          (dashboard-set-init-info t)
          (dashboard-set-file-icons t)
          (dashboard-set-heading-icons t)
          (dashboard-set-navigator t)
          (dashboard-startup-banner 'logo)
          (dashboard-projects-backend 'projectile)
          (dashboard-items '((projects . 5)
                             (recents . 5)
                             (agenda . 5 )))
  :config (dashboard-setup-startup-hook))

(setq dashboard-navigator-buttons
      `(;; First row.
        ((,(all-the-icons-fileicon "brain" :height 1.1 :v-adjust 0.0)
         "Brain"
         "Knowledge base"
         (lambda (&rest _) (browse-url "http://localhost:8080"))))
        ;; Second row.
        ((,(all-the-icons-material "public" :height 1.1 :v-adjust 0.0)
         "Homepage"
         "Personal website"
         (lambda (&rest _) (browse-url "https://chrishayward.xyz"))))
        ;; Third row.
        ((,(all-the-icons-faicon "university" :height 1.1 :v-adjust 0.0)
         "Athabasca"
         "Univeristy login"
         (lambda (&rest _) (browse-url "https://login.athabascau.ca/cas/login"))))
        ;; Fourth row.
        ((,(all-the-icons-faicon "book" :height 1.1 :v-adjust 0.0)
         "Bookshelf"
         "Vitalsource bookshelf"
         (lambda (&rest _) (browse-url "https://online.vitalsource.com"))))))

(setq initial-buffer-choice
      (lambda ()
        (get-buffer "*dashboard*")))
