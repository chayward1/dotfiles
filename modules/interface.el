;; Ivy

;; Download and configure [[https://oremacs.com/swiper/][ivy]], a powerful selection menu for Emacs.


(use-package ivy
  :diminish
  :config (ivy-mode 1))



;; Counsel is a customized set of commands to replace built in completion buffers.


(use-package counsel
  :after ivy
  :custom (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config (counsel-mode 1))



;; Switch buffers with =SPC , (comma)=.


(dotfiles/leader
  "," '(counsel-switch-buffer :which-key "Buffers"))



;; Provide more information about each item with [[https://github.com/Yevgnen/ivy-rich][ivy-rich]].


(use-package ivy-rich
  :after counsel
  :init (ivy-rich-mode 1))

;; Fonts

;; Write out to all *3* of Emacs' default font faces.


(set-face-attribute 'default nil :font dotfiles/font :height dotfiles/font-size)
(set-face-attribute 'fixed-pitch nil :font dotfiles/font :height dotfiles/font-size)
(set-face-attribute 'variable-pitch nil :font dotfiles/font :height dotfiles/font-size)



;; Define a transient keybinding for scaling the text.
  

(defhydra hydra-text-scale (:timeout 4)
  "Scale"
  ("j" text-scale-increase "Increase")
  ("k" text-scale-decrease "Decrease")
  ("f" nil "Finished" :exit t))



;; Increase the font size in buffers with =SPC t f=.
;; + Increase =j=
;; + Decrease =k=
;; + Finish =f=


(dotfiles/leader
  "tf" '(hydra-text-scale/body :which-key "Font"))

;; Lines

;; Relative line numbers are important when using =VI= emulation keys. You can prefix most commands with a *number*, allowing you to jump up / down by a line count.

;; #+begin_example
;;   5:
;;   4:
;;   3:
;;   2:
;;   1:
;; 156: << CURRENT LINE >>
;;   1:
;;   2:
;;   3:
;;   4:
;;   5:
;; #+end_example

;; https://github.com/emacsmirror/linum-relative
;; + Integrate with ~display-line-numbers-mode~ for performance


(use-package linum-relative
  :commands (linum-relative-global-mode)
  :custom (linum-relative-backend 'display-line-numbers-mode))



;; Add line numbers to the toggles behind =SPC t l=.


(dotfiles/leader
  "tl" '(linum-relative-global-mode :which-key "Lines"))



;; https://github.com/Fanael/rainbow-delimiters
;; + Colourize nested parenthesis


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Themes

;; #+ATTR_ORG: :width 420px
;; #+ATTR_HTML: :width 420px
;; #+ATTR_LATEX: :width 420px
;; [[./docs/images/what-is-emacs-customizable.gif]]

;; Cherry pick a few modules from =doom-emacs=. High quality and modern colour themes are provided in the [[https://github.com/hlissner/emacs-doom-themes][doom-themes]] package.


(use-package doom-themes
  :init (load-theme 'doom-moonlight t))



;; [[https://github.com/seagle0128/doom-modeline][doom-modeline]] provides an elegant status bar / modeline.


(use-package doom-modeline
  :custom (doom-modeline-height 16)
  :config (doom-modeline-mode 1))



;; Load a theme with =SPC t t=.


(dotfiles/leader
  "tt" '(counsel-load-theme t t :which-key "Theme"))

;; Pretty

;; Make programming buffers prettier with [[https://github.com/pretty-mode/pretty-mode][pretty-mode]], complimentary to the built in ~prettify-symbols-mode~. 


(use-package pretty-mode
  :hook (python-mode . turn-on-pretty-mode))

;; Ligatures

;; Enable font ligatures via [[https://github.com/jming422/fira-code-mode][fira-code-mode]], perform this action *only* when ~Fira Code~ is the current font.


(when (display-graphic-p)
  (use-package fira-code-mode
    :hook (prog-mode org-mode)))



;; Toggle global ligature mode with =SPC t g=.


(dotfiles/leader
  "tg" '(global-fira-code-mode :which-key "Ligatures"))

;; Dashboard

;; #+ATTR_ORG: :width 420px
;; #+ATTR_HTML: :width 420px
;; #+ATTR_LATEX: :width 420px
;; [[./docs/images/desktop.png]]

;; Present a dashboard when first launching Emacs. Customize the buttons of the navigator:

;; + Brain @ http://localhost:8080
;; + Homepage @ https://chrishayward.xyz
;; + Athabasca @ https://login.athabascau.ca/cas/login
;; + Bookshelf @ https://online.vitalsource.com


(use-package dashboard
  :custom (dashboard-center-content t)
          (dashboard-set-init-info t)
          (dashboard-set-file-icons t)
          (dashboard-set-heading-icons t)
          (dashboard-set-navigator t)
          (dashboard-startup-banner 'logo)
          (dashboard-projects-backend 'projectile)
          (dashboard-items '((projects . 5) (recents  . 5) (agenda . 10)))
          (dashboard-navigator-buttons `(((,(all-the-icons-fileicon "brain" :height 1.1 :v-adjust 0.0)
                                          "Brain" "Knowledge base"
                                          (lambda (&rest _) (browse-url "http://localhost:8080"))))
                                         ((,(all-the-icons-material "public" :height 1.1 :v-adjust 0.0)
                                          "Homepage" "Personal website"
                                          (lambda (&rest _) (browse-url "https://chrishayward.xyz"))))
                                         ((,(all-the-icons-faicon "university" :height 1.1 :v-adjust 0.0)
                                          "Athabasca" "Univeristy login"
                                          (lambda (&rest _) (browse-url "https://login.athabascau.ca/cas/login"))))
                                         ((,(all-the-icons-faicon "book" :height 1.1 :v-adjust 0.0)
                                          "Bookshelf" "Vitalsource bookshelf"
                                          (lambda (&rest _) (browse-url "https://online.vitalsource.com"))))))
  :config (dashboard-setup-startup-hook))



;; When running in *daemon* mode, ensure that the dashboard is the initial buffer.


(setq initial-buffer-choice
      (lambda ()
        (get-buffer "*dashboard*")))
