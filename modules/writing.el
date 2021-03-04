;; Writing
;; :PROPERTIES:
;; :header-args: :tangle modules/writing.el
;; :END:

;; I am using [[https://orgmode.org][org-mode]] extensively for writing projects for different purposes. Most of the improvements are done in the *Core* module for the Literate programming configuration. Encrypt files using symmetric key encryption via PGP. This enables my workflow of storing my personal notes anywhere. Emacs can cache the gpg password if you trust your session.


(setq epa-file-select-keys 2
      epa-file-cache-passphrase-for-symmetric-encryption t
      epa-file-encrypt-to dotfiles/public-key)



;; Download and install [[https://github.com/integral-dw/org-superstar-mode][org-superstar-mode]] for making headline stars more *super*.


(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode))

;; Roam

;; #+ATTR_ORG: :width 420px
;; #+ATTR_HTML: :width 420px
;; #+ATTR_LATEX: :width 420px
;; [[./docs/images/2021-02-13-example-roam.png]]

;; Download and install [[https://orgroam.com][org-roam]], a plain text knowledge management system for Emacs.


(use-package org-roam
  :hook (after-init . org-roam-mode)
  :custom (org-roam-directory org-directory)
          (org-roam-encrypt-files t)
          (org-roam-capture-templates '())
          (org-roam-dailies-capture-templates
          '(("d" "Default" entry (function org-roam-capture--get-point)
             "* %?"
             :file-name "docs/daily/%<%Y-%m-%d>"
             :head
"
#+TITLE: %<%Y-%m-%d>
#+AUTHOR: Christopher James Hayward
"))))



;; Place keybindings behind =SPC r=.

;; + Find with =f=
;; + Toggle buffer with =b=
;; + Dailies with =d=
;;   + Arbitrary date with =d=
;;   + Today with =t=
;;   + Tomorrow with =m=
;;   + Yesterday with =y=


(dotfiles/leader
  "r" '(:ignore t :which-key "Roam")
  "rf" '(org-roam-find-file :which-key "Find")
  "rb" '(org-roam-buffer-toggle-display :which-key "Buffer")
  "rd" '(:ignore t :which-key "Dailies")
  "rdd" '(org-roam-dailies-find-date :which-key "Date")
  "rdt" '(org-roam-dailies-find-today :which-key "Today")
  "rdm" '(org-roam-dailies-find-tomorrow :which-key "Tomorrow")
  "rdy" '(org-roam-dailies-find-yesterday :which-key "Yesterday"))



;; Visualize the org-roam database with the server, available when the editor is running at http://localhost:8080


(use-package org-roam-server
  :hook (org-roam-mode . org-roam-server-mode))

;; Hugo

;; I use [[https://gohugo.io][Hugo]] for my personal [[https://chrishayward.xyz][website]], which I write in =org-mode= before compiling to =hugo-markdown=. [[https://github.com/kaushalmodi/ox-hugo][ox-hugo]], configured for =one-post-per-file= is my technique for managing my content. 


(use-package ox-hugo 
  :after ox)

;; Posts

;; Add a capture template for creating new blog posts.


(with-eval-after-load 'org-roam
  (add-to-list 'org-roam-capture-templates
               '("p" "Post" plain (function org-roam-capture--get-point)
                 "%?"
                 :file-name "docs/posts/${slug}"
                 :unnarrowed t
                 :head
"
#+TITLE: ${title}
#+AUTHOR: Christopher James Hayward
#+DATE: %<%Y-%m-%d>

#+EXPORT_FILE_NAME: ${slug}
#+ROAM_KEY: https://chrishayward.xyz/posts/${slug}/

#+HUGO_BASE_DIR: ../
#+HUGO_AUTO_SET_LASTMOD: t
#+HUGO_SECTION: posts
#+HUGO_DRAFT: true
")))

;; Notes

;; Add a capture template for creating blog posts and notes on other peoples content / published works.


(with-eval-after-load 'org-roam
  (add-to-list 'org-roam-capture-templates
               '("n" "Notes" plain (function org-roam-capture--get-point)
                  "%?"
                  :file-name "docs/notes/${slug}"
                  :unnarrowed t
                  :head
"
#+TITLE: ${title}
#+AUTHOR: Christopher James Hayward

#+EXPORT_FILE_NAME: ${slug}
#+ROAM_KEY: https://chrishayward.xyz/notes/${slug}/

#+HUGO_BASE_DIR: ../
#+HUGO_AUTO_SET_LASTMOD: t
#+HUGO_SECTION: notes
#+HUGO_DRAFT: true
")))

;; Slides

;; Produce high quality presentations that work anywhere with =HTML/JS= and the [[https://revealjs.com][reveal.js]] package. [[https://github.com/hexmode/ox-reveal][ox-reveal]], configured to use a =cdn= allows us to produce ones that are not dependent on a local version of =reveal.js=.


(use-package ox-reveal
  :after ox
  :custom (org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"))



;; Create a capture template for creating slides quickly, with our desired configuration.


(with-eval-after-load 'org-roam
  (add-to-list 'org-roam-capture-templates
               '("s" "Slides" plain (function org-roam-capture--get-point)
                 "%?"
                 :file-name "docs/slides/${slug}"
                 :unnarrowed t
                 :head
"
#+TITLE: ${title}
#+AUTHOR: Christopher James Hayward

#+EXPORT_FILE_NAME: ${slug}
#+OPTIONS: toc:nil num:nil reveal_title_slide:nil

#+REVEAL_ROOT: https://cdn.jsdelivr.net/npm/reveal.js
#+REVEAL_THEME: serif
")))

;; Agenda

;; #+ATTR_ORG: :width 420px
;; #+ATTR_HTML: :width 420px
;; #+ATTR_LATEX: :width 420px
;; [[./docs/images/2021-02-13-example-agenda.gif]]

;; Override ~org-agenda-file-regexp~ to include =.org.gpg= files.


(unless (string-match-p "\\.gpg" org-agenda-file-regexp)
  (setq org-agenda-file-regexp
        (replace-regexp-in-string "\\\\\\.org" "\\\\.org\\\\(\\\\.gpg\\\\)?"
                                  org-agenda-file-regexp)))



;; Create a capture template for courses.


(with-eval-after-load 'org-roam
  (add-to-list 'org-roam-capture-templates
               '("c" "Course" plain (function org-roam-capture--get-point)
               "%?"
               :file-name "docs/courses/${slug}"
               :unnarrowed t
               :head
"
#+TITLE: ${title}
#+SUBTITLE:
#+AUTHOR: Christopher James Hayward
")))



;; Configure agenda sources.
  

(setq org-agenda-files '("~/.emacs.d/docs/"
                         "~/.emacs.d/docs/courses/"
                         "~/.emacs.d/docs/daily/"))



;; Open an agenda buffer with =SPC a=.


(dotfiles/leader
  "a" '(org-agenda :which-key "Agenda"))

;; Images

;; Capture screenshots with [[https://github.com/tecosaur/screenshot][screenshot.el]].


(use-package screenshot
  :commands (screenshot))



;; Create screencasts with =one-frame-per-action= GIF recording via [[https://github.com/takaxp/emacs-gif-screencast][emacs-gif-screencast]].

;; + Pause / Resume
;; + High Quality
;; + Optimized

;; It requires the installation of ~scrot~, ~gifsicle~, and ~convert~ from the =ImageMagick= library.
  

(use-package gif-screencast
  :commands (gif-screencast-start-or-stop gif-screencast-toggle-pause)
  :custom (gif-screencast-output-directory (concat dotfiles/home "docs/images/")))



;; Place keybindings behind =SPC s=.
;; + Screenshot with =s=
;; + Screencast with =c=


(dotfiles/leader
  "s" '(:ignore t :which-key "Screen")
  "ss" '(screenshot :which-key "Screenshot")
  "sc" '(gif-screencast-start-or-stop :which-key "Screencast"))

;; Grammar

;; I use [[https://github.com/bnbeckwith/writegood-mode][writegood-mode]] to find common writing problems such as cliches and poor wording. Grammarly for the peons!


(use-package writegood-mode
  :after org
  :config (writegood-mode))



;; Toggle ~writegood~ mode with =SPC t w=.


(dotfiles/leader
  "tw" '(writegood-mode :which-key "Grammar"))

;; Spelling

;; Use the built in =ispell= package to add spell checking features to buffers.


(use-package ispell
  :after org
  :custom (ispell-dictionary dotfiles/lang))



;; Toggle highlighting within buffers with =SPC t s=.


(dotfiles/leader
  "ts" '(flyspell-buffer :which-key "Spelling"))
