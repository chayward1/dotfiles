(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode))

(setq epa-file-select-keys 2
      epa-file-cache-passphrase-for-symmetric-encryption t
      epa-file-encrypt-to "37AB1CB72B741E478CA026D43025DCBD46F81C0F")

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

(dotfiles/leader
  "r" '(:ignore t :which-key "Roam")
  "rf" '(org-roam-find-file :which-key "Find")
  "rb" '(org-roam-buffer-toggle-display :which-key "Buffer")
  "rd" '(:ignore t :which-key "Dailies")
  "rdd" '(org-roam-dailies-find-date :which-key "Date")
  "rdt" '(org-roam-dailies-find-today :which-key "Today")
  "rdm" '(org-roam-dailies-find-tomorrow :which-key "Tomorrow")
  "rdy" '(org-roam-dailies-find-yesterday :which-key "Yesterday"))

(use-package org-roam-server
  :hook (org-roam-mode . org-roam-server-mode))

(use-package ox-hugo 
  :after ox)

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

(use-package ox-reveal
  :after ox
  :custom (org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"))

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

(unless (string-match-p "\\.gpg" org-agenda-file-regexp)
  (setq org-agenda-file-regexp
        (replace-regexp-in-string "\\\\\\.org" "\\\\.org\\\\(\\\\.gpg\\\\)?"
                                  org-agenda-file-regexp)))

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

(setq org-agenda-files '("~/.emacs.d/docs/"
                         "~/.emacs.d/docs/courses/"
                         "~/.emacs.d/docs/daily/"))

(dotfiles/leader
  "a" '(org-agenda :which-key "Agenda"))

(use-package screenshot
  :commands (screenshot))

(use-package gif-screencast
  :commands (gif-screencast-start-or-stop gif-screencast-toggle-pause)
  :custom (gif-screencast-output-directory (concat dotfiles/home "docs/images/")))

(dotfiles/leader
  "s" '(:ignore t :which-key "Screen")
  "ss" '(screenshot :which-key "Screenshot")
  "sc" '(gif-screencast-start-or-stop :which-key "Screencast"))

(use-package writegood-mode
  :after org
  :config (writegood-mode))

(dotfiles/leader
  "tw" '(writegood-mode :which-key "Grammar"))

(use-package ispell
  :after org
  :custom (ispell-dictionary dotfiles/lang))
