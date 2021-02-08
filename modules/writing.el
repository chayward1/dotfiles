(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode))

(use-package ox-hugo 
  :after ox)

(use-package ox-reveal
  :after ox
  :custom (org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"))

(use-package org-roam
  :hook (after-init . org-roam-mode)
  :custom (org-roam-directory org-directory)
          (org-roam-encrypt-files t)
          (org-roam-capture-templates
          '(("p" "Post" plain (function org-roam-capture--get-point)
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

#+HUGO_BASE_DIR: ../../website/
#+HUGO_AUTO_SET_LASTMOD: t
#+HUGO_SECTION: posts
#+HUGO_DRAFT: true
")
            ("n" "Notes" plain (function org-roam-capture--get-point)
             "%?"
             :file-name "docs/notes/${slug}"
             :unnarrowed t
             :head
"
#+TITLE: ${title}
#+AUTHOR: Christopher James Hayward

#+EXPORT_FILE_NAME: ${slug}
#+ROAM_KEY: https://chrishayward.xyz/notes/${slug}/

#+HUGO_BASE_DIR: ../../website
#+HUGO_AUTO_SET_LASTMOD: t
#+HUGO_SECTION: notes
#+HUGO_DRAFT: true
")

            ("s" "Slides" plain (function org-roam-capture--get-point)
             "%?"
             :file-name "docs/slides/${slug}"
             :unnarrowed t
             :head
"
#+TITLE: ${title}
#+AUTHOR: Christopher James Hayward

#+REVEAL_ROOT: https://cdn.jsdelivr.net/npm/reveal.js
")))
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

(unless (string-match-p "\\.gpg" org-agenda-file-regexp)
  (setq org-agenda-file-regexp
        (replace-regexp-in-string "\\\\\\.org" "\\\\.org\\\\(\\\\.gpg\\\\)?"
                                  org-agenda-file-regexp)))

(setq org-agenda-files '("~/.emacs.d/docs/"
                         "~/.emacs.d/docs/daily/"))

(dotfiles/leader
  "a" '(org-agenda :which-key "Agenda"))

(use-package gif-screencast
  :commands (gif-screencast-start-or-stop gif-screencast-toggle-pause)
  :custom (gif-screencast-output-directory (concat dotfiles/home "docs/images/")))

(dotfiles/leader
  "s" '(:ignore t :which-key "Screencast")
  "ss" '(gif-screencast-start-or-stop :which-key "Start / Stop")
  "sp" '(gif-screencast-toggle-pause :which-key "Pause"))
