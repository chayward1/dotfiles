(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

(use-package ox-hugo 
  :after ox)

(use-package ox-reveal
  :after ox
  :custom (org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"))

(use-package org-roam
  :hook (after-init . org-roam-mode)
  :custom
  (org-roam-directory org-directory)
  (org-roam-encrypt-files t))

(use-package org-roam-server
  :hook (org-roam-mode . org-roam-server-mode))

(dotfiles/leader
  "r" '(:ignore t :which-key "Roam")
  "rf" '(org-roam-find-file :which-key "Find")
  "rb" '(org-roam-buffer-toggle-display :which-key "Buffer")
  "rd" '(:ignore t :which-key "Dailies")
  "rdd" '(org-roam-dailies-find-date :which-key "Date")
  "rdt" '(org-roam-dailies-find-today :which-key "Today")
  "rdm" '(org-roam-dailies-find-tomorrow :which-key "Tomorrow")
  "rdy" '(org-roam-dailies-find-yesterday :which-key "Yesterday"))

(setq org-roam-capture-templates
      '(("p" "Posts" plain (function org-roam-capture--get-point)
          "%?"
          :file-name "docs/posts/${slug}"
          :unnarrowed t
          :head
"
#+TITLE: ${title}
#+AUTHOR: Christopher James Hayward
#+DATE: %<%Y-%m-%d>

#+ROAM_KEY: https://chrishayward.xyz/posts/${slug}/

#+HUGO_BASE_DIR: ~/.local/source/website
#+HUGO_AUTO_SET_LASTMOD: t
#+HUGO_SECTION: posts
")
        ("n" "Notes" plain (function org-roam-capture--get-point)
          "%?"
          :file-name "docs/notes/${slug}"
          :unnarrowed t
          :head
"
#+TITLE: ${title}
#+AUTHOR: Christopher James Hayward

#+ROAM_KEY: https://chrishayward.xyz/notes/${slug}/

#+HUGO_BASE_DIR: ~/.local/source/website
#+HUGO_AUTO_SET_LASTMOD: t
#+HUGO_SECTION: notes
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

(setq org-roam-dailies-capture-templates
      '(("d" "Default" entry (function org-roam-capture--get-point)
         "* %?"
         :file-name "docs/daily/%<%Y-%m-%d>"
         :head
"
#+TITLE: %<%Y-%m-%d>
#+AUTHOR: Christopher James Hayward
")))

(use-package mu4e
  :load-path "/usr/share/emacs/site-lisp/mu4e"
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

(use-package mu4e-alert
  :custom
  (mu4e-alert-set-default-style 'libnotify)
  :config
  (mu4e-alert-enable-notifications)
  (mu4e-alert-enable-mode-line-display))

(dotfiles/leader
  "m" '(mu4e :which-key "Mail"))

(unless (string-match-p "\\.gpg" org-agenda-file-regexp)
  (setq org-agenda-file-regexp
        (replace-regexp-in-string "\\\\\\.org" "\\\\.org\\\\(\\\\.gpg\\\\)?"
                                  org-agenda-file-regexp)))

(setq org-agenda-files '("~/.emacs.d/docs/"
                         "~/.emacs.d/docs/daily/"))

(dotfiles/leader
  "a" '(org-agenda :which-key "Agenda"))

(use-package gif-screencast
  :custom
  (gif-screencast-output-directory (concat dotfiles/home "docs/images/")))

(dotfiles/leader
  "s" '(:ignore t :which-key "Screencast")
  "ss" '(gif-screencast-start-or-stop :which-key "Start / Stop")
  "sp" '(gif-screencast-toggle-pause :which-key "Pause"))
