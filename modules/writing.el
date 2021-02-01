(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

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

(setq org-agenda-files '("~/.local/source/secrets/org/"))

(dotfiles/leader
  "a" '(org-agenda :which-key "Agenda"))

(use-package org-journal
  :config
  (setq org-journal-dir (concat dotfiles/home "/daily/")
        org-journal-file-type 'daily
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-enable-agenda-integration t
        org-icalendar-store-UID t
        org-icalendar-include-todo "all"
        org-icalendar-combined-agenda-file (concat dotfiles/home "calender.ics")))

(dotfiles/leader
  "f" '(:ignore t :which-key "Journal")
  "ff" '(org-journal-new-entry :which-key "New")
  "fs" '(org-journal-search :which-key "Search")
  "fl" '(org-journal-open-next-entry :which-key "Next")
  "fh" '(org-journal-open-previous-entry :which-key "Previous"))

(use-package ox-hugo 
  :after ox)

(use-package gif-screencast
  :custom
  (gif-screencast-output-directory (concat dotfiles/home "images/")))

(dotfiles/leader
  "s" '(:ignore t :which-key "Screencast")
  "ss" '(gif-screencast-start-or-stop :which-key "Start / Stop")
  "sp" '(gif-screencast-toggle-pause :which-key "Pause"))

(use-package ox-reveal
  :after ox
  :custom (org-reveal-root "https://cdn.jsdelivr.net/reveal.js/3.9.2/"))
