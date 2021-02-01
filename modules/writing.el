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

(dotfiles/leader
  "m" '(mu4e :which-key "Mail"))

(use-package org-roam
  :hook (after-init . org-roam-mode)
  :custom (org-roam-directory dotfiles/brain))

(use-package org-roam-server
  :hook (org-roam-mode . org-roam-server-mode))

(dotfiles/leader
  "r" '(:ignore t :which-key "Roam")
  "rf" '(org-roam-find-file :which-key "Find")
  "rb" '(org-roam-buffer-toggle-display :which-key "Buffer")
  "rc" '(org-roam-capture :which-key "Capture")
  "rd" '(:ignore t :which-key "Dailies")
  "rdd" '(org-roam-dailies-find-date :which-key "Date")
  "rdt" '(org-roam-dailies-find-today :which-key "Today")
  "rdm" '(org-roam-dailies-find-tomorrow :which-key "Tomorrow")
  "rdy" '(org-roam-dailies-find-yesterday :which-key "Yesterday"))

(setq org-roam-capture-templates
      '(("d" "Default" plain (function org-roam-capture--get-point)
         "%?"
         :file-name "${slug}"
         :head "#+TITLE: ${title}\n"
         :unnarrowed t)))

(setq org-roam-dailies-capture-templates
      '(("d" "Default" entry (function org-roam-capture--get-point)
         "* %?"
         :file-name "daily/%<%Y-%m-%d>"
         :head "#+TITLE: %<%Y-%m-%d>\n")))

(use-package org-noter
  :after org
  :config
  (setq org-noter-always-create-frame nil
        org-noter-notes-search-path dotfiles/notes))

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter-pdftools
  :after org-noter
  :config
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-active-handler-functions #'org-noter-pdftools-jump-to-note)))

(setq bibtex-completion-notes-path dotfiles/notes
      bibtex-completion-bibliography dotfiles/bib
      bibtex-completion-pdf-field "file"
      bibtex-completion-notes-template-multiple-files
      (concat
        "#+TITLE: ${title}\n"
        "#+ROAM_KEY: cite:${=key=}\n"
        "#* TODO Notes\n"
        ":PROPERTIES:\n"
        ":CUSTOM_ID: ${=key}\n"
        ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
        ":AUTHOR: ${author-abbrev}\n"
        ":JOURNAL: ${journaltitle}\n"
        ":DATE: ${date}\n"
        ":YEAR: ${year}\n"
        ":DOI: ${doi}\n"
        ":URL: ${url}\n"
        ":END:\n\n"))

(use-package org-ref
  :config
  (setq org-ref-completion-library 'org-ref-helm-cite
        org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
        org-ref-default-bibliography dotfiles/bib
        org-ref-bibliography-notes dotfiles/notes
        org-ref-notes-directory dotfiles/notes
        org-ref-notes-function 'orb-edit-notes
        org-ref-note-title-format "* TODO %y - %t\n
:PROPERTIES:\n
:CUSTOM_ID: %k\n
:NOTER_DOCUMENT: %F\n
:ROAM_KEY: cite:%k\n
:AUTHOR: %9a\n
:JOURNAL: %j\n
:YEAR: %y\n
:VOLUME: %v\n
:PAGES: %p\n
:DOI: %D\n
:URL: %U\n
:END:\n\n"))

(use-package org-roam-bibtex
  :after (org-roam)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq orb-preformat-keywords
        '("=key=" "title" "url" "file" "author-or-editor" "keywords")))

(add-to-list 'org-roam-capture-templates
             '("n" "Notes" plain (function org-roam-capture--get-point)
               ""
               :file-name "notes/${slug}"
               :head "#+TITLE: ${=key=}: ${title}\n\n
#+ROAM_KEY:${ref}\n\n* ${title}\n
:PROPERTIES:\n
:CUSTOM_ID: ${=key=}\n
:URL: ${url}\n
:AUTHOR: ${author-or-editor}\n
:NOTER_DOCUMENT:%(orb-process-file-field \"${=key=}\")\n
:NOTER_PAGE:\n
:END:\n\n"))

(setq org-agenda-files '("~/.local/source/brain/daily/"
                         "~/.local/source/secrets/org/"))

(dotfiles/leader
  "a" '(org-agenda :which-key "Agenda"))

(use-package ox-hugo 
  :after ox)

(add-to-list 'org-roam-capture-templates
             '("b" "Blogging" plain (function org-roam-capture--get-point)
               "%?"
               :file-name "posts/${slug}"
               :head "#+TITLE: ${title}\n
#+HUGO_BASE_DIR: ~/.local/source/website\n
#+HUGO_SECTION: posts\n"))

(use-package gif-screencast
  :custom
  (gif-screencast-output-directory "~/.local/source/brain/screen/"))

(dotfiles/leader
  "s" '(:ignore t :which-key "Screencast")
  "ss" '(gif-screencast-start-or-stop :which-key "Start / Stop")
  "sp" '(gif-screencast-toggle-pause :which-key "Pause"))

(use-package ox-reveal
  :after ox
  :custom (org-reveal-root "https://cdn.jsdelivr.net/reveal.js/3.9.2/"))

(add-to-list 'org-roam-capture-templates
             '("p" "Presentation" plain (function org-roam-capture--get-point)
               "%?"
               :file-name "slides/${slug}"
               :head "#+TITLE: ${title}\n"))
