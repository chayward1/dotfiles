(use-package mu4e
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :custom (mu4e-maildir "~/.cache/mail")
          (mu4e-update-interval (* 5 60))
          (mu4e-get-mail-command "mbsync -a")
          (mu4e-compose-format-flowed t)
          (mu4e-change-filenames-when-moving t)
          (message-send-mail-function 'smtpmail-send-it)  
          (mml-secure-openpgp-signers '("37AB1CB72B741E478CA026D43025DCBD46F81C0F"))
          (mu4e-compose-signature (concat "Chris Hayward\n"
                                          "https://chrishayward.xyz\n"))
  :config
  (add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime)
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
  :custom (mu4e-alert-set-default-style 'libnotify)
  :config (mu4e-alert-enable-notifications)
          (mu4e-alert-enable-mode-line-display))

(dotfiles/leader
  "m" '(mu4e :which-key "Mail"))

;; (dotfiles/leader
;;   "b" '(dotfiles/run-in-background dotfiles/browser :which-key "Browser"))

(defun dotfiles/run (command)
  "Run an external process."
  (interactive (list (read-shell-command "λ ")))
  (start-process-shell-command command nil command))

(defun dotfiles/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(dotfiles/leader
  "x" '(dotfiles/run :which-key "Run")
  "z" '(async-shell-command :which-key "Async"))

(defun dotfiles/init-hook ()
  (exwm-workspace-switch-create 1)
  (setq display-time-and-date t)
  (display-battery-mode 1)
  (display-time-mode 1))

(defun dotfiles/update-display ()
  (dotfiles/run-in-background "autorandr --change --force"))

(use-package exwm
  :custom (exwm-input-prefix-keys
            '(?\M-x
              ?\C-g
              ?\C-\ ))
          (exwm-input-global-keys
            `(([?\s-r] . exwm-reset)
              ,@(mapcar (lambda (i)
                          `(,(kbd (format "s-%d" i)) .
                          (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                          (number-sequence 1 9))))
  :config (require 'exwm-randr)
          (exwm-randr-enable)
          (add-hook 'exwm-init-hook #'dotfiles/init-hook)
          (add-hook 'exwm-randr-screen-change-hook #'dotfiles/update-display)
          (dotfiles/update-display)
          (exwm-enable))
