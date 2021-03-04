

;; Once the mail's synchronized, and has indexed with =mu=, it's time to install the required packages for Emacs.

;; + Update every 5 minutes
;; + Scale text for all devices
;; + Sign outbound mail with GPG key
;; + Configure mail account(s)


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



;; Use [[https://github.com/iqbalansari/mu4e-alert][mu4e-alert]] to give us desktop notifications about incoming mail.


(use-package mu4e-alert
  :after mu4e
  :custom (mu4e-alert-set-default-style 'libnotify)
  :config (mu4e-alert-enable-notifications)
          (mu4e-alert-enable-mode-line-display))



;; Create a keybinding to open the mail dashboard with =SPC m=.


(dotfiles/leader
  "m" '(mu4e :which-key "Mail"))

;; Browser

;; Write out the ~$BROWSER~ environment variable.


(setenv "BROWSER" dotfiles/browser)

;; Methods

;; Define a method to run an external process, allowing us to launch any application on a new process without interferring with Emacs.


(defun dotfiles/run (command)
  "Run an external process."
  (interactive (list (read-shell-command "λ ")))
  (start-process-shell-command command nil command))



;; Apply methods to the current call process to avoid issues with hooks.


(defun dotfiles/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))



;; Place keybindings for executing shell commands behind =SPC x=.

;; + Run shell commands with =x=
;; + Run async shell commands with =z=


(dotfiles/leader
  "x" '(:ignore t :which-key "Run")
  "xx" '(dotfiles/run :which-key "Run")
  "xz" '(async-shell-command :which-key "Async"))

;; Displays

;; When the window manager first launches the ~init-hook~ executes, allowing us to define some custom logic.

;; + Display time and date
;; + Display battery info (if available)

;; In my personal configuration, I do not want the battery or time displayed within Emacs when it's not running as desktop environment because that information is typically already available.


(defun dotfiles/init-hook ()
  (exwm-workspace-switch-create 1)
  (setq display-time-and-date t)
  (display-battery-mode 1)
  (display-time-mode 1))



;; Using =autorandr= with pre configured profiles, switching screens (AKA hot plugging) is also handled through a hook.


(defun dotfiles/update-display ()
  "Update the displays by forcing a change through autorandr."
  (dotfiles/run-in-background "autorandr --change --force"))

;; Configuration

;;  Connect our custom hooks and configure the input keys, a custom layer for key capture layers.

;; + Enable =randr= support
;; + Pass through to Emacs
;;   + =M-x= to Emacs
;;   + =C-g= to Emacs
;;   + =C-SPC= to Emacs
;; + Bindings with =S= (Super / Win)
;;   + Reset =S-r=
;;   + Launch =S-&=
;;   + Workspace =S-[1..9]=
    

(use-package exwm
  :custom (exwm-workspace-show-all-buffers t)
          (exwm-input-prefix-keys
            '(?\M-x
              ?\C-c
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
