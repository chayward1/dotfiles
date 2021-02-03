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
  :config
  (require 'exwm-randr)
  (exwm-randr-enable)
  (add-hook 'exwm-init-hook #'dotfiles/init-hook)
  (add-hook 'exwm-randr-screen-change-hook #'dotfiles/update-display)
  (dotfiles/update-display)
  (setq exwm-input-prefix-keys
        '(?\M-x
          ?\C-g
          ?\C-\ )
        exwm-input-global-keys
        `(([?\s-r] . exwm-reset)
          ([?\s-&] . dotfiles/run)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 1 9))))
  (exwm-enable))
