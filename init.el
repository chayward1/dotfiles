(let ((host-file (concat user-emacs-directory "/hosts/" system-name ".el")))
  (when (file-exists-p host-file)
    (load-file host-file)))

(dolist (m dotfiles/modules)
  (let ((mod-file (concat dotfiles/home "/modules/" (symbol-name m) ".el")))
    (when (file-exists-p mod-file)
      (load-file mod-file))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("/home/chris/.local/source/secrets/org/birthdays.org" "/home/chris/.local/source/secrets/org/courses.org" "/home/chris/.local/source/secrets/org/holidays.org" "/home/chris/.local/source/dotfiles/daily/20210201"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
