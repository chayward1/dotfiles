;; Load the host configuration.
(let ((host-file (concat user-emacs-directory "/hosts/" system-name ".el")))
  (when (file-exists-p host-file)
    (load-file host-file)))

;; Load the enabled modules.
(dolist (m dotfiles/modules)
  (let ((mod-file (concat dotfiles/home "/modules/" (symbol-name m) ".el")))
    (when (file-exists-p mod-file)
      (load-file mod-file))))
