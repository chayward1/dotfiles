(let ((host-file (concat "~/.local/source/dotfiles/hosts/" system-name ".el")))
  (when (file-exists-p host-file)
    (load-file host-file)))

(dolist (m dotfiles/modules)
  (let ((mod-file (concat "~/.local/source/dotfiles/modules/" (symbol-name m) ".el")))
    (when (file-exists-p mod-file)
      (load-file mod-file))))
