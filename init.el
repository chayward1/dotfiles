;; NOTE: This file is tangled from README.org
(defconst IS-LINUX (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin ms-dos windows-nt)))

(load-file "~/.local/source/dotfiles/modules/core.el")
(load-file "~/.local/source/dotfiles/modules/desktop.el")
(load-file "~/.local/source/dotfiles/modules/writing.el")
(load-file "~/.local/source/dotfiles/modules/projects.el")
(load-file "~/.local/source/dotfiles/modules/interface.el")
