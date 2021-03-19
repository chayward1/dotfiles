;; Init

;; This project makes heavy use of modern features and libraries. Since [[https://orgmode.org/worg/org-contrib/babel/intro.html][Org-babel]][fn:2]'s used during the initialization, [[https://orgmode.org][Org-mode]][fn:3] must load prior to importing any custom modules. My solution includes the introduction of some early intitialization code written in [[https://gnu.org/software/emacs/manual/html_node/elisp/index.html][Emacs Lisp]][fn:4].


(load-file "~/.emacs.d/bin/options.el")
(load-file "~/.emacs.d/bin/cleanup.el")
(load-file "~/.emacs.d/bin/packages.el")

;; Load host definition

;; Begin the process by loading any host specific option overrides. The host configuration tangles, and loads (if it exist) using the systems name. If a host definition doesn't exist the default values remain. 


(let ((host-file (concat dotfiles/home "/hosts/" system-name ".org")))
  (when (file-exists-p host-file)
    (org-babel-load-file host-file)))

;; Load enabled modules

;; All of the modules in ~dotfiles/modules~ load after the host overrides. By default, all of the packages defined in ~dotfiles/modules-p~ load. Override this behaviour in a host configuration file.


(dolist (m dotfiles/modules)
  (let ((mod-file (concat dotfiles/home "/modules/" (symbol-name m) ".org")))
    (when (file-exists-p mod-file)
      (org-babel-load-file mod-file))))
