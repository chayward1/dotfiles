;; Modules

;; Breaking down the project into logical units or chapters to keep the code more maintainable and organized. This is also a fundamental requirement to achieve the goal of modularity. All of the modules in ~dotfiles/modules~ load after the host overrides. By default, all of the packages defined in ~dotfiles/modules-p~ load. Override this behaviour in a host configuration file.


(defun dotfiles/load-modules (modules)
  "Load the `modules' in sequential order."
  (interactive)
  (dolist (m modules)
    (let ((mod-file (concat dotfiles/home "/modules/" (symbol-name m) ".org")))
      (when (file-exists-p mod-file)
	(org-babel-load-file mod-file)))))
