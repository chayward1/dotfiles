;; Hosts

;; Each host machines configuration loaded immediately after declaring the options, before applying any configuration. This allows system to system control while remaining immutable. Override any of the available options configurations in a host file. Begin the process by loading any host specific option overrides. The host configuration tangles, and loads (if it exist) using the systems name. If a host definition doesn't exist the default values remain. 

(defun dotfiles/load-host (host-name)
  "Load the host configuration file for the system `host-name'."
  (interactive)
  (let ((host-file (concat dotfiles/home "/hosts/" host-name ".org")))
    (when (file-exists-p host-file)
      (org-babel-load-file host-file))))
