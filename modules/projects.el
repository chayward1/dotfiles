(use-package lsp-mode
  :custom (gc-cons-threshold 1000000000)
          (lsp-idle-delay 0.500))

(use-package lsp-ui
  :custom (lsp-ui-doc-position 'at-point)
          (lsp-ui-doc-delay 0.500))

(use-package docker)

(dotfiles/leader
  "k" '(docker :which-key "Docker"))

(use-package projectile
  :custom (projectile-project-search-path '("~/.local/source"))
  :config (projectile-mode))

(use-package password-store
  :custom (password-store-dir dotfiles/passwords))

(dotfiles/leader
  "p" '(:ignore t :which-key "Passwords")
  "pp" '(password-store-copy :which-key "Copy")
  "pr" '(password-store-rename :which-key "Rename")
  "pg" '(password-store-generate :which-key "Generate"))

(use-package dap-mode)

(use-package company)
(use-package company-lsp
  :custom (company-backend 'company-lsp))

(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda ()
           (require 'ccls)
           (lsp)))
  :config (add-to-list 'org-structure-template-alist '("cc" . "src cc")))

(use-package python-mode
  :hook (python-mode . lsp)
  :config (require 'dap-python)
          (add-to-list 'org-src-lang-modes '("python" . python))
          (add-to-list 'org-structure-template-alist '("py" . "src python"))
          (org-babel-do-load-languages 'org-babel-load-languages '((python . t)))
  :custom (python-shell-interpreter "python3") ;; Required if "python" is not python 3.
          (dap-python-executable "python3")    ;; Same as above.
          (dap-python-debugger 'debugpy))

(use-package plantuml-mode
  :custom (plantuml-default-exec-mode 'jar)
          (plantuml-jar-path "~/.local/bin/plantuml.jar")
          (org-plantuml-jar-path (expand-file-name "~/.local/bin/plantuml.jar"))
          (org-startup-with-inline-images t)
  :config (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
          (add-to-list 'org-structure-template-alist '("pl" . "src plantuml"))
          (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))

(dotfiles/leader
  "ti" '(org-display-inline-images :which-key "Images"))

(setenv "GOPATH" (concat (getenv "HOME") "/.go/"))

(setenv "PATH" (concat (getenv "GOPATH") "bin:" (getenv "PATH")))

(use-package go-mode
  :hook (go-mode . lsp)
  :custom (lsp-go-gopls-server-path "~/.go/bin/gopls"))

(defun dotfiles/go-hook ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'go-mode-hook #'dotfiles/go-hook)

(add-to-list 'org-structure-template-alist '("go" . "src go"))
