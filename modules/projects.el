(use-package lsp-mode
  :custom (gc-cons-threshold 1000000000)
          (lsp-idle-delay 0.500))

(use-package lsp-ui
  :custom (lsp-ui-doc-position 'at-point)
          (lsp-ui-doc-delay 0.500))

(use-package projectile
  :config
  (setq projectile-project-search-path '("~/.local/source"))
  (projectile-mode))

(use-package password-store
  :custom (password-store-dir dotfiles/passwords))

(dotfiles/leader
  "p" '(:ignore t :which-key "Passwords")
  "pp" '(password-store-copy :which-key "Copy")
  "pr" '(password-store-rename :which-key "Rename")
  "pg" '(password-store-generate :which-key "Generate"))

(use-package dap-mode)

(use-package company)
(use-package company-lsp)

(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))

(use-package python-mode
  :hook (python-mode . lsp)
  :config (require 'dap-python)
  :custom (python-shell-interpreter "python3") ;; Required if "python" is not python 3.
          (dap-python-executable "python3")    ;; Same as above.
          (dap-python-debugger 'debugpy))

(setenv "GOPATH" (concat (getenv "HOME") "/.go/"))

(use-package go-mode
  :hook (go-mode . lsp))

(defun dotfiles/go-hook ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'go-mode-hook #'dotfiles/go-hook)
