(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom (lsp-idle-delay (* 5 dotfiles/idle)))

(use-package lsp-ui
  :after lsp
  :custom (lsp-ui-doc-position 'at-point)
          (lsp-ui-doc-delay 0.500))

(use-package dap-mode
  :commands (dap-debug))

(use-package docker
  :commands (docker))

(dotfiles/leader
  "k" '(docker :which-key "Docker"))

(use-package projectile
  :custom (projectile-project-search-path '("~/.local/source"))
  :config (projectile-mode))

(use-package company
  :after lsp)

(use-package company-lsp
  :after (lsp company)
  :custom (company-backend 'company-lsp))

(use-package password-store
  :custom (password-store-dir dotfiles/passwords))

(dotfiles/leader
  "p" '(:ignore t :which-key "Passwords")
  "pp" '(password-store-copy :which-key "Copy")
  "pr" '(password-store-rename :which-key "Rename")
  "pg" '(password-store-generate :which-key "Generate"))

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

(use-package ob-http
  :after org
  :config (org-babel-do-load-languages
            'org-babel-load-languages
            '((http . t))))

(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda ()
           (require 'ccls)
           (lsp-deferred)))
  :config (add-to-list 'org-structure-template-alist '("cc" . "src cc")))

(use-package python-mode
  :hook (python-mode . lsp-deferred)
  :config (require 'dap-python)
          (add-to-list 'org-src-lang-modes '("python" . python))
          (add-to-list 'org-structure-template-alist '("py" . "src python"))
          (org-babel-do-load-languages 'org-babel-load-languages '((python . t)))
  :custom (python-shell-interpreter "python3") ;; Required if "python" is not python 3.
          (dap-python-executable "python3")    ;; Same as above.
          (dap-python-debugger 'debugpy))

(add-hook 'python-mode-hook
          (lambda ()
            (mapc (lambda (pair) (push pair prettify-symbols-alist))
                  '(;; Syntax
                    ("def" .      #x2131)
                    ("not" .      #x2757)
                    ("in" .       #x2208)
                    ("not in" .   #x2209)
                    ("return" .   #x27fc)
                    ("yield" .    #x27fb)
                    ("for" .      #x2200)
                    ;; Base Types
                    ("int" .      #x2124)
                    ("float" .    #x211d)
                    ("str" .      #x1d54a)
                    ("True" .     #x1d54b)
                    ("False" .    #x1d53d)
                    ;; Mypy
                    ("Dict" .     #x1d507)
                    ("List" .     #x2112)
                    ("Tuple" .    #x2a02)
                    ("Set" .      #x2126)
                    ("Iterable" . #x1d50a)
                    ("Any" .      #x2754)
                    ("Union" .    #x22c3)))))

(use-package plantuml-mode
  :after org
  :custom (plantuml-default-exec-mode 'jar)
          (plantuml-jar-path "~/.local/bin/plantuml.jar")
          (org-plantuml-jar-path (expand-file-name "~/.local/bin/plantuml.jar"))
          (org-startup-with-inline-images t)
  :config (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
          (add-to-list 'org-structure-template-alist '("pl" . "src plantuml"))
          (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))

(dotfiles/leader
  "ti" '(org-display-inline-images :which-key "Images"))
