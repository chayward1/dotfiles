;; Projects
;; :PROPERTIES:
;; :header-args: :tangle modules/projects.el
;; :END:

;; An IDE like experience (or better) can be achieved in Emacs using two *Microsoft* open source initiatives.

;; + [[https://microsoft.github.io/language-server-protocol/][Language Server Protocol]]
;; + [[https://microsoft.github.io/debug-adapter-protocol/][Debug Adapter Protocol]]

;; Add support for language servers with [[https://emacs-lsp.github.io/lsp-mode/][lsp-mode]].
  

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom (lsp-idle-delay (* 5 dotfiles/idle)))



;; [[https://emacs-lsp.github.io/lsp-ui/][lsp-ui]] provides UI improvements for =lsp-mode=.


(use-package lsp-ui
  :after lsp
  :custom (lsp-ui-doc-position 'at-point)
          (lsp-ui-doc-delay 0.500))



;; [[https://emacs-lsp.github.io/dap-mode/][Dap-mode]] adds support for the debug adapter protocol to Emacs.


(use-package dap-mode
  :commands (dap-debug))

;; Containers

;; Use ~docker~ for running containers. Download and install https://github.com/Silex/docker.el, allowing us to manage containers within Emacs.


(use-package docker
  :commands (docker))



;; Open the management screen with =SPC k=.


(dotfiles/leader
  "k" '(docker :which-key "Docker"))

;; Management

;; Configure [[https://projectile.mx][projectile]], a project interaction library for Emacs. It provides a nice set of features for operating on a project level without introducing external dependencies.


(use-package projectile
  :custom (projectile-project-search-path '("~/.local/source"))
  :config (projectile-mode))

;; Completion

;; Text completion framework via =company= aka *Complete Anything*.

;; http://company-mode.github.io/
;; + Integrate with =lsp-mode=
  

(use-package company
  :after lsp)

(use-package company-lsp
  :after (lsp company)
  :custom (company-backend 'company-lsp))

;; Passwords

;; Pass makes managing passwords extremely easy, encrypring them in a file structure and providing easy commands for generating, modify, and copying passwords. =password-store.el= provides a wrapper for the functionality within Emacs.


(use-package password-store
  :custom (password-store-dir dotfiles/passwords))



;; Configure keybindings behind =SPC p=.
;; + Copy with =p=
;; + Rename with =r=
;; + Generate with =g=


(dotfiles/leader
  "p" '(:ignore t :which-key "Passwords")
  "pp" '(password-store-copy :which-key "Copy")
  "pr" '(password-store-rename :which-key "Rename")
  "pg" '(password-store-generate :which-key "Generate"))



;; Set the ~GOPATH~ environment variable prior to loading, this allows us to change the default value of ~$HOME/go~ to ~$HOME/.go~.


(setenv "GOPATH" (concat (getenv "HOME") "/.go/"))



;; Additionally, include the =bin= subdirectory of the ~$GOPATH~ in the ~$PATH~ variable, adding compiled golang programs.


(setenv "PATH" (concat (getenv "GOPATH") "bin:" (getenv "PATH")))



;; Finally we can include the =go-mode= package, integrating it with =lsp=.


(use-package go-mode
  :hook (go-mode . lsp)
  :custom (lsp-go-gopls-server-path "~/.go/bin/gopls"))



;; Apply some custom behaviour before saving:

;; + Format buffer
;; + Organize imports


(defun dotfiles/go-hook ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'go-mode-hook #'dotfiles/go-hook)



;; Add a golang source code block structure template with ~<go~:


(add-to-list 'org-structure-template-alist '("go" . "src go"))

;; HTTP

;; Instead of the popular =restclient= package, I use [[https://github.com/zweifisch/ob-http][ob-http]] as a lightweight alternative.


(use-package ob-http
  :after org
  :config (org-babel-do-load-languages
            'org-babel-load-languages
            '((http . t))))

;; C/C++

;; #+ATTR_ORG: :width 420px
;; #+ATTR_HTML: :width 420px
;; #+ATTR_LATEX: :width 420px
;; [[./docs/images/2021-02-13-example-ccls.gif]]

;; Add support for C/C++ languages.

;; + Configure the [[https://github.com/MaskRay/ccls][ccls]] language server
;; + Load babel language modules for C/C++
;; + Create a new structure templates for C/C++
;;   - ~<cc~ for C
;;   - ~<cpp~ for C++
    

(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda ()
           (require 'ccls)
           (lsp-deferred)))
  :config (add-to-list 'org-structure-template-alist '("cc" . "src C"))
          (add-to-list 'org-structure-template-alist '("cpp" . "src C++"))
          (org-babel-do-load-languages 'org-babel-load-languages '((C . t))))



;; [[https://www.emacswiki.org/emacs/PythonProgrammingInEmacs][Python-mode]] is an Emacs built in mode.

;; + Load the babel language module for Python
;; + Add a python source code block structure template with ~<py~
  

(use-package python-mode
  :hook (python-mode . lsp-deferred)
  :config (require 'dap-python)
          (add-to-list 'org-src-lang-modes '("python" . python))
          (add-to-list 'org-structure-template-alist '("py" . "src python"))
          (org-babel-do-load-languages 'org-babel-load-languages '((python . t)))
  :custom (python-shell-interpreter "python3") ;; Required if "python" is not python 3.
          (dap-python-executable "python3")    ;; Same as above.
          (dap-python-debugger 'debugpy))

;; PlantUML

;; Download and install [[https://plantuml.com][PlantUML]], a text-based markup language for creating UML diagrams.

;; + Load the babel language module for PlantUML
;; + Create a structure template with ~<pl~


(use-package plantuml-mode
  :after org
  :custom (plantuml-default-exec-mode 'jar)
          (plantuml-jar-path "~/.local/bin/plantuml.jar")
          (org-plantuml-jar-path (expand-file-name "~/.local/bin/plantuml.jar"))
          (org-startup-with-inline-images t)
  :config (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
          (add-to-list 'org-structure-template-alist '("pl" . "src plantuml"))
          (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))



;; Toggle inline images with =SPC t i=.


(dotfiles/leader
  "ti" '(org-toggle-inline-images :which-key "Images"))
