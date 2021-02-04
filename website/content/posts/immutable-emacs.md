+++
title = "Immutable Emacs"
author = ["Christopher James Hayward"]
date = 2021-01-19
lastmod = 2021-02-01T16:10:13-05:00
draft = false
+++

You can easily create an **Immutable** and **100% Reproducible** custom Emacs configuration with very little effort. My inspiration for this came from the `Emacs From Scratch` series by [System Crafters](https://youtube.com/c/SystemCrafters).


## Getting started {#getting-started}

Emacs created **lots** of files relative to `user-emacs-directory`, which are **not** part of this configuration and do not belong in the same directory.

-   Disable lockfiles
-   Disable backup files

To acheive this functionality, before anything else happens we change this directory to `~/.cache/emacs`, while keeping the old value of `user-emacs-directory` in our own variable.

```emacs-lisp
(defvar dotfiles/home user-emacs-directory)
(defvar dotfiles/cache "~/.cache/emacs")

(setq create-lockfiles nil
      make-backup-files nil
      user-emacs-directory dotfiles/cache)
```


## Package management {#package-management}

[Straight](https://github.com/raxod502/straight.el) is a 100% functional package manager for Emacs, with integration for `use-package`, a common way to download / configure / install Emacs packages.

-   Use the development branch
-   Integrate with `use-package`

Apply the configurations prior to bootstrapping the package manager, by setting (writing) to the variables that `straight` will ultimately read from.

```emacs-lisp
(setq straight-repository-branch "develop"
      straight-use-package-by-default t)
```

Bootstrap the package manager, downloading, installing, or configuring depending on the state of the configuration. All packages are downloaded and built from source, and can be pinned to specific git commit hashes.

```emacs-lisp
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
```

Complete the integration with `use-package` by installing it with `straight-use-package`:

```emacs-lisp
(straight-use-package 'use-package)
```


## A E S T H E T I C S {#a-e-s-t-h-e-t-i-c-s}

If you've ever looked at default Emacs, you know that it's one of the ugliest GUI applications out of the box. Including a few packages, cherry picked from `Doom` can bring Emacs out of the eightes!

```emacs-lisp
(use-package all-the-icons)
(use-package doom-modeline :init (doom-modeline-mode 1))
(use-package doom-themes :init (load-theme 'doom-one t))
```


## Conclusion {#conclusion}

Now that the **stateful** and **immutable** files are seperated, and the default look has been improved **slightly** you're left with a clean, immutable, and reproducible Emacs configuration to continue hacking on.

Enjoy it!
