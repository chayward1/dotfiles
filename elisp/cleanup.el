;; Cleanup

;; Emacs creates a lot of files relative to `user-emacs-directory'.
;; These files are not part of this immutable configuration and do not belong in the emacs directory.

(defconst dotfiles/home
  (or (getenv "DOTFILES_HOME")
      (expand-file-name user-emacs-directory)))

(defconst dotfiles/cache
  (or (getenv "DOTFILES_CACHE")
      (expand-file-name "~/.cache/emacs")))

;; How can we solve this issue?

(unless (file-exists-p dotfiles/cache)
  (make-directory dotfiles/cache t))

;; Shortly after initialization, before most packages load, we change the value to `dotfiles/cache'.
;; I elaborate more on the technique in my post https://chrishayward.xyz/immutable-emacs/.

(setq user-emacs-directory dotfiles/cache)

;; Disable error messages for packages that do not yet support native compilation.

(setq comp-async-report-warnings-errors nil)

;; Because this project uses version-control, we can disable more unwanted features:

;; + Lock files
;; + Backup files

(setq make-backup-files nil
      create-lockfiles nil)
