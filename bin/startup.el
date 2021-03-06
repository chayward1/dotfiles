;; Emacs creates a lot of files relative to `user-emacs-directory'.
;; These files are not part of this immutable configuration and do not belong in the emacs directory.

;; How can we solve this issue?

;; Shortly after initialization, before most packages load, we change the value to `dotfiles/cache'.
;; I elaborate more on the technique in my post https://chrishayward.xyz/immutable-emacs/.

(setq user-emacs-directory dotfiles/cache)

;; Because this project uses version-control, we can disable more unwanted features:

;; + Lock files
;; + Backup files

(setq create-lockfiles nil
      make-backup-files nil)
