;; Termux
;; :PROPERTIES:
;; :header-args: :tangle hosts/localhost.el
;; :END:

;; Android devices do not use the Linux host names, so setting a custom hostname is pointless as it just changes the value of ~kernel.hostname~ and have no impact on the running system. It's recommended to leave the hostname as =localhost= in Termux. While this raises an issue with my host definition system, it's no more of an issue than the =raspberry= namespace.

;; + Increase font size for small screen


(setq dotfiles/font-size 132)
