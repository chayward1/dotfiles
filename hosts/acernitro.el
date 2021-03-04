;; Acernitro
;; :PROPERTIES:
;; :header-args: :tangle hosts/acernitro.el
;; :END:

;; The first machine with real hardware to deploy this configuration to. It's an Acer Nitro AN-515 with the NVIDIA / Intel hybrid graphics card. Due to the issues I encountered with this hardware setup, I again opted to install Ubuntu 20.04, and stripped away the components I don't need.

;; + Set the browser manually
;; + Set the language to Canadian english
;; + Increase font size for high DPI screen

;; Configure the browser.


(setq dotfiles/browser "firefox"
      dotfiles/language "en_CA"
      dotfiles/font-size 132)
