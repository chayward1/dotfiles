;; Options

;; All of the options available for configuration are defined here. They may be overriden in a host configuration, and are read by the definitions in the modules. All of the variables are prefixed with ~dotfiles/~ to avoid name collision with other functions and packages. All of the available modules are defined in ~dotfiles/modules-p~. The variable is constant, meaning it cannot be modified. Each time a new module is added, it must be included in this list.


(defconst dotfiles/modules-p
  '(org trash
    editor
    shell
    email
    feeds
    media
    terminal
    encryption 
    desktop
    writing 
    presentations
    website 
    capture
    projects
    development 
    interface 
    dashboard) 
  "All of the available modules.")



;; After the host configuration has loaded, the modules defined in ~dotfiles/modules~ are loaded sequentially. By default, the value of ~dotfiles/modules~ is equal to ~dotfiles/modules-p~. This means that all of the modules will load by default. Remove symbols from this list in a host configuration, or override it entirely to modify this behaviour.


(defvar dotfiles/modules dotfiles/modules-p
  "All of the enable modules, default value equal to `dotfiles/modules-p'.")

;; Environment variables

;; Some of the behaviour in Emacs depends on the values of mutable environment variables. To reduce confusion in my own configuration, the values are read when Emacs starts, and then written to once the configuration has loaded. This allows the values to be overriden in a host configuration, without modifying the environment variable prior to starting.


(defvar dotfiles/browser (getenv "BROWSER")
  "Default system web browser.")

(defvar dotfiles/language (getenv "LANG")
  "Default system dictionary language.")

;; Look and feel

;; Define the options for the unified system font. The default is =Fira Code= due to its readability and support for ligatures. All font faces will be set with this value. Any variations to the font sizes are based on the value defined here as well, reducing the number of places to make modifications to when changing fonts.


(defvar dotfiles/font "Fira Code"
  "Unified system font family.")

(defvar dotfiles/font-size 96
  "Unified system font size.")



;; Certain actions like code completions, or binding suggestions, can be configured to wait for a specific delay before offering their respective choices. I prefer to keep this value low, so that suggestions are shown immediately. This can have an affect on the performance of Emacs on older hardware. Adjust accordingly.


(defvar dotfiles/idle 0.0
  "Delay time before offering suggestions and completions.")



;; Prefix all of the custom keybinding actions with =SPC=, a tehcnique that comes from Doom / Spacemacs. In some situations, namely when using the [[file:modules/desktop.org][Desktop]] module, the leader key may not always be available. The global prefix should be used in these circumstances.


(defvar dotfiles/leader-key "SPC"
  "The all-powerful leader key.")

(defvar dotfiles/leader-key-global
  (concat "C-" dotfiles/leader-key)
  "Global prefix for the all-powerful leader key.")

;; Productivity

;; The location of source code projects for indexing in the [[file:modules/projects.org][Projects]] module are defined here. These projects will integrate their TODOs with the local Agenda. Override this setting in a host configuration, with the =DOTFILES_PROJECTS= environment variable, or use the default value of =~/.local/source/= in compliance with the XDG Base Directory specification.


(defvar dotfiles/projects
  (or (getenv "DOTFILES_PROJECTS")
      (expand-file-name "~/.local/source"))
  "Location of source code projects.")

;; Security

;; The local password store should be cloned prior to initialization. Override this setting in a host configuration, with the =DOTFILES_PASSWORDS= environment variable, or use the default value of =~/.password-store=, which is what GNU pass will assume.


(defvar dotfiles/passwords
  (or (getenv "DOTFILES_PASSWORDS")
      (expand-file-name "~/.password-store"))
  "Location of the local password store.")



;; Since I keep all of my writing in this repository, I encrypt *most* of my Org files with GPG. Define the key to encrypt them for / to. Override this in a host configuration file.


(defvar dotfiles/public-key "37AB1CB72B741E478CA026D43025DCBD46F81C0F"
  "GPG kley to encrpy org files for/to.")

;; Hosts

;; Each host machines configuration loaded immediately after declaring the options, before applying any configuration. This allows system to system control while remaining immutable. Override any of the available options configurations in a host file. Begin the process by loading any host specific option overrides. The host configuration tangles, and loads (if it exist) using the systems name. If a host definition doesn't exist the default values remain. 


(let ((host-file (concat dotfiles/home "/hosts/" system-name ".org")))
  (when (file-exists-p host-file)
    (org-babel-load-file host-file)))

;; Modules

;; Breaking down the project into logical units or chapters to keep the code more maintainable and organized. This is also a fundamental requirement to achieve the goal of modularity. All of the modules in ~dotfiles/modules~ load after the host overrides. By default, all of the packages defined in ~dotfiles/modules-p~ load. Override this behaviour in a host configuration file.


(dolist (m dotfiles/modules)
  (let ((mod-file (concat dotfiles/home "/modules/" (symbol-name m) ".org")))
    (when (file-exists-p mod-file)
      (org-babel-load-file mod-file))))
