;;;;;;;;;;;;;;;;;;;;;;;;;
;; JARED ENGLER CONFIG ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; elpa
(require 'package)
(setq package-archives
	  '(("ELPA" . "http://tromey.com/elpa/")
		("gnu" . "http://elpa.gnu.org/packages/")
		("melpa" . "http://melpa.milkbox.net/packages/")
		("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

;; install use package
(if (not (package-installed-p 'use-package))
	(progn
	  (package-refresh-contents)
	  (package-install 'use-package)))
(setq use-package-always-ensure t)
(require 'use-package)

;; add load path
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; config packages
(load "config")

;; setup environment 
(load "global")

;; load custom settings
(load "custom")

;; custom 
(custom-set-variables)
(custom-set-faces)

;; themes
(load "themes")

;; motd
(message "#################")
(message "# CONFIG LOADED #")
(message "#################")
