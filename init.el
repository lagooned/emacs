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

;; add load path
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; load custom settings
(load "custom")

;; setup environment 
(load "global")
(load "config")

;; motd
(message "#################")
(message "# CONFIG LOADED #")
(message "#################")

;; customs
(custom-set-variables)
(custom-set-faces)
