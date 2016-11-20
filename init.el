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

;;;;;;;;;;;;;;;
;; SETUP ENV ;;
;;;;;;;;;;;;;;;

;; load custom functions
(load "functions")

;; load global config
(load "global")

;; load package config
(load "config")

;; motd
(message "#################")
(message "# CONFIG LOADED #")
(message "#################")

;; customs
(setq custom-file "~/.emacs.d/.custom.el")
(load custom-file 'noerror)

;; load themes
(load "themes")
