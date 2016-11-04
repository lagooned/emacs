(require 'package)
(setq package-archives
	  '(("ELPA" . "http://tromey.com/elpa/")
		("gnu" . "http://elpa.gnu.org/packages/")
		("melpa" . "http://melpa.milkbox.net/packages/")
		("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

;; add load path
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; setup environment 
(load "global")
(load "config")

;; load custom settings
(load "custom")

;; motd
(message "############################")
(message "# lagooned's config loaded #")
(message "############################")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
	("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(package-selected-packages
   (quote
	(emmet-mode auto-complete remember-theme nxml-mode python-mode yasnippet adaptive-wrap magit web-mode projectile smex lorem-ipsum linum-relative ido-ubiquitous ido-vertical-mode evil-surround dired+ use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
