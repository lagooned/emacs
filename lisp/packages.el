;;;;;;;;;;;;;;;;;;;;
;; PACKAGE CONFIG ;;
;;;;;;;;;;;;;;;;;;;;

;; INSTALL USE-PACKAGE IF NOT INSTALLED
(if (not (package-installed-p 'use-package))
	(progn
	  (package-refresh-contents)
	  (package-install 'use-package)))
(setq use-package-always-ensure t)
(require 'use-package)

;; USE PACKAGE CONFIGS
(use-package adaptive-wrap
  :config
  (progn
	(setq-default adaptive-wrap-extra-indent 2)
	(add-hook 'visual-line-mode-hook
			  (lambda ()
				(adaptive-wrap-prefix-mode +1)
				(diminish 'visual-line-mode)))
	(global-visual-line-mode +1)))

(use-package try)

(use-package company
  :diminish company-mode
  :init
  (setq company-show-numbers t)
  :config
  (add-hook 'after-init-hook 'global-company-mode)) 

(use-package dired+
  :init
  (setq dired-omit-mode t)) 

(use-package eldoc
  :diminish eldoc-mode
  :commands turn-on-eldoc-mode
  :defer t
  :init
  (progn
	(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
	(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
	(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)))

(use-package evil
  :ensure evil-surround
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (progn
	(evil-mode 1)
	(global-evil-surround-mode 1)))

(use-package ido
  :ensure ido-vertical-mode 
  :ensure ido-ubiquitous
  :init
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  :config
  (progn
	(ido-mode t)
	(ido-everywhere 1)
	(ido-vertical-mode 1)
	(ido-ubiquitous-mode 1)))

(use-package linum-relative
  :diminish linum-relative-mode
  :init
  (progn
	(setq linum-relative-format "%3s ")
	(setq linum-relative-current-symbol ""))
  :config
  (progn
	(linum-relative-mode)
	(global-linum-mode)))

(use-package lorem-ipsum
  :config
  (lorem-ipsum-use-default-bindings))

(use-package smex
  :commands smex
  :bind (("M-x" . smex)
		 ("M-X" . smex-major-mode-commands))
  :config (smex-initialize))

(use-package projectile
  :commands projectile-find-file
  :bind (:map evil-normal-state-map
			  ("C-p" . projectile-find-file))
  :config (projectile-global-mode))

(use-package emmet-mode
  :init
  (progn
	(add-hook 'sgml-mode-hook 'emmet-mode) 
	(add-hook 'web-mode-hook 'emmet-mode)))

(use-package erc
  :config
  (setq erc-hide-list '("PART" "QUIT" "JOIN"))
  (setq erc-server "irc.freenode.net"
		erc-nick "lagooned"
		erc-autojoin-channels-alist '(("freenode.net"
									   "#bitswebteam"
									   "#gamestoptrades"
									   "#emacs-beginners"))))

(use-package magit
  :init (setq magit-push-always-verify nil)
  :bind ("C-x g" . magit-status)
  :config
  (with-eval-after-load 'info
	(info-initialize)
	(add-to-list 'Info-directory-list
				 "~/.emacs.d/packages/magit/Documentation/")))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

(use-package nxml-mode
  :init
  (setq nxml-child-indent 4
		nxml-attribute-indent 4))

(use-package fix-word
  :bind (("M-u" . fix-word-upcase)
		 ("M-l" . fix-word-downcase)
		 ("M-c" . fix-word-capitalize)))

(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package remember-theme
  :init
  (setq remember-theme-file "~/.emacs.d/.last-theme")
  (add-hook 'kill-emacs-hook 'remember-theme-save))

(use-package web-mode
  :ensure web-mode
  :ensure skewer-mode
  :ensure impatient-mode
  :init
  (progn 
	(add-to-list 'auto-mode-alist '("\\.x?html\\'" . web-mode))
	(add-to-list 'auto-minor-mode-alist '("\\.x?html\\'" . skewer-html-mode))
	(add-to-list 'auto-minor-mode-alist '("\\.x?html\\'" . impatient-mode))
	(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
	(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
	(add-to-list 'auto-minor-mode-alist '("\\.css\\'" . skewer-css-mode))
	(add-to-list 'auto-mode-alist '("\\.less\\'" . web-mode))
	(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
	(add-to-list 'auto-mode-alist '("\\.js\\'" . skewer-mode))))


(use-package restclient)

(use-package restart-emacs)

(use-package haskell-mode)

(use-package skewer-mode
  :init
  (setq httpd-root "~/")
  :config
  (skewer-setup))

(use-package solarized-theme
  :init
  (setq solarized-use-variable-pitch nil
		solarized-high-contrast-mode-line t
		solarized-use-less-bold t
		solarized-emphasize-indicators nil
		solarized-scale-org-headlines nil
		solarized-height-minus-1 1
		solarized-height-plus-1 1
		solarized-height-plus-2 1
		solarized-height-plus-3 1
		solarized-height-plus-4 1))
