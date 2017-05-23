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
  :commands global-visual-line-mode
  :init
  (setq-default adaptive-wrap-extra-indent 2)
  :config
  (add-hook 'visual-line-mode-hook
			(lambda ()
			  (adaptive-wrap-prefix-mode +1)
			  (diminish 'visual-line-mode)))
  (global-visual-line-mode +1))

(use-package try)

(use-package elpy
  :commands elpy-enable
  :ensure py-autopep8
  :config
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
  (elpy-enable))

(use-package ace-jump-mode)

(use-package haskell-mode
  :ensure intero
  :ensure flycheck
  :config
  (add-hook 'haskell-mode-hook 'intero-mode))

(use-package flycheck
  :commands flycheck-add-next-checker
  :init
  (setq flycheck-check-syntax-automatically '(save new-line))
  :config
  (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))

(use-package magit
  :init
  (setq magit-push-always-verify nil)
  :bind ("C-x g" . magit-status)
  :config
  (progn
	(with-eval-after-load 'info
	  (info-initialize)
	  (add-to-list 'Info-directory-list
				   "~/.emacs.d/packages/magit/Documentation/"))))

(use-package projectile
  :commands projectile-find-file
  :config (projectile-global-mode))

(use-package company
  :diminish company-mode
  :init
  (setq company-show-numbers t)
  :config
  (add-hook 'after-init-hook 'global-company-mode)) 

(use-package dired+
  :init
  (setq diredp-hide-details-initially-flag t)
  (setq dired-omit-mode t))

(use-package eldoc
  :diminish eldoc-mode
  :commands turn-on-eldoc-mode
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode))

(use-package evil
  :ensure projectile
  :ensure evil-surround
  :init
  (progn
	(setq evil-want-C-u-scroll t)
	(global-evil-surround-mode 1)
	(evil-mode 1))
  :config
  (progn 
	;; (define-key evil-normal-state-map (kbd "M-d") 'evil-scroll-up)
	;; (define-key evil-visual-state-map (kbd "M-d") 'evil-scroll-up)
	;; (define-key evil-normal-state-map (kbd "C-M-d") 'evil-scroll-up)
	;; (define-key evil-visual-state-map (kbd "C-M-d") 'evil-scroll-up)
	;; (define-key evil-normal-state-map (kbd "C-S-d") 'evil-scroll-up)
	;; (define-key evil-visual-state-map (kbd "C-S-d") 'evil-scroll-up)
	(define-key evil-normal-state-map (kbd "f") 'ace-jump-mode)
	(define-key evil-normal-state-map (kbd "S-f") 'ace-jump-char-mode)
	(define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)
	(define-key evil-insert-state-map (kbd "C-c f") 'ace-jump-char-mode)
	(define-key evil-emacs-state-map (kbd "M-p") 'projectile-find-file)
	(define-key evil-emacs-state-map (kbd "C-c C-f") 'ace-jump-char-mode)
	(define-key evil-emacs-state-map (kbd "C-/") 'undo-tree-visualize)
	(loop for (mode . state) in '(()) do (evil-set-initial-state mode state))))

(use-package ido
  :ensure ido-vertical-mode 
  :ensure ido-ubiquitous
  :init
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  :config
  (ido-mode t)
  (ido-everywhere 1)
  (ido-vertical-mode 1)
  (ido-ubiquitous-mode 1))

(use-package linum-relative
  :diminish linum-relative-mode
  :init
  (setq linum-relative-format "%3s "
		linum-relative-current-symbol "")
  :config
  (linum-relative-mode)
  (global-linum-mode))

(use-package lorem-ipsum
  :config
  (lorem-ipsum-use-default-bindings))

(use-package emmet-mode
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) 
  (add-hook 'web-mode-hook 'emmet-mode))

(use-package smex
  :commands smex
  :bind (("M-x" . smex)
		 ("M-X" . smex-major-mode-commands))
  :config
  (smex-initialize))

(use-package erc
  :config
  (require 'erc-services nil t)
  (erc-services-mode 1)
  (setq erc-prompt-for-nickserv-password nil)
  (setq erc-hide-list '("PART" "QUIT" "JOIN"))
  (setq erc-default-coding-system '(utf-8 . utf-8)
		erc-server-coding-system '(utf-8 . utf-8)
		erc-server "irc.freenode.net"
		erc-nick "lagooned"
		erc-hide-list '("JOIN" "PART" "QUIT" "NICK" "MODE")
		erc-prompt-for-password nil
		erc-prompt (lambda () (concat (buffer-name) ">"))
		erc-server-send-ping-interval 10
		erc-server-send-ping-timeout 180
		erc-server-reconnect-timeout 60
		erc-prompt-for-nickserv-password nil
		;; erc-kill-buffer-on-part t
		;; erc-server-auto-reconnect t
		;; erc-kill-server-buffer-on-quit t
		erc-kill-queries-on-quit t
		erc-autojoin-channels-alist '((".*freenode.net" "#bitswebteam"))))

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

(use-package nxml-mode
  :init
  (setq nxml-child-indent 4
		nxml-attribute-indent 4))

(use-package fix-word
  :bind (("M-u" . fix-word-upcase)
		 ("M-l" . fix-word-downcase)
		 ("M-c" . fix-word-capitalize)))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package restclient
  :ensure company-restclient)

(use-package remember-theme
  :init
  (setq remember-theme-file "~/.emacs.d/.last-theme")
  :config
  (add-hook 'kill-emacs-hook 'remember-theme-save))

(use-package org
  :ensure org-bullets
  :bind (("C-c l" . org-store-link)
		 ("C-c a" . org-agenda))
  :config
  (add-hook 'org-mode-hook (lambda() (org-bullets-mode 1))))

(use-package org-bullets)

(use-package restart-emacs)

(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  :config
  (global-undo-tree-mode))

(use-package web-mode
  :ensure web-mode
  :ensure impatient-mode
  ;; setup mode
  ;; :mode 
  :init
  (progn 
	(add-to-list 'auto-mode-alist '("\\.x?html\\'" . web-mode))
	(add-to-list 'auto-minor-mode-alist '("\\.x?html\\'" . impatient-mode))
	(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
	(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
	(add-to-list 'auto-mode-alist '("\\.less\\'" . web-mode))
	(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))))

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
