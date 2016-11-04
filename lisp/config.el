;;;;;;;;;;;;;;;;;;;;
;; PACKAGE CONFIG ;;
;;;;;;;;;;;;;;;;;;;;

;; install use-package
(if (not (package-installed-p 'use-package))
	(progn
	  (package-refresh-contents)
	  (package-install 'use-package)))
(setq use-package-always-ensure t)
(require 'use-package)

;; try
(use-package try)

;; auto-complete
(use-package auto-complete
  :config
  (ac-config-default)
  (global-auto-complete-mode t))

;; dired+ 
(use-package dired+
  :init
  (setq dired-omit-mode t)) 

;; evil
(use-package evil
  :ensure evil-surround
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (global-evil-surround-mode 1))

;; ido
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

;; linum-relative
(use-package linum-relative
  :init
  (setq linum-relative-format "%3s ")
  (setq linum-relative-current-symbol "")
  :config
  (linum-relative-mode)
  (global-linum-mode))

;; lorem-ipsum
(use-package lorem-ipsum
  :config
  (lorem-ipsum-use-default-bindings))

;; smex
(use-package smex
  :commands smex
  :bind (("M-x" . smex)
		 ("M-X" . smex-major-mode-commands))
  :config (smex-initialize))

;; projectile
(use-package projectile
  :commands projectile-find-file
  :bind (:map evil-normal-state-map
			  ("C-p" . projectile-find-file))
  :config (projectile-global-mode))

(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.x?html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.less\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode)))

;; emmet-mode
(use-package emmet-mode
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) 
  (add-hook 'web-mode-hook 'emmet-mode))

;; magit
(use-package magit
  :init (setq magit-push-always-verify nil)
  :bind ("C-x g" . magit-status)
  :config
  (with-eval-after-load 'info
	(info-initialize)
	(add-to-list 'Info-directory-list
				 "~/.emacs.d/packages/magit/Documentation/")))

;; adaptive wrap
(use-package adaptive-wrap
  :config
  (setq-default adaptive-wrap-extra-indent 2)
  (add-hook 'visual-line-mode-hook
			(lambda ()
			  (adaptive-wrap-prefix-mode +1)
			  (diminish 'visual-line-mode)))
  (global-visual-line-mode +1))

;; yasnippet
(use-package yasnippet
  :config
  (yas-global-mode 1))

;; undo-tree
(use-package undo-tree
  :config
  (global-undo-tree-mode))

(use-package python-mode
  :config
  (defun electric-indent-ignore-python (char)
	"Ignore electric indentation for python-mode"
	(if (equal major-mode 'python-mode)
		'no-indent
	  nil))
  (add-hook 'electric-indent-functions 'electric-indent-ignore-python)
  (defun set-newline-and-indent ()
	"Map the return key with `newline-and-indent'"
	(local-set-key (kbd "RET") 'newline-and-indent))
  (add-hook 'python-mode-hook 'set-newline-and-indent))

;; nxml-mode
(use-package nxml-mode
  :config
  (setq nxml-child-indent 4
		nxml-attribute-indent 4))

;; remember theme
(use-package remember-theme
  :commands remember-theme-load
  :config
  (remember-theme-load)
  (add-hook 'kill-emacs-hook 'remember-theme-save))

;; solarized
(use-package solarized-theme
  :init
  (setq solarized-use-variable-pitch nil)
  (setq solarized-high-contrast-mode-line t)
  (setq solarized-use-less-bold t)
  (setq solarized-emphasize-indicators nil)
  (setq solarized-scale-org-headlines nil)
  (setq solarized-height-minus-1 1)
  (setq solarized-height-plus-1 1)
  (setq solarized-height-plus-2 1)
  (setq solarized-height-plus-3 1)
  (setq solarized-height-plus-4 1))

