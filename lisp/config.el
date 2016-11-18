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
  (setq-default adaptive-wrap-extra-indent 2)
  (add-hook 'visual-line-mode-hook
			(lambda ()
			  (adaptive-wrap-prefix-mode +1)
			  (diminish 'visual-line-mode)))
  (global-visual-line-mode +1))

(use-package try)

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)) 

(use-package dired+
  :init
  (setq dired-omit-mode t)) 

(use-package evil
  :ensure evil-surround
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (global-evil-surround-mode 1))

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
  :init
  (setq linum-relative-format "%3s ")
  (setq linum-relative-current-symbol "")
  :config
  (linum-relative-mode)
  (global-linum-mode))

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
  (add-hook 'sgml-mode-hook 'emmet-mode) 
  (add-hook 'web-mode-hook 'emmet-mode))

(use-package magit
  :init (setq magit-push-always-verify nil)
  :bind ("C-x g" . magit-status)
  :config
  (with-eval-after-load 'info
	(info-initialize)
	(add-to-list 'Info-directory-list
				 "~/.emacs.d/packages/magit/Documentation/")))

(use-package yasnippet
  :config
  (yas-global-mode 1))

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

(use-package web-mode
  :ensure skewer-mode
  :ensure impatient-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.x?html\\'" . web-mode))
  (add-to-list 'auto-minor-mode-alist '("\\.x?html\\'" . skewer-html-mode))
  (add-to-list 'auto-minor-mode-alist '("\\.x?html\\'" . impatient-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (add-to-list 'auto-minor-mode-alist '("\\.css\\'" . skewer-css-mode))
  (add-to-list 'auto-mode-alist '("\\.less\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . skewer-mode))
  :config
  (setq httpd-root "~/web"))
