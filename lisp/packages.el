;;; packages.el --- package configuragion            -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: calendar, convenience, extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
(setq use-package-always-ensure t)
(require 'use-package)

(use-package ace-jump-mode)

(use-package all-the-icons)

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

(use-package aggressive-indent
  :config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'css-mode-hook #'aggressive-indent-mode))

(use-package dired+
  :init
  (setq diredp-hide-details-initially-flag t)
  (setq dired-omit-mode t))

(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t)
  :config
  (load-theme 'doom-molokai t))

(use-package eldoc
  :diminish eldoc-mode
  :commands turn-on-eldoc-mode
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode))

(use-package elpy
  :commands elpy-enable
  :ensure py-autopep8
  :config
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
  (elpy-enable))

(use-package evil
  :ensure projectile
  :ensure evil-surround
  :ensure evil-ediff
  :ensure evil-vimish-fold
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (global-evil-surround-mode 1)
  (evil-vimish-fold-mode 1)
  (define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
  (define-key evil-normal-state-map (kbd "M-f") 'ace-jump-char-mode)
  (define-key evil-visual-state-map (kbd "M-f") 'ace-jump-char-mode)
  (define-key evil-insert-state-map (kbd "C-c C-f") 'ace-jump-char-mode)
  (define-key evil-emacs-state-map (kbd "C-c C-f") 'ace-jump-char-mode)
  (define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)
  (define-key evil-normal-state-map (kbd "U") 'undo-tree-visualize)
  (define-key evil-emacs-state-map (kbd "C-/") 'undo-tree-visualize))

(use-package expand-region
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(use-package flycheck
  :commands flycheck-add-next-checker
  :init
  (setq flycheck-check-syntax-automatically '(save new-line))
  :config
  (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))

(use-package gnus
  :init
  (setq gnus-select-method '(nntp "gmane" (nntp-address "news.gmane.org"))))

(use-package haskell-mode
  :ensure intero
  :ensure flycheck
  :config
  (add-hook 'haskell-mode-hook 'intero-mode))

(use-package magit
  :init
  (setq magit-push-always-verify nil)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  :bind
  ("C-x g" . magit-status)
  ("C-x C-g" . magit-status)
  :config
  (progn
    (with-eval-after-load 'info
      (info-initialize)
      (add-to-list 'Info-directory-list
                   "~/.emacs.d/packages/magit/Documentation/"))))

(use-package ido
  :ensure ido-vertical-mode
  :ensure ido-ubiquitous
  :init
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  :config
  (ido-ubiquitous-mode 1)
  (ido-vertical-mode 1)
  (ido-everywhere 1)
  (ido-mode t))

(use-package linum-relative
  :diminish linum-relative-mode
  :init
  (setq linum-relative-format "%3s "
        linum-relative-current-symbol "")
  :config
  (linum-relative-mode)
  (add-hook 'prog-mode-hook #'(lambda () (linum-mode 1))))

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
         ("C-c M-x" . smex-major-mode-commands))
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
        erc-kill-queries-on-quit t))

(use-package nxml-mode
  :init
  (setq nxml-child-indent 4
        nxml-attribute-indent 4))

(use-package fix-word
  :bind (("M-u" . fix-word-upcase)
         ("M-l" . fix-word-downcase)
         ("M-c" . fix-word-capitalize)))

(use-package org
  :ensure org-bullets
  :ensure org-beautify-theme
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c c" . org-capture))
  :init
  (setq org-startup-indented t)
  :config
  (load-theme 'org-beautify t)
  (add-hook 'org-mode-hook (lambda() (toggle-truncate-lines 0)))
  (add-hook 'org-mode-hook (lambda() (org-bullets-mode 1))))

(use-package projectile
  :commands projectile-find-file
  :config
  (projectile-global-mode))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package restart-emacs
  :bind
  ("C-x C-r" . restart-emacs))

(use-package restclient)

(use-package smartparens
  :config
  (require 'smartparens-config)
  (add-hook 'prog-mode-hook #'smartparens-mode))

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

(use-package switch-window
  :commands switch-window
  :init
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts
        '("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o"))
  :bind
  ("C-x o" . switch-window))

(use-package try)

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
  :init
  (add-to-list 'auto-mode-alist '("\\.x?html\\'" . web-mode))
  (add-to-list 'my/auto-minor-mode-alist '("\\.x?html\\'" . impatient-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode)))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

(provide 'packages)
;;; packages.el ends here
