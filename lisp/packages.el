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

(use-package adaptive-wrap
  :commands global-visual-line-mode
  :init
  (setq-default adaptive-wrap-extra-indent 2)
  :config
  (add-hook
   'visual-line-mode-hook
   (lambda ()
     (adaptive-wrap-prefix-mode +1)
     (diminish 'visual-line-mode)))
  (global-visual-line-mode +1))

(use-package aggressive-indent
  :commands aggressive-indent-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'css-mode-hook #'aggressive-indent-mode))

(use-package counsel
  :diminish counsel-mode
  :config
  (counsel-mode 1))

(use-package dired+
  :diminish dired-omit-mode
  :init
  (setq diredp-hide-details-initially-flag t)
  (setq dired-omit-mode t)
  (setq dired-dwim-target t))

(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t)
  :config
  ;; (load-theme 'doom-molokai)
  ;; (load-theme 'doom-nova t)
  ;; (load-theme 'doom-one t)
  ;; (load-theme 'doom-tomorrow-night t)
  (load-theme 'doom-vibrant t)
  )

(use-package eldoc
  :diminish eldoc-mode
  :commands turn-on-eldoc-mode
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode))

(use-package evil
  :ensure ace-jump-mode
  :ensure counsel
  :ensure evil-matchit
  :ensure evil-ediff
  :ensure evil-surround
  :ensure evil-vimish-fold
  :ensure undo-tree
  :diminish evil-vimish-fold-mode
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (global-evil-matchit-mode 1)
  (global-evil-surround-mode 1)
  (evil-vimish-fold-mode 1)

  ;; evil binds
  (define-key evil-normal-state-map (kbd "M-f")         'ace-jump-char-mode)
  (define-key evil-visual-state-map (kbd "M-f")         'ace-jump-char-mode)
  (define-key evil-insert-state-map (kbd "C-c j")       'ace-jump-char-mode)
  (define-key evil-emacs-state-map  (kbd "C-c j")       'ace-jump-char-mode)
  (define-key evil-normal-state-map (kbd "C-c g")       'my/counsel-rg-region)
  (define-key evil-insert-state-map (kbd "C-c g")       'my/counsel-rg-region)
  (define-key evil-emacs-state-map  (kbd "C-c g")       'my/counsel-rg-region)
  (define-key evil-normal-state-map (kbd "C-x C-M-f")   'counsel-locate)
  (define-key evil-insert-state-map (kbd "C-x C-M-f")   'counsel-locate)
  (define-key evil-emacs-state-map  (kbd "C-x C-M-f")   'counsel-locate)
  (define-key evil-normal-state-map (kbd "C-x M-f")     'counsel-recentf)
  (define-key evil-insert-state-map (kbd "C-x M-f")     'counsel-recentf)
  (define-key evil-emacs-state-map  (kbd "C-x M-f")     'counsel-recentf)
  (define-key evil-normal-state-map (kbd "C-p")         'counsel-git)
  (define-key evil-normal-state-map (kbd "M-y")         'counsel-yank-pop)
  (define-key evil-insert-state-map (kbd "M-y")         'counsel-yank-pop)
  (define-key evil-emacs-state-map  (kbd "M-y")         'counsel-yank-pop)
  (define-key evil-insert-state-map (kbd "M-\\")        'evil-execute-in-emacs-state)
  (define-key evil-insert-state-map (kbd "C-s")         'swiper)
  (define-key evil-normal-state-map (kbd "C-s")         'swiper)
  (define-key evil-visual-state-map (kbd "C-s")         'swiper)
  (define-key evil-insert-state-map (kbd "TAB")         'tab-to-tab-stop)
  (define-key evil-normal-state-map (kbd "U")           'undo-tree-visualize)
  (define-key evil-normal-state-map (kbd "M-/")         'yas-expand)
  (define-key evil-insert-state-map (kbd "M-/")         'yas-expand)
  (define-key evil-emacs-state-map (kbd "M-/")          'hippie-expand))

(use-package expand-region
  :commands er/expand-region
  :bind ("C-=" . er/expand-region))

(use-package impatient-mode
  :commands impatient-mode)

(use-package ivy
  :diminish ivy-mode
  :ensure counsel
  :ensure swiper
  :bind (:map ivy-minibuffer-map
              ([escape] . minibuffer-keyboard-quit)) 
  :config
  (ivy-mode))

(use-package magit
  :commands magit-status
  :init
  (setq magit-push-always-verify nil)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  :bind
  ("C-x g" . magit-status)
  ("C-x C-g" . magit-status)
  :config
  (with-eval-after-load 'info
    (info-initialize)
    (add-to-list 'Info-directory-list
                 "~/.emacs.d/packages/magit/Documentation/")))

(use-package linum-relative
  :commands linum-relative-mode
  :diminish linum-relative-mode
  :bind
  ("C-x l" . linum-relative-mode)
  :init
  (setq linum-relative-format "%3s "
        linum-relative-current-symbol "")
  (add-hook 'prog-mode-hook (lambda () (linum-relative-mode 1))))

(use-package lorem-ipsum)

(use-package emmet-mode
  :commands emmet-mode
  :bind
  (("C-c C-n" . emmet-next-edit-point)
   ("C-c C-p" . emmet-prev-edit-point))
  :init 
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook  'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode))

(use-package erc
  :commands erc
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

(use-package fix-word
  :bind (("M-u" . fix-word-upcase)
         ("M-l" . fix-word-downcase)
         ("M-c" . fix-word-capitalize)))

(use-package org
  :commands org-mode
  :ensure org-beautify-theme
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c c" . org-capture))
  :init
  (setq org-startup-indented t)
  :config
  (load-theme 'org-beautify t)
  (add-hook 'org-mode-hook (lambda() (toggle-truncate-lines 0))))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package restart-emacs
  :bind
  ("C-x C-r" . restart-emacs))

(use-package restclient
  :commands restclient-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.rest\\'" . restclient-mode)))

(use-package smartparens
  :commands smartparens-mode
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (add-hook 'prog-mode-hook #'smartparens-mode))

(use-package swiper
  :bind (:map swiper-map
              ([escape] . minibuffer-keyboard-quit)))

(use-package switch-window
  :commands switch-window
  :init
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts
        '("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o"))
  :bind
  ("C-x o" . switch-window))

(use-package telephone-line
  :init
  (setq telephone-line-height 32 
        telephone-line-evil-use-short-tag t)
  (setq telephone-line-lhs
        '((evil   . (telephone-line-evil-tag-segment))
          (accent . (telephone-line-vc-segment
                     telephone-line-erc-modified-channels-segment
                     telephone-line-process-segment))
          (nil    . (telephone-line-minor-mode-segment
                     telephone-line-buffer-segment))))

  (setq telephone-line-rhs
        '((nil    . (telephone-line-misc-info-segment))
          (accent . (telephone-line-major-mode-segment))
          (evil   . (telephone-line-airline-position-segment))))

  (custom-set-faces
   '(telephone-line-evil-normal ((t (:inherit telephone-line-evil :background "darkmagenta")))))

  :config
  (telephone-line-mode t))

(use-package try)

(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  :config
  (global-undo-tree-mode))

(use-package web-mode
  :commands web-mode
  :ensure web-mode
  :ensure impatient-mode
  :ensure emmet-mode
  :bind
  (:map web-mode-map ("C-c C-n" . nil)) 
  :init
  (add-to-list 'auto-mode-alist '("\\.x?html\\'" . web-mode))
  (add-to-list 'my/auto-minor-mode-alist '("\\.x?html\\'" . impatient-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  :config 
  (emmet-mode 1))

(use-package yasnippet
  :diminish yas-minor-mode
  :commands yas-minor-mode
  :bind ("C-c y i s" . yas-insert-snippet)
  :init
  (require 'yasnippet)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (define-key yas-minor-mode-map (kbd "C-i") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (yas-reload-all)
  :config
  (yas-minor-mode 1))

(provide 'packages)
;;; packages.el ends here
