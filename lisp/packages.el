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

(use-package avy)

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

(use-package centered-cursor-mode)

(use-package company
  :diminish
  company-mode
  :commands
  company-complete
  company-mode
  :bind (:map company-active-map
              ("C-n" . company-select-next-or-abort)
              ("C-p" . company-select-previous-or-abort)
              ("C-d" . company-show-doc-buffer)
              ("<tab>" . nil))

  :init
  (setq company-idle-delay nil))

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
  (load-theme 'doom-vibrant t))

(use-package eldoc
  :diminish eldoc-mode
  :commands turn-on-eldoc-mode
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode))

(use-package evil
  :ensure avy
  :ensure counsel
  :ensure company
  :ensure evil-ediff
  :ensure evil-escape
  :ensure evil-exchange
  :ensure evil-leader
  :ensure evil-matchit
  :ensure evil-numbers
  :ensure evil-surround
  :ensure evil-vimish-fold
  :ensure evil-visualstar
  :ensure undo-tree
  :ensure golden-ratio
  :diminish evil-vimish-fold-mode
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (global-evil-leader-mode 1)
  (global-evil-matchit-mode 1)
  (global-evil-surround-mode 1)
  (global-evil-visualstar-mode 1)
  (evil-vimish-fold-mode 1)
  (evil-exchange-install)
  (evil-escape-mode)
  (golden-ratio-mode 1)
  (global-company-mode 1)
  (load "evil-config.el"))

(use-package evil-escape
  :init
  (setq-default evil-escape-key-sequence "kj")
  (setq-default evil-escape-unordered-key-sequence t)
  (setq-default evil-escape-delay 0.1))

(use-package evil-leader
  :commands
  global-evil-leader-mode
  :config
  (load "leader-config.el"))

(use-package evil-magit
  :after magit evil
  :config
  (require 'evil-magit))

(use-package evil-numbers
  :commands
  evil-numbers/inc-at-point
  evil-numbers/dev-at-point
  :config
  (require 'evil-numbers))

(use-package evil-org
  :diminish evil-org-mode
  :after org
  :init
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme))))

(use-package expand-region
  :commands er/expand-region)

(use-package golden-ratio
  :diminish
  golden-ratio-mode
  :commands
  golden-ratio-mode
  :init
  (setq golden-ratio-extra-commands
        (append golden-ratio-extra-commands
                '(evil-window-left
                  evil-window-right
                  evil-window-up
                  evil-window-down
                  evil-window-next
                  evil-window-prev
                  select-window-1
                  select-window-2
                  select-window-3
                  select-window-4
                  select-window-5)))
  :config
  (golden-ratio-mode 1))

(use-package impatient-mode
  :commands impatient-mode)

(use-package ivy
  :diminish ivy-mode
  :ensure ivy-hydra
  :ensure counsel
  :ensure swiper
  :ensure avy
  :config
  (ivy-mode))

(use-package magit
  :commands magit-status
  :init
  (setq magit-push-always-verify nil)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  :config
  (with-eval-after-load 'info
    (info-initialize)
    (add-to-list 'Info-directory-list
                 "~/.emacs.d/packages/magit/Documentation/")))

(use-package linum-relative
  :commands linum-relative-mode
  :diminish linum-relative-mode
  :init
  (setq linum-relative-format "%3s "
        linum-relative-current-symbol "")
  (add-hook 'prog-mode-hook (lambda () (linum-relative-mode 1))))

(use-package lorem-ipsum)

(use-package emmet-mode
  :commands
  emmet-mode
  emmet-next-edit-point
  emmet-prev-edit-point
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
  :commands
  fix-word-upcase
  fix-word-downcase
  fix-word-capitalize)

(use-package org
  :commands
  org-mode
  org-store-link
  org-agenda
  org-iswitchb
  org-capture
  :ensure org-beautify-theme
  :init
  (setq org-startup-indented t
        org-log-done t
        org-agenda-files (list "~/org/work.org"
                               "~/org/home.org")
        org-beautify-theme-use-box-hack nil)
  :config
  (load-theme 'org-beautify t)
  (custom-theme-set-faces 'org-beautify
                          `(org-done
                            ((t (:strike-through nil))))
                          `(org-headline-done
                            ((t (:strike-through nil)))))
  (add-hook 'org-mode-hook (lambda() (toggle-truncate-lines 0))))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package restart-emacs
  :commands restart-emacs)

(use-package restclient
  :commands restclient-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.rest\\'" . restclient-mode)))

(use-package smartparens
  :commands smartparens-mode
  :diminish smartparens-mode
  :init
  (add-hook 'prog-mode-hook #'smartparens-mode)
  :config
  (require 'smartparens-config))

(use-package swiper
  :commands
  swiper)

(use-package telephone-line
  :init
  (setq telephone-line-primary-left-separator 'telephone-line-identity-right
        telephone-line-primary-right-separator 'telephone-line-identify-left)
  (setq telephone-line-height 32
        telephone-line-evil-use-short-tag t)

  (setq telephone-line-lhs
        '((evil . ())
          (evil . (telephone-line-simple-major-mode-segment))
          (evil . ())
          (accent . ())
          (accent . (telephone-line-simple-minor-mode-segment))
          (accent . ())
          (nil . ())
          (nil . (telephone-line-buffer-segment))
          (nil . ())
          (nil . (telephone-line-airline-position-segment))
          (nil . ())
          (nil . (telephone-line-vc-segment))))

  (setq telephone-line-rhs
        '((nil . ())))

  (custom-set-faces
   '(telephone-line-evil-normal
     ((t (:inherit telephone-line-evil :background "darkmagenta")))))

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
  :init
  (add-hook 'web-mode-hook 'electric-indent-mode)
  (add-to-list 'auto-mode-alist '("\\.x?html\\'" . web-mode))
  (add-to-list 'my/auto-minor-mode-alist '("\\.x?html\\'" . impatient-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  :config
  (emmet-mode 1))

(use-package which-key
  :diminish
  which-key-mode
  :config
  (which-key-setup-minibuffer)
  (which-key-mode 1))

(use-package yasnippet
  :diminish yas-minor-mode
  :commands
  yas-minor-mode
  yas-insert-snippet
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
