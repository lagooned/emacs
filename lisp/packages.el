;;; packages.el --- package configuragion            -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: gmacs, packages

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

(use-package all-the-icons)

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
  :ensure company-flx
  :diminish
  company-mode 
  :commands
  company-complete
  company-mode
  global-company-mode
  :bind
  (:map company-active-map
        ("C-n" . company-select-next-or-abort)
        ("C-p" . company-select-previous-or-abort)
        ("C-d" . company-show-doc-buffer)
        ("<tab>" . nil))
  :init
  (setq company-idle-delay nil)
  :config
  (global-company-mode 1)
  (company-flx-mode +1))

(use-package counsel
  :diminish counsel-mode
  :config
  (counsel-mode 1))

(use-package dired
  :defer t
  :ensure nil
  :after evil
  :init
  (setq-default dired-omit-files-p t)
  (add-hook 'dired-mode-hook (lambda() (toggle-truncate-lines 1)))
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dired+
  :defer t
  :diminish dired-omit-mode
  :after dired
  :init
  (setq diredp-hide-details-initially-flag nil)
  (setq dired-dwim-target t))

(use-package dired-x
  :defer t
  :ensure nil
  :after dired+
  :bind
  (:map dired-mode-map
        ("C-j" . dired-find-file))
  :config
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
  (setq dired-omit-mode t))

(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t)
  :config
  (load-theme 'doom-vibrant t))

(use-package dumb-jump
  :init
  (setq dumb-jump-selector 'ivy
        dumb-jump-prefer-searcher 'rg
        dumb-jump-quiet t)
  (when (eq system-type 'windows-nt)
    (setq dumb-jump-force-searcher 'rg)))

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
  :ensure evil-commentary
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
  (load "evil-init.el")
  :config
  (load "evil-config.el"))

(use-package evil-commentary
  :diminish
  evil-commentary-mode
  :commands
  evil-commentary-mode
  :config
  (evil-commentary-mode))

(use-package evil-escape
  :init
  (setq-default evil-escape-key-sequence "kj")
  (setq-default evil-escape-unordered-key-sequence t)
  (setq-default evil-escape-delay 0.05))

(use-package evil-leader
  :ensure indent-guide
  :commands
  global-evil-leader-mode
  :config
  (load "leader-config.el"))

(use-package evil-magit
  :after
  magit
  evil
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
                '(top-level
                  magit-status
                  evil-window-left
                  evil-window-right
                  evil-window-up
                  evil-window-down
                  evil-window-next
                  evil-window-prev
                  select-window-1
                  select-window-2
                  select-window-3
                  select-window-4
                  select-window-5
                  keyboard-quit
                  ivy-done
                  ivy-alt-done)))
  :config
  (golden-ratio-mode 1))

(use-package hidden-mode-line-mode
  :ensure nil)

(use-package impatient-mode
  :commands impatient-mode)

(use-package ivy
  :diminish ivy-mode
  :ensure ivy-hydra
  :ensure counsel
  :ensure swiper
  :ensure avy
  :bind
  (:map ivy-minibuffer-map
        ("M--" . counsel-up-directory))
  :init
  (setq
   ivy-re-builders-alist
   '((ivy-switch-buffer . ivy--regex-ignore-order)
     (counsel-M-x . ivy--regex-ignore-order)
     (t . ivy--regex-plus)))
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
    (add-to-list
     'Info-directory-list
     "~/.emacs.d/packages/magit/Documentation/")))

(use-package linum-relative
  :commands linum-relative-mode
  :diminish linum-relative-mode
  :init
  (setq linum-relative-format "%5s"
        linum-relative-current-symbol "")
  (add-hook
   'prog-mode-hook (lambda () (linum-relative-mode 1))))

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

(use-package focus
  :commands
  focus-mode)

(use-package org
  :commands
  org-mode
  org-store-link
  org-agenda
  org-iswitchb
  org-capture
  :init
  (setq org-startup-indented t
        org-log-done t
        org-agenda-files (list "~/org/work.org"
                               "~/org/home.org"))
  :config
  (add-to-list 'org-file-apps '(directory . emacs))
  (set-face-attribute 'org-level-1 nil :height 1.0)
  (add-hook 'org-mode-hook (lambda() (toggle-truncate-lines 0))))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package restart-emacs
  :commands restart-emacs)

(use-package restclient
  :commands restclient-mode
  :init
  (add-to-list
   'auto-mode-alist
   '("\\.rest\\'" . restclient-mode)))

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
  (require 'telephone-line-config)
  (setq telephone-line-height 30
        telephone-line-separator-extra-padding 1
        telephone-line-primary-left-separator 'telephone-line-flat
        telephone-line-secondary-left-separator 'telephone-line-flat
        telephone-line-primary-right-separator 'telephone-line-flat
        telephone-line-secondary-right-separator 'telephone-line-flat)
  (setq telephone-line-lhs
        '((evil . (telephone-line-simple-major-mode-segment))
          (accent . (telephone-line-simple-minor-mode-segment))
          (nil . (telephone-line-buffer-segment))
          (nil . (telephone-line-vc-segment))
          (nil . (telephone-line-airline-position-segment))))
  (setq telephone-line-rhs nil)
  (custom-set-faces
   '(telephone-line-evil-normal
     ((t (:inherit telephone-line-evil :background "darkmagenta"))))
   '(telephone-line-evil-replace
     ((t (:inherit telephone-line-evil :background "darkcyan"))))
   '(telephone-line-evil-emacs
     ((t (:inherit telephone-line-evil :background "red")))))
  (telephone-line-mode 1))

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
  (add-hook
   'web-mode-hook
   'electric-indent-mode)
  (add-to-list
   'auto-mode-alist '("\\.x?html\\'" . web-mode))
  (add-to-list
   'my/auto-minor-mode-alist '("\\.x?html\\'" . impatient-mode))
  (add-to-list
   'auto-mode-alist '("\\.php\\'" . web-mode))
  :config
  (emmet-mode 1))

(use-package which-key
  :diminish
  which-key-mode
  :config
  (which-key-setup-minibuffer)
  (which-key-add-key-based-replacements
    "SPC SPC" "M-x"
    "SPC b" "buffer"
    "SPC c" "case"
    "SPC e" "editor"
    "SPC f" "file"
    "SPC g" "grep"
    "SPC h" "help"
    "SPC i" "insert"
    "SPC j" "jump"
    "SPC j a" "away"
    "SPC l" "insert"
    "SPC o" "org"
    "SPC p" "project"
    "SPC p c" "current"
    "SPC s" "search"
    "SPC t" "toggle"
    "SPC u" "univ arg"
    "SPC U" "negt arg"
    "SPC w" "window")
  (which-key-mode 1))

(use-package whitespace-mode
  :ensure nil
  :init
  (require 'whitespace)
  (setq whitespace-line-column 100))

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
