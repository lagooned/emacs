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
        ("C-j" . company-complete-selection)
        ("C-d" . company-show-doc-buffer)
        ("M-n" . nil)
        ("M-p" . nil)
        ("<tab>" . nil))
  :init
  (setq company-idle-delay nil)
  :config
  (global-company-mode 1)
  (company-flx-mode +1))

(use-package counsel
  :ensure counsel-projectile
  :diminish counsel-mode
  :config
  (counsel-mode 1))

(use-package counsel-projectile
  :after counsel)

(use-package dired
  :defer t
  :ensure nil
  :after evil
  :init
  (add-hook
   'dired-mode-hook
   (lambda ()
     (progn
       (toggle-truncate-lines 1)
       (message nil))))
  (setq-default dired-omit-files-p t)
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dired-x
  :diminish dired-omit-mode
  :defer t
  :ensure nil
  :after dired
  :bind
  (:map dired-mode-map
        ("C-j" . dired-find-file))
  :config
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
  (setq dired-omit-mode t))

(use-package dired+
  :defer t
  :diminish dired-omit-mode
  :after dired-x
  :init
  (setq diredp-hide-details-initially-flag nil)
  (setq dired-dwim-target t))

(use-package disable-mouse
  :diminish global-disable-mouse-mode
  :config
  (global-disable-mouse-mode))

(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t)
  :config
  (load-theme 'doom-vibrant t))

(use-package dumb-jump
  ;; smart jump calls require
  :defer t
  :bind
  (:map dumb-jump-mode-map
        ("C-M-g" . nil)
        ("C-M-p" . nil)
        ("C-M-q" . nil))
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

(use-package evil-leader
  :commands
  global-evil-leader-mode
  :config
  (load "leader-config.el"))

(use-package evil
  :after evil-leader
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
  :ensure exato
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
  (setq-default evil-escape-delay 0.04))

(use-package evil-magit
  :after
  magit
  evil
  :config
  (require 'evil-magit)
  (evil-define-key
    evil-magit-state
    magit-mode-map [escape] 'nil))

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

(use-package help-mode
  :ensure nil
  :bind
  (:map help-mode-map
        ("C-j" . push-button)))

(use-package indent-guide)

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
     (counsel-git . ivy--regex-ignore-order)
     (t . ivy--regex-plus)))
  :config
  (require 'ivy-hydra)
  (ivy-mode))

(use-package magit
  :commands magit-status
  :init
  (setq magit-push-always-verify nil
        magit-refresh-status-buffer nil
        ediff-window-setup-function 'ediff-setup-windows-plain
        magit-refresh-verbose t)
  (setq vc-handled-backends (delq 'Git vc-handled-backends))
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
  (setq linum-relative-format "%5s "
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

(use-package exato)

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
  (add-hook
   'org-mode-hook
   (lambda ()
     (progn
       (toggle-truncate-lines 0)
       (message nil)))))

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode 1))

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

(use-package ripgrep
  :bind
  (:map ripgrep-search-mode-map
        ("C-j" . compile-goto-error))
  :init
  (require 'ripgrep))

(use-package smart-jump
  :ensure dumb-jump
  :init
  (setq smart-jump-find-references-fallback-function nil
        smart-jump-bind-keys-for-evil nil
        smart-jump-bind-keys nil
        smart-jump-refs-key nil
        smart-jump-pop-key nil)
  :config
  (smart-jump-register :modes '(emacs-lisp-mode lisp-interaction-mode)
                       :jump-fn 'xref-find-definitions
                       :pop-fn 'pop-tag-mark
                       :refs-fn 'gmacs/xref-find-apropos-symbol
                       :should-jump t
                       :heuristic 'error
                       :async nil))

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
  (setq telephone-line-height 24
        telephone-line-separator-extra-padding 1
        ;; telephone-line-primary-left-separator 'telephone-line-flat
        telephone-line-primary-left-separator 'telephone-line-abs-left
        ;; telephone-line-secondary-left-separator 'telephone-line-flat
        telephone-line-secondary-left-separator 'telephone-line-abs-left)

  (setq telephone-line-lhs
        '((evil . (telephone-line-simple-major-mode-segment))
          (accent . (telephone-line-simple-minor-mode-segment))
          (nil . (telephone-line-buffer-segment))
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

(use-package try
  :defer t)

(use-package undo-tree
  :diminish undo-tree-mode
  :commands undo-tree-visualize
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
   'auto-mode-alist
   '("\\.x?html\\'" . web-mode))
  (add-to-list
   'gmacs/auto-minor-mode-alist
   '("\\.x?html\\'" . impatient-mode))
  (add-to-list
   'auto-mode-alist
   '("\\.php\\'" . web-mode))
  :config
  (emmet-mode 1))

(use-package winner
  :ensure nil
  :config
  (winner-mode 1))

(use-package which-key
  :diminish
  which-key-mode
  :config
  (which-key-setup-minibuffer)
  (which-key-add-key-based-replacements
    "SPC ;" "M-x"
    "SPC b" "buffer"
    "SPC e" "emacs"
    "SPC f" "file"
    "SPC g" "grep"
    "SPC G" "grep buffer"
    "SPC h" "help"
    "SPC h d" "describe"
    "SPC h v" "view"
    "SPC i" "insert"
    "SPC j" "jump"
    "SPC n" "narrow"
    "SPC o" "org"
    "SPC p" "project"
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
  (setq whitespace-line-column 100)
  ;; replace ascii spaces unicode spaces
  (advice-add
   'linum-relative :filter-return
   (lambda (num)
     (if (not (get-text-property 0 'invisible num))
         (propertize
          (replace-regexp-in-string " " "\u2002" num)
          'face (get-text-property 0 'face num))))))

(use-package xref
  :ensure nil
  :bind
  (:map xref--button-map
        ("C-j" . xref-goto-xref))
  :init
  (setq xref-after-jump-hook '(recenter)
        xref-after-return-hook '(recenter)))

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

(use-package zoom
  :diminish zoom-mode
  :config
  (setq zoom-size '(0.618 . 0.618)))

(use-package zop-to-char)

(provide 'packages)
;;; packages.el ends here
