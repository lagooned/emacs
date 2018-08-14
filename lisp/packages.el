;;; packages.el --- package configuration            -*- lexical-binding: t; -*-

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

;;; Commentary:

;; Gmacs package configuration.

;;; Code:

(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
(defvar use-package-always-ensure)
(setq use-package-always-ensure t)
(require 'use-package)

(use-package all-the-icons)

(use-package autorevert
  :ensure nil
  :init
  (defvar auto-revert-verbose)
  (setq auto-revert-verbose nil)
  (add-hook
   'auto-revert-mode-hook
   (lambda () (diminish 'auto-revert-mode)))
  (global-auto-revert-mode 1))

(use-package avy)

(use-package adaptive-wrap
  :commands visual-line-mode
  :init
  (setq-default adaptive-wrap-extra-indent 2)
  (add-hook
   'visual-line-mode-hook
   (lambda ()
     (adaptive-wrap-prefix-mode +1)
     (diminish 'visual-line-mode))))

(use-package aggressive-indent
  :commands aggressive-indent-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'css-mode-hook #'aggressive-indent-mode))

(use-package centered-cursor-mode
  :commands
  centered-cursor-mode)

(use-package company
  :diminish company-mode
  :commands company-mode
  :bind
  (:map company-active-map
        ("<tab>"  . nil)
        ("<return>" . nil)
        ("RET" . nil)
        ("\e\e\e" . nil)
        ("M-n"    . company-select-next-or-abort)
        ("M-p"    . company-select-previous-or-abort)
        ("M-i"    . company-complete-common-or-cycle)
        ("C-n"    . gmacs/company-cancel-complete-next)
        ("C-p"    . gmacs/company-cancel-complete-prev)
        ("M-j"    . company-complete-selection)
        ("M-m"    . company-complete-selection)
        ("C-v"    . company-next-page)
        ("M-v"    . company-previous-page))
  :init
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 2)
  (setq company-show-numbers t)
  :config
  (setq company-backends nil))

(use-package counsel
  :ensure counsel-projectile
  :diminish counsel-mode
  :config
  (counsel-mode 1))

(use-package counsel-projectile
  :after counsel)

(use-package dired
  :defer t
  :after evil
  :ensure nil
  :bind
  (:map dired-mode-map
        ("C-j" . dired-find-file))
  :init
  (add-hook
   'dired-mode-hook
   (lambda ()
     (progn
       (toggle-truncate-lines 1)
       (message nil))))
  (setq-default dired-omit-files-p t
                dired-hide-details-hide-symlink-targets nil)
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dired-x
  :defer t
  :ensure nil
  :after dired
  :config
  (setq dired-omit-files
        (concat dired-omit-files
                "\\|^\\..+$\\|^~.+$\\|^#.+$"))
  (setq dired-omit-mode t))

(use-package dired+
  :defer t
  :after dired-x
  :init
  (defvar diredp-hide-details-initially-flag)
  (setq diredp-hide-details-initially-flag t)
  ;; (setq diredp-hide-details-initially-flag nil)
  (setq dired-dwim-target t))

(use-package doom-themes
  :init
  (defvar doom-themes-enable-bold)
  (setq doom-themes-enable-bold nil)
  :config
  (load-theme 'doom-vibrant t))

(use-package ediff
  :init
  (defvar ediff-diff-options)
  (defvar ediff-window-setup-function)
  (defvar ediff-split-window-function)
  (setq ediff-diff-options "-w"
        ediff-window-setup-function #'ediff-setup-windows-plain)
  (setq ediff-split-window-function
        (if (> (frame-width) 150)
            'split-window-horizontally
          'split-window-vertically)))

(use-package elscreen
  :init
  (setq elscreen-tab-display-kill-screen nil
        elscreen-tab-display-control nil
        elscreen-display-tab nil)
  :config
  (elscreen-start))

(use-package erc
  :commands erc
  :config
  (require 'erc-services nil t)
  (erc-services-mode 1)
  (defvar erc-prompt-for-nickserv-password)
  (setq erc-prompt-for-nickserv-password nil)
  (setq erc-hide-list '("PART" "QUIT" "JOIN"))
  (setq erc-server-coding-system '(utf-8 . utf-8)
        erc-server "irc.freenode.net"
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

(use-package eshell
  :init
  (setq eshell-banner-message 'gmacs/eshell-message
        eshell-prompt-function 'gmacs/eshell-prompt-function
        eshell-prompt-regexp (eval 'gmacs/eshell-prompt-regexp))
  :config
  (with-eval-after-load 'em-term (push "ngrok" eshell-visual-commands))
  (with-eval-after-load 'em-hist (setq eshell-hist-ignoredups t)))

(use-package evil
  :init
  (load "evil-init.el")
  :config
  (load "evil-config.el"))

(use-package expand-region
  :commands er/expand-region)

(use-package exec-path-from-shell)

(use-package fix-word
  :commands
  fix-word-upcase
  fix-word-downcase
  fix-word-capitalize)

(use-package flyspell
  :defer t
  :diminish flyspell-mode "spl"
  :commands flyspell-mode
  :init
  (custom-set-faces
   '(flyspell-duplicate ((t (:underline "Green"))))
   '(flyspell-incorrect ((t (:underline "Magenta")))))
  (setq flyspell-issue-message-flag nil))

(use-package focus
  :commands
  focus-mode)

(use-package git-gutter
  :diminish git-gutter-mode "gg"
  :config
  (require 'git-gutter-fringe)
  (set-face-foreground 'git-gutter-fr:modified "darkorange")
  (set-face-foreground 'git-gutter-fr:added    "darkorange")
  (set-face-foreground 'git-gutter-fr:deleted  "darkorange")
  (fringe-helper-define 'git-gutter-fr:added nil
    "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..."
    "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..."
    "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..."
    "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..."
    "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..."
    "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..."
    "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..."
    "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..."
    "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..."
    "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX...")
  (fringe-helper-define 'git-gutter-fr:deleted nil
    "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..."
    "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..."
    "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..."
    "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..."
    "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..."
    "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..."
    "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..."
    "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..."
    "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..."
    "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX...")
  (fringe-helper-define 'git-gutter-fr:modified nil
    "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..."
    "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..."
    "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..."
    "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..."
    "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..."
    "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..."
    "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..."
    "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..."
    "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..."
    "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX..." "XXXXX...")
  (add-hook
   'prog-mode-hook
   (lambda () (git-gutter-mode 1))))

(use-package help
  :ensure nil
  :bind
  (:map help-mode-map
        ("C-j" . push-button)))

(use-package hi-lock
  :diminish hi-lock-mode "hi"
  :ensure nil
  :init
  (defface hi-magenta
    '((((background dark)) (:background "magenta" :foreground "black"))
      (t (:background "magenta")))
    "Custom magenta face for hi-lock mode."
    :group 'hi-lock-faces)
  (defvar hi-lock-face-defaults)
  (setq hi-lock-face-defaults
        '("hi-yellow"
          "hi-pink"
          "hi-green"
          "hi-blue"
          "hi-magenta")))

(use-package indent-guide
  :defer t
  :commands indent-guide-mode)

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

(use-package linum-relative
  :commands linum-relative-mode
  :diminish linum-relative-mode
  :init
  (setq linum-relative-format "%5s "
        linum-relative-current-symbol ""))

(use-package lorem-ipsum)

(use-package magit
  :commands magit-status
  :init
  (defvar magit-push-always-verify)
  (setq magit-push-always-verify nil
        magit-refresh-status-buffer nil
        magit-refresh-verbose t)
  (setq vc-handled-backends (delq 'Git vc-handled-backends))
  :config
  (with-eval-after-load 'info
    (info-initialize)
    (add-to-list
     'Info-directory-list
     "~/.emacs.d/packages/magit/Documentation/")))

(use-package multiple-cursors
  :config
  (add-hook 'multiple-cursors-mode-enabled-hook
            'mc-evil-compat/switch-to-emacs-state)
  (add-hook 'multiple-cursors-mode-disabled-hook
            'mc-evil-compat/back-to-previous-state))

(use-package multi-term)

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
  :init
  (setq projectile-indexing-method 'alien)
  :config
  (projectile-mode 1))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package recentf
  :defer t
  :ensure nil
  :commands counsel-recentf
  :config
  (setq recentf-max-menu-items 0
        recentf-max-saved-items 999
        recentf-filename-handlers '(file-truename)
        recentf-exclude
        (list "^/tmp/"
              "^/ssh:"
              "\\.?ido\\.last$"
              "\\.revive$"
              "/TAGS$"
              "^/var/folders/.+$"
              (concat "^" (expand-file-name "~/\\(.emacs.d\\|emacs\\)/workspace") "/.+$")
              "^#")))

(use-package restart-emacs
  :commands restart-emacs)

(use-package restclient
  :commands restclient-mode
  :init
  (add-to-list
   'auto-mode-alist
   '("\\.rest\\'" . restclient-mode)))

(use-package ripgrep
  :defer t
  :commands
  ripgrep
  gmacs/ripgrep-regexp-git
  :bind
  (:map ripgrep-search-mode-map
        ("C-j" . compile-goto-error))
  :init
  (require 'ripgrep))

(use-package shell-pop
  :defer t
  :commands shell-pop
  :init
  (defvar shell-pop-shell-type)
  (defvar shell-pop-full-span)
  (defvar shell-pop-window-position)
  (setq shell-pop-shell-type '("eshell" "*eshell*" (lambda nil (eshell shell-pop-term-shell)))
        shell-pop-full-span t
        shell-pop-window-position "bottom"))

(use-package smartparens
  :commands smartparens-mode
  :diminish smartparens-mode "sp"
  :init
  (add-hook 'prog-mode-hook #'smartparens-mode)
  :config
  (require 'smartparens-config)
  (setq sp-autowrap-region nil
        sp-highlight-pair-overlay nil
        sp-cancel-autoskip-on-backward-movement nil
        sp-show-pair-delay 0
        sp-max-pair-length 3)
  (add-hook 'evil-replace-state-entry-hook #'turn-off-smartparens-mode)
  (add-hook 'evil-replace-state-exit-hook  #'turn-on-smartparens-mode))

(use-package swiper
  :commands swiper)

(use-package suggest
  :defer t
  :commands suggest)

(use-package telephone-line
  :init
  (require 'telephone-line-config)
  (defvar telephone-line-height)
  (defvar telephone-line-separator-extra-padding)
  (defvar telephone-line-primary-left-separator)
  (defvar telephone-line-secondary-left-separator)
  (defvar telephone-line-primary-right-separator)
  (defvar telephone-line-secondary-right-separator)
  (setq telephone-line-height 20
        ;; telephone-line-separator-extra-padding 1
        ;; telephone-line-primary-left-separator 'telephone-line-identity-left
        ;; telephone-line-secondary-left-separator 'telephone-line-identity-left
        ;; telephone-line-primary-right-separator 'telephone-line-identity-right
        ;; telephone-line-secondary-right-separator 'telephone-line-identity-right
        ;; telephone-line-primary-left-separator 'telephone-line-abs-left
        ;; telephone-line-secondary-left-separator 'telephone-line-abs-left
        ;; telephone-line-primary-right-separator 'telephone-line-abs-right
        ;; telephone-line-secondary-right-separator 'telephone-line-abs-right
        telephone-line-primary-left-separator 'telephone-line-flat
        telephone-line-secondary-left-separator 'telephone-line-flat
        telephone-line-primary-right-separator 'telephone-line-flat
        telephone-line-secondary-right-separator 'telephone-line-flat
        )
  (defvar telephone-line-lhs)
  (defface telephone-line-elscreen '((t (:foreground "black" :background "grey77")))
    "Elscreen telephone-line segment theme.")
  (setq telephone-line-faces
        '((evil . telephone-line-modal-face)
          (modal . telephone-line-modal-face)
          (elscreen . (telephone-line-elscreen . mode-line-inactive))
          (ryo . telephone-line-ryo-modal-face)
          (accent . (telephone-line-accent-active . mode-line-inactive))
          (nil . (mode-line . mode-line-inactive))))
  (telephone-line-defsegment* telephone-line-elscreen-mode-line-string-segment ()
    (telephone-line-raw elscreen-mode-line-string t))
  (setq telephone-line-lhs '((elscreen . (telephone-line-elscreen-mode-line-string-segment))
                             (evil . (telephone-line-simple-major-mode-segment))
                             (accent . (telephone-line-simple-minor-mode-segment))
                             (nil . (telephone-line-buffer-segment))
                             (nil . (telephone-line-airline-position-segment))))
  (defvar telephone-line-rhs)
  (setq telephone-line-rhs '((nil . ())))
  (custom-set-faces
   '(telephone-line-evil-normal
     ((t (:inherit telephone-line-evil :background "darkmagenta"))))
   '(telephone-line-evil-replace
     ((t (:inherit telephone-line-evil :background "darkcyan"))))
   '(telephone-line-evil-emacs
     ((t (:inherit telephone-line-evil :background "red")))))
  (telephone-line-mode 1)
  ;; fix in-window modeline fragements on quit
  (add-hook 'minibuffer-exit-hook #'redraw-display))

(use-package tiny
  :config
  (tiny-setup-default))

(use-package try
  :defer t
  :commands try)

(use-package undo-tree
  :diminish undo-tree-mode
  :commands undo-tree-visualize
  :init
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  :config
  (global-undo-tree-mode))

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'reverse
        uniquify-after-kill-buffer-p t
        uniquify-separator ":"
        uniquify-ignore-buffers-re "^\\*"))

(use-package winner
  :ensure nil
  :config
  (winner-mode 1))

(use-package which-key
  :diminish
  which-key-mode
  :config
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 5)
  (which-key-setup-minibuffer)
  (which-key-add-key-based-replacements
    "SPC ;" "M-x"
    "SPC !" "term"
    "SPC b" "buffer"
    "SPC TAB" "last buffer"
    "SPC c" "config"
    "SPC f" "file"
    "SPC g" "grep"
    "SPC G" "grep buffer"
    "SPC h" "help"
    "SPC h d" "describe"
    "SPC h v" "view"
    "SPC i" "insert"
    "SPC j" "jump"
    "SPC m" "mode"
    "SPC n" "narrow"
    "SPC o" "org"
    "SPC p" "project"
    "SPC s" "search"
    "SPC t" "toggle"
    "SPC u" "univ arg"
    "SPC U" "negt arg"
    "SPC w" "window"
    "SPC q" "quit"
    "SPC z" "screen"
    "SPC =" "^v ++"
    "SPC -" "^v --"
    "SPC ]" "<> ++"
    "SPC [" "<> --")
  (which-key-mode 1))

(use-package whitespace
  :ensure nil
  :init
  (setq whitespace-display-mappings
        '((tab-mark ?\t [?› ?\t])
          (newline-mark ?\n [?¬ ?\n])
          (space-mark ?\  [?·] [?.])))
  ;; replace ascii spaces unicode spaces
  (advice-add
   'linum-relative :filter-return
   (lambda (num)
     (if (not (get-text-property 0 'invisible num))
         (propertize
          (replace-regexp-in-string " " "\u2002" num)
          'face (get-text-property 0 'face num)))))
  :config
  (setq whitespace-line-column 100))

(use-package yasnippet
  :commands
  yas-minor-mode-on
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode-on)
  :config
  (require 'yasnippet)
  (define-key yas-minor-mode-map (kbd "C-i") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (yas-reload-all))

(use-package zoom
  :commands zoom-mode
  :diminish zoom-mode
  :config
  (setq zoom-size '(0.618 . 0.618)
        zoom-ignored-major-modes '(term-mode)
        zoom-ignored-buffer-name-regexps '("^*calc")
        zoom-ignore-predicates
        '((lambda ()
            ;; visible term or ediff window
            (string-match
             "term\\|ediff\\|[^e]shell"
             (downcase
              (format
               "%s"
               (mapcar
                '(lambda (window)
                   (buffer-name (window-buffer window)))
                (window-list)))))))))

(use-package zop-to-char
  :commands zop-to-char)

(provide 'packages)
;;; packages.el ends here
