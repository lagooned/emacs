;;; packages.el --- package configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: jeemacs, config, packages

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

;; Je/Emacs package configuration.

;;; Code:

(je/ensure-use-package)

(require 'use-package)

(use-package abbrev
  :defer t
  :diminish abbrev-mode)

(use-package alpha
  :commands transparency-set-value)

(use-package autorevert
  :after evil-leader
  :commands auto-revert-mode
  :diminish auto-revert-mode
  :init
  (setq auto-revert-verbose nil)
  (evil-leader/set-key
    "t a" 'auto-revert-mode))

(use-package avy
  :defer t)

(use-package aggressive-indent
  :after evil-leader
  :diminish aggressive-indent-mode
  :commands aggressive-indent-mode
  :init
  (evil-leader/set-key "t i" 'aggressive-indent-mode))

(use-package company
  :diminish company-mode
  :commands company-mode
  :hook (prog-mode . company-mode)
  :bind
  (:map company-active-map
        ("<tab>" . nil)
        ("<return>" . nil)
        ("RET" . nil)
        ("\e\e\e" . nil)
        ("C-j" . company-select-next-or-abort)
        ("C-k" . company-select-previous-or-abort)
        ("C-n" . je/company-cancel-complete-next)
        ("C-p" . je/company-cancel-complete-prev)
        ("C-i" . company-complete-common-or-cycle)
        ("C-m" . company-complete-selection)
        ("C-u" . company-previous-page)
        ("C-d" . company-next-page))
  :init
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-show-numbers t)
  :config
  (setq company-backends nil)
  (make-variable-buffer-local 'company-backends))

(use-package counsel
  :commands counsel-mode
  :hook (emacs-startup . counsel-mode)
  :diminish counsel-mode)

(use-package counsel-projectile
  :config
  (setq
   counsel-projectile-switch-project-action
   '(1
     ("o" counsel-projectile-switch-project-action-dired
      "open project in dired")
     ("k" counsel-projectile-switch-project-action-remove-known-project
      "remove project from known projects"))))

(use-package dashboard
  :after
  (evil-collection projectile)
  :commands
  dashboard-insert-startupify-lists
  :init
  (add-hook
   'kill-buffer-query-functions
   #'je/dont-kill-dashboard)
  (setq
   dashboard-startup-banner 99
   dashboard-items
   '((recents  . 10)
     (projects . 10)))
  (dashboard-setup-startup-hook)
  :config
  (je/print-to-file (dashboard-get-banner-path 99) je/logo)
  (evil-collection-define-key 'normal 'dashboard-mode-map
    (kbd "RET") 'dashboard-return))

(use-package diminish)

(use-package dimmer
  :commands dimmer-mode
  :hook (emacs-startup . dimmer-mode)
  :config
  (setq dimmer-fraction 0.15)
  (dimmer-configure-which-key))

(use-package dired
  :hook
  ((dired-mode . je/rename-dired-buffer)
   (dired-mode . auto-revert-mode)
   (dired-mode . je/enable-truncate-lines-no-message)
   (dired-mode . dired-hide-details-mode))
  :init
  (add-hook 'kill-buffer-query-functions #'je/dont-kill-dired)
  (setq dired-listing-switches "-lah")
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dired-x
  :after dired
  :config
  (setq-default dired-omit-mode 1)
  (setq dired-omit-files
        (concat dired-omit-files
                "\\|^\\..+$\\|^~.+$\\|^#.+$")))

(use-package ediff
  :commands ediff-mode
  :init
  (setq ediff-diff-options "-w"
        ediff-window-setup-function #'ediff-setup-windows-plain)
  (setq ediff-split-window-function
        (if (> (frame-width) 150)
            'split-window-horizontally
          'split-window-vertically)))

(use-package elscreen
  :commands elscreen-start
  :hook (emacs-startup . elscreen-start)
  :init
  (setq elscreen-tab-display-kill-screen nil
        elscreen-tab-display-control nil
        elscreen-display-tab nil
        elscreen-display-screen-number nil)
  :config
  (je/setup-elscreen))

(use-package eshell
  :hook (eshell-mode . je/enable-truncate-lines-no-message)
  :config
  (with-eval-after-load 'em-term (push 'eshell-truncate-buffer eshell-output-filter-functions))
  (with-eval-after-load 'em-hist (setq eshell-hist-ignoredups t)))

(use-package evil
  :commands evil-mode
  :hook
  ((with-editor-mode . evil-insert-state)
   (evil-command-window-mode . je/evil-command-window-setup)
   (eshell-mode . je/evil-eshell-mode-setup)
   (minibuffer-setup . je/evil-minibuffer-setup)
   (org-mode . je/evil-org-mode-setup)
   (c-mode-common . je/evil-c-common-mode-setup)
   (evil-insert-state-exit . je/evil-company-abort-on-insert-leave)
   (evil-mode . je/evil-jumplist-setup)
   (emacs-startup . evil-mode))
  :init
  (setq
   evil-move-cursor-back nil
   evil-insert-state-message nil
   evil-visual-state-message nil
   evil-replace-state-message nil
   evil-normal-state-message nil
   evil-emacs-state-message nil
   evil-motion-state-message nil
   evil-insert-state-message nil
   evil-operator-state-message nil
   evil-want-Y-yank-to-eol t
   evil-want-keybinding nil
   evil-jumps-max-length 20
   evil-mode-line-format '(before . mode-line-front-space))
  (setq-default mode-line-format (je/create-mode-line-format))
  :config
  (setq evil-normal-state-cursor `("#00ffff" (hbar . #xffff)))
  (setq evil-insert-state-cursor `("#00e000" (hbar . #xffff)))
  (setq evil-visual-state-cursor `("#ff8800" (hbar . #xffff)))
  (setq evil-emacs-state-cursor `("#ff0000" (hbar . #xffff)))
  (setq evil-motion-state-cursor `("#0055ff" (hbar . #xffff)))
  (setq evil-replace-state-cursor `("#00acff" (hbar . #xffff)))
  (setq evil-operator-state-cursor `("#ff66ff" (hbar . #xffff)))
  (load "evil-config"))

(use-package evil-collection
  :after evil
  :config
  (je/configure-evil-collection-mode-list)
  (evil-collection-init)
  (evil-collection-define-key 'normal 'dired-mode-map
    "." 'dired-omit-mode
    "B" 'evil-backward-WORD-begin
    "p" 'projectile-vc
    "W" 'evil-forward-WORD-begin
    "v" 'evil-visual-char
    "V" 'evil-visual-line
    "/" 'evil-search-forward
    "?" 'evil-search-backward
    "n" 'evil-search-next
    "N" 'evil-search-previous
    (kbd "C-v") 'evil-visual-block
    (kbd "C-j") 'dired-find-file-other-window
    (kbd "C-i") 'evil-jump-forward
    (kbd "C-o") 'evil-jump-backward
    (kbd "<return>") 'dired-find-file)
  (evil-collection-define-key 'insert 'term-raw-map
    (kbd "<return>") 'term-send-input)
  (evil-collection-define-key 'normal 'term-mode-map
    (kbd "<return>") 'void))

(use-package evil-leader
  :after evil
  :init
  (setq evil-leader/in-all-states 1)
  :config
  (evil-leader/set-leader (concat "<" je/leader-key ">"))
  (evil-leader/set-key
    "b b" 'switch-to-buffer
    "b k" 'kill-this-buffer
    "b K" 'kill-buffer
    "b l" 'ibuffer
    "b r" 'rename-buffer
    "b c" 'clean-buffer-list
    "b s" 'save-some-buffers
    "TAB" 'evil-switch-to-windows-last-buffer
    "c i" 'je/open-init-config
    "c f" 'je/open-functions-config
    "c v" 'je/open-variables-config
    "c ." 'je/open-custom-config
    "c l" 'je/open-language-config
    "c g" 'je/open-global-config
    "c k" 'je/open-evil-config
    "c p" 'je/open-packages-config
    "c c" 'je/load-config
    "f f" 'find-file
    "f a" 'find-alternate-file
    "f c" 'je/cleanup-file
    "f r" 'counsel-recentf
    "f s" 'save-buffer
    "f w" 'write-file
    "h a" 'about-emacs
    "h i" 'info
    "h h" 'help-for-help
    "h I" 'info-other-window
    "h n" 'view-emacs-news
    "h d b" 'describe-bindings
    "h d d" 'describe-distribution
    "h d f" 'describe-function
    "h d k" 'describe-key
    "h d K" 'finder-by-keyword
    "h d m" 'describe-mode
    "h d v" 'describe-variable
    "h d s" 'describe-symbol
    "h d t" 'describe-syntax
    "h d p" 'describe-package
    "h d c" 'describe-coding-system
    "h l" 'view-lossage
    "i f" 'insert-file
    "i h" 'auto-insert
    "l s" 'org-store-link
    "l l" 'browse-url-at-point
    "n r" 'narrow-to-region
    "n f" 'narrow-to-defun
    "n p" 'narrow-to-page
    "n w" 'widen
    "o a" 'org-agenda
    "o o" 'je/open-org-dir
    "o d" 'je/open-downloads-dir
    "o c" 'je/open-code-dir
    "o h" 'je/open-home-dir
    "o s" 'je/switch-to-scratch-buffer
    "o m" 'je/switch-to-messages-buffer
    "o ESC" 'je/switch-to-dashboard-buffer
    "p s" 'je/projectile-vc
    "p e" 'projectile-run-eshell
    "p p" 'counsel-projectile-switch-project
    "p r" 'je/projectile-root-dir
    "p f" 'je/counsel-projectile-find-file-region
    "p d" 'je/counsel-projectile-find-dir-region
    "p g" 'je/counsel-git-grep-region
    "s s" 'je/swiper-region-thing
    "s c" 'avy-goto-char
    "s l" 'avy-goto-line
    "s j" 'avy-goto-word-0
    "s r" 'ivy-resume
    "t t" 'toggle-truncate-lines
    "t p" 'smartparens-mode
    "t h" 'highlight-symbol-at-point
    "t H" 'je/unhighlight-all
    "u" 'universal-argument
    "U" 'negative-argument
    "w w" 'evil-window-next
    "w W" 'evil-window-prev
    "w n" 'evil-window-next
    "w p" 'evil-window-prev
    "w h" 'evil-window-left
    "w j" 'evil-window-down
    "w k" 'evil-window-up
    "w l" 'evil-window-right
    "w v" 'evil-window-vsplit
    "w s" 'evil-window-split
    "w c" 'evil-window-delete
    "w o" 'delete-other-windows
    "w f" 'reposition-window
    "w m" 'je/switch-to-minibuffer
    "w RET" 'toggle-frame-fullscreen
    "]" 'je/enlarge-window-horizontally
    "[" 'je/shrink-window-horizontally
    "=" 'je/enlarge-window
    "-" 'je/shrink-window
    ";" 'counsel-M-x
    ":" 'eval-expression
    "`" 'eshell
    "!" 'shell-command
    "@" 'async-shell-command
    "q q" 'save-buffers-kill-terminal)
  (global-evil-leader-mode 1))

(use-package evil-numbers
  :defer t)

(use-package evil-org
  :after evil
  :commands evil-org-mode
  :diminish evil-org-mode
  :hook
  ((org-mode . evil-org-mode)
   (evil-org-mode . evil-org-set-key-theme)))

(use-package evil-magit
  :after evil evil-leader
  :after magit
  :config
  (evil-define-key
    evil-magit-state
    magit-mode-map [escape] 'nil))

(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1))

(use-package evil-surround
  :after evil
  :diminish evil-surround-mode
  :config
  (global-evil-surround-mode 1))

(use-package evil-visualstar
  :after evil
  :config
  (global-evil-visualstar-mode 1))

(use-package expand-region
  :commands er/expand-region)

(use-package exec-path-from-shell
  :commands exec-path-from-shell-initialize)

(use-package fireplace
  :commands fireplace)

(use-package fix-word
  :commands
  fix-word-upcase
  fix-word-downcase
  fix-word-capitalize)

(use-package flycheck
  :commands flycheck-mode
  :after evil-leader
  :diminish flycheck-mode "flyc"
  :init
  (setq
   flycheck-indication-mode nil
   flycheck-highlighting-mode 'lines
   flycheck-emacs-lisp-load-path 'inherit)
  (custom-set-faces
   '(flycheck-error ((t (:foreground "red" :underline nil))))
   '(flycheck-info ((t (:foreground "green" :underline nil))))
   '(flycheck-warning ((t (:foreground "yellow" :underline nil)))))
  (evil-leader/set-key
    "t c" 'flycheck-mode))

(use-package flyspell
  :diminish flyspell-mode "spl"
  :commands flyspell-mode
  :init
  (custom-set-faces
   '(flyspell-duplicate ((t (:underline "Green"))))
   '(flyspell-incorrect ((t (:underline "Magenta")))))
  (setq flyspell-issue-message-flag nil))

(use-package focus
  :commands focus-mode
  :after evil-leader
  :init
  (evil-leader/set-key
    "t f" 'focus-mode))

(use-package grep
  :after which-key evil-leader
  :hook (grep-mode . je/enable-truncate-lines-no-message)
  :init
  (setq
   grep-command "grep -R . --exclude-dir={.git,.svn} --color -n -e "
   grep-use-null-device nil)
  (evil-leader/set-key "g" 'je/run-grep)
  (which-key-add-key-based-replacements "SPC g" "grep"))

(use-package hi-lock
  :commands hi-lock-mode
  :diminish hi-lock-mode "hi"
  :init
  (defface hi-magenta
    '((((background dark)) (:background "magenta" :foreground "black"))
      (t (:background "magenta")))
    "Custom magenta face for hi-lock mode."
    :group 'hi-lock-faces)
  (setq hi-lock-face-defaults
        '("hi-yellow"
          "hi-pink"
          "hi-green"
          "hi-blue"
          "hi-magenta")))

(use-package hydra
  :defer t)

(use-package ispell
  :commands ispell
  :after evil-leader
  :when (executable-find "ispell")
  :init
  (when (executable-find "aspell")
    (setq
     ispell-program-name "aspell"
     ispell-extra-args '("--sug-mode=ultra")))
  :config
  (evil-leader/set-key
    "t s" 'je/toggle-spelling))

(use-package ivy
  :commands ivy-mode
  :hook (emacs-startup . ivy-mode)
  :diminish ivy-mode
  :init
  (setq
   ivy-use-selectable-prompt t
   ivy-re-builders-alist
   '((ivy-switch-buffer . ivy--regex-ignore-order)
     (counsel-M-x . ivy--regex-ignore-order)
     (counsel-find-file . ivy--regex-ignore-order)
     (counsel-git . ivy--regex-ignore-order)
     (counsel-git-grep . ivy--regex-ignore-order)
     (t . ivy--regex-plus))))

(use-package ivy-hydra
  :after (ivy hydra))

(use-package locate
  :defer t
  :after evil-leader counsel
  :when (executable-find "locate")
  :init (evil-leader/set-key "f l" 'counsel-locate))

(use-package lorem-ipsum
  :commands
  lorem-ipsum-insert-list
  lorem-ipsum-insert-sentences
  lorem-ipsum-insert-paragraphs
  :after evil-leader
  :init
  (evil-leader/set-key
    "i l s" 'lorem-ipsum-insert-sentences
    "i l p" 'Lorem-ipsum-insert-paragraphs))

(use-package magit
  :commands magit-status
  :when
  (and (not (eq system-type 'windows-nt))
       (executable-find "git"))
  :init
  (setq
   magit-push-always-verify nil
   magit-refresh-status-buffer nil
   magit-refresh-verbose t
   magit-section-visibility-indicator nil
   vc-handled-backends (delq 'Git vc-handled-backends))
  :config
  (with-eval-after-load 'info
    (info-initialize)
    (add-to-list
     'Info-directory-list
     "~/.emacs.d/packages/magit/Documentation/"))
  (magit-auto-revert-mode 0))

(use-package modus-vivendi-theme
  :init
  (setq modus-vivendi-theme-no-link-underline t))

(use-package multiple-cursors
  :defer t
  :hook
  ((multiple-cursors-mode-enabled . je/mc-evil-emacs-state)
   (multiple-cursors-mode-disabled . je/mc-evil-normal-state)))

(use-package org
  :commands
  org-mode
  org-store-link
  org-agenda
  org-iswitchb
  org-capture
  :after evil-leader
  :hook
  (org-mode . je/enable-truncate-lines-no-message)
  (org-indent-mode . je/org-indent-setup)
  :init
  (setq org-startup-indented t
        org-log-done t
        org-agenda-files (list "~/org/work.org"
                               "~/org/home.org"))
  :config
  (evil-leader/set-key-for-mode 'org-mode
    "l l" 'je/org-link-follow
    "l i" 'org-insert-link-global
    "m c" 'org-toggle-checkbox
    "m e" 'org-export-dispatch
    "n e" 'org-narrow-to-element
    "n b" 'org-narrow-to-block
    "n s" 'org-narrow-to-subtree)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
  (add-to-list 'org-file-apps '(directory . emacs)))

(use-package prog-mode
  :commands prog-mode
  :hook (prog-mode . je/enable-truncate-lines-no-message))

(use-package projectile
  :diminish projectile-mode
  :commands projectile-mode
  :hook (emacs-startup . projectile-mode)
  :init
  (setq projectile-switch-project-action 'projectile-dired)
  (projectile-add-known-project "~/emacs"))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :hook (prog-mode . rainbow-delimiters-mode)
  :init
  (custom-set-faces
   '(rainbow-delimiters-mismatched-face ((t (:overline nil))))
   '(rainbow-delimiters-unmatched-face ((t (:overline nil))))))

(use-package rainbow-mode
  :commands rainbow-mode
  :diminish "rnb")

(use-package recentf
  :defer t
  :init
  (setq
   recentf-max-menu-items 0
   recentf-max-saved-items 20
   recentf-filename-handlers '(file-truename)
   recentf-exclude
   (list "^/tmp/"
         "^/ssh:"
         "^/sudo:"
         "\\.?ido\\.last$"
         "\\.revive$"
         "/TAGS$"
         "^/var/folders/.+$"
         "^/usr/lib/.+$"
         "^.+gz$"
         "^#")))

(use-package restart-emacs
  :commands restart-emacs
  :after evil-leader
  :init
  (evil-leader/set-key
    "q r" 'restart-emacs))

(use-package save-place
  :defer 1
  :config
  (save-place-mode 1))

(use-package shell
  :commands shell
  :hook (shell-mode . je/shell-kill-buffer-on-exit-sentinel))

(use-package smartparens
  :commands smartparens-mode
  :diminish smartparens-mode
  :hook ((prog-mode . smartparens-mode)
         (evil-replace-state-entry . turn-off-smartparens-mode)
         (evil-replace-state-exit . turn-on-smartparens-mode))
  :config
  (require 'smartparens-config)
  (setq sp-autowrap-region nil
        sp-highlight-pair-overlay nil
        sp-cancel-autoskip-on-backward-movement nil
        sp-show-pair-delay 0
        sp-max-pair-length 3))

(use-package swiper
  :commands swiper)

(use-package sudo-edit
  :when
  (and (not (or (eq system-type 'windows-nt) (eq system-type 'cygwin)))
       (executable-find "sudo")))

(use-package suggest
  :commands suggest)

(use-package term
  :defer t
  :init
  (defadvice term-handle-exit
      (after term-kill-buffer-on-exit activate)
    (kill-buffer)))

(use-package tiny
  :commands tiny-expand
  :config (tiny-setup-default))

(use-package too-long-lines-mode
  :hook (prog-mode . too-long-lines-mode)
  :diminish too-long-lines-mode)

(use-package try
  :commands try)

(use-package undo-tree
  :defer t
  :diminish undo-tree-mode)

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'reverse
        uniquify-after-kill-buffer-p t
        uniquify-separator ":"
        uniquify-ignore-buffers-re "^\\*"))

(use-package winner
  :commands winner-mode
  :after evil-leader
  :hook (emacs-startup . winner-mode)
  :config
  (evil-leader/set-key
    "w u" 'winner-undo
    "w r" 'winner-redo))

(use-package which-key
  :diminish which-key-mode
  :commands which-key-mode
  :hook (emacs-startup . which-key-mode)
  :init
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 5)
  :config
  (which-key-setup-side-window-bottom)
  (which-key-add-key-based-replacements
    "SPC ;" "M-x"
    "SPC !" "shell command"
    "SPC @" "async shell command"
    "SPC b" "buffer"
    "SPC TAB" "last buffer"
    "SPC c" "config"
    "SPC f" "file"
    "SPC h" "help"
    "SPC h d" "describe"
    "SPC i" "insert"
    "SPC m" "mode"
    "SPC n" "narrow"
    "SPC o" "open"
    "SPC p" "project"
    "SPC s" "search"
    "SPC t" "toggle"
    "SPC u" "univ arg"
    "SPC U" "negt arg"
    "SPC w" "window"
    "SPC q" "quit"
    "SPC =" "^|v ++"
    "SPC -" "^|v --"
    "SPC ]" "<=> ++"
    "SPC [" "<=> --"))

(use-package whitespace
  :after evil-leader
  :commands whitespace-mode
  :init
  (setq whitespace-display-mappings
        '((tab-mark ?\t [?› ?\t])
          (newline-mark ?\n [?¬ ?\n])
          (space-mark ?\  [?·] [?.])))
  (evil-leader/set-key "t w" 'whitespace-mode)
  :config
  (setq whitespace-line-column 100))

(use-package wdired
  :commands wdired-mode
  :init
  (setq wdired-allow-to-change-permissions t
        wdired-allow-to-redirect-links t))

(use-package wgrep
  :commands
  wgrep-change-to-wgrep-mode)

(use-package yasnippet
  :diminish yas-minor-mode
  :commands yas-minor-mode yas-minor-mode-on
  :hook (prog-mode . yas-minor-mode-on)
  :init (evil-leader/set-key "i s" 'yas-insert-snippet)
  :config
  (require 'yasnippet)
  (define-key yas-minor-mode-map (kbd "C-i") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (yas-reload-all))

(use-package xref
  :bind
  (:map xref--button-map
        ("C-j" . xref-goto-xref))
  :init
  (setq xref-after-jump-hook '(recenter)
        xref-after-return-hook '(recenter)))

(provide 'packages)
;;; packages.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
