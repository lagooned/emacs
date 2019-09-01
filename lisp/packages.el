;;; packages.el --- package configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: gmacs, config, packages

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
(require 'use-package)

(use-package abbrev
  :diminish abbrev-mode "ab")

(use-package alpha)

(use-package autorevert
  ;; :diminish doesn't work as mode and lib name are different
  :hook (auto-revert-mode . gmacs/auto-revert-mode-setup)
  :init
  (setq auto-revert-verbose nil))

(use-package avy)

(use-package aggressive-indent
  :diminish "agg"
  :commands aggressive-indent-mode)

(use-package badwolf-theme)

(use-package company
  :diminish company-mode
  :commands company-mode
  :bind
  (:map company-active-map
        ("<tab>" . nil)
        ("<return>" . nil)
        ("RET" . nil)
        ("\e\e\e" . nil)
        ("M-n" . company-select-next-or-abort)
        ("M-p" . company-select-previous-or-abort)
        ("M-i" . company-complete-common-or-cycle)
        ("C-n" . gmacs/company-cancel-complete-next)
        ("C-p" . gmacs/company-cancel-complete-prev)
        ("M-g" . company-abort)
        ("M-j" . company-complete-selection)
        ("M-m" . company-complete-selection)
        ("C-v" . company-next-page)
        ("M-v" . company-previous-page))
  :init
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-show-numbers t)
  :config
  (setq company-backends nil))

(use-package counsel
  :diminish counsel-mode
  :config
  (counsel-mode 1))

(use-package counsel-projectile
  :after counsel
  :config
  (setq counsel-projectile-switch-project-action
        '(1 ("o" counsel-projectile-switch-project-action-dired "open project in dired")
            ("f" counsel-projectile-switch-project-action-find-file "jump to a project file")
            ("d" counsel-projectile-switch-project-action-find-dir "jump to a project directory")
            ("b" counsel-projectile-switch-project-action-switch-to-buffer "jump to a project buffer")
            ("m" counsel-projectile-switch-project-action-find-file-manually "find file manually from project root")
            ("S" counsel-projectile-switch-project-action-save-all-buffers "save all project buffers")
            ("k" counsel-projectile-switch-project-action-kill-buffers "kill all project buffers")
            ("K" counsel-projectile-switch-project-action-remove-known-project "remove project from known projects")
            ("c" counsel-projectile-switch-project-action-compile "run project compilation command")
            ("C" counsel-projectile-switch-project-action-configure "run project configure command")
            ("E" counsel-projectile-switch-project-action-edit-dir-locals "edit project dir-locals")
            ("v" counsel-projectile-switch-project-action-vc "open project in vc-dir / magit / monky")
            ("sg" counsel-projectile-switch-project-action-grep "search project with grep")
            ("si" counsel-projectile-switch-project-action-git-grep "search project with git grep")
            ("ss" counsel-projectile-switch-project-action-ag "search project with ag")
            ("sr" counsel-projectile-switch-project-action-rg "search project with rg")
            ("xs" counsel-projectile-switch-project-action-run-shell "invoke shell from project root")
            ("xe" counsel-projectile-switch-project-action-run-eshell "invoke eshell from project root")
            ("xt" counsel-projectile-switch-project-action-run-term "invoke term from project root")
            ("Oc" counsel-projectile-switch-project-action-org-capture "capture into project")
            ("Oa" counsel-projectile-switch-project-action-org-agenda "open project agenda"))))

(use-package dired
  :hook
  ((dired-mode . gmacs/rename-dired-buffer)
   (dired-mode . auto-revert-mode)
   (dired-mode . gmacs/enable-truncate-lines-no-message)
   (dired-mode . dired-hide-details-mode))
  :init
  (setq dired-listing-switches "-lah")
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dired-x
  :after dired
  :config
  (setq-default dired-omit-mode 1)
  (setq dired-omit-files
        (concat dired-omit-files
                "\\|^\\..+$\\|^~.+$\\|^#.+$")))

(use-package doom-themes
  :init
  (setq doom-themes-enable-bold nil))

(use-package dracula-theme)

(use-package ediff
  :init
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

(use-package eshell
  :hook
  (eshell-mode . gmacs/enable-truncate-lines-no-message)
  :init
  (setq eshell-banner-message 'gmacs/eshell-message
        eshell-prompt-function 'gmacs/eshell-prompt-function
        eshell-prompt-regexp (eval 'gmacs/eshell-prompt-regexp))
  :config
  (with-eval-after-load 'em-term
    (push 'eshell-truncate-buffer eshell-output-filter-functions))
  (with-eval-after-load 'em-hist
    (setq eshell-hist-ignoredups t)))

(use-package evil
  :hook
  ((with-editor-mode . evil-insert-state)
   (evil-command-window-mode . gmacs/evil-command-window-setup)
   (eshell-mode . gmacs/evil-eshell-mode-setup)
   (minibuffer-setup . gmacs/evil-minibuffer-setup)
   (org-mode . gmacs/evil-org-mode-setup)
   (c-mode-common . gmacs/evil-c-common-mode-setup)
   (evil-insert-state-exit . gmacs/evil-company-abort-on-insert-leave)
   (evil-mode . gmacs/evil-jumplist-setup))
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
   evil-jumps-max-length 20)
  :config
  (load "evil-pre-config")
  (evil-mode 1)
  (load "evil-config"))

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
  :diminish flycheck-mode "flyc"
  :init
  (setq
   flycheck-indication-mode nil
   flycheck-highlighting-mode 'lines
   flycheck-emacs-lisp-load-path 'inherit)
  (custom-set-faces
   '(flycheck-error ((t (:foreground "red" :underline nil))))
   '(flycheck-info ((t (:foreground "green" :underline nil))))
   '(flycheck-warning ((t (:foreground "yellow" :underline nil))))))

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

(use-package grep
  :init
  (setq grep-command "grep -R . --exclude-dir={.git,.svn} --color -n -e "
        grep-use-null-device nil)
  (add-hook 'grep-mode-hook #'gmacs/enable-truncate-lines-no-message))

(use-package hi-lock
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

(use-package ivy
  :diminish ivy-mode
  :bind
  (:map ivy-minibuffer-map
        ("M--" . counsel-up-directory))
  :init
  (setq
   ivy-re-builders-alist
   '((ivy-switch-buffer . ivy--regex-ignore-order)
     (counsel-M-x . ivy--regex-ignore-order)
     (counsel-find-file . ivy--regex-ignore-order)
     (counsel-git . ivy--regex-ignore-order)
     (counsel-git-grep . ivy--regex-ignore-order)
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

(use-package lorem-ipsum
  :commands
  lorem-ipsum-insert-list
  lorem-ipsum-insert-sentences
  lorem-ipsum-insert-paragraphs)

(use-package magit
  :commands magit-status
  :init
  (setq magit-push-always-verify nil
        magit-refresh-status-buffer nil
        magit-refresh-verbose t
        magit-section-visibility-indicator nil)
  (setq vc-handled-backends (delq 'Git vc-handled-backends))
  :config
  (with-eval-after-load 'info
    (info-initialize)
    (add-to-list
     'Info-directory-list
     "~/.emacs.d/packages/magit/Documentation/"))
  (magit-auto-revert-mode 0))

(use-package multiple-cursors
  :hook
  ((multiple-cursors-mode-enabled . gmacs/mc-evil-emacs-state)
   (multiple-cursors-mode-disabled . gmacs/mc-evil-normal-state)))

(use-package org
  :commands
  org-mode
  org-store-link
  org-agenda
  org-iswitchb
  org-capture
  :hook
  (org-mode . gmacs/enable-truncate-lines-no-message)
  :init
  (setq org-startup-indented t
        org-log-done t
        org-agenda-files (list "~/org/work.org"
                               "~/org/home.org"))
  (add-hook 'org-indent-mode-hook (lambda () (diminish 'org-indent-mode "in")))
  :config
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
  (add-to-list 'org-file-apps '(directory . emacs))
  (require 'org-tempo))

(use-package prog-mode
  :commands prog-mode
  :hook ((prog-mode . too-long-lines-mode)
         (prog-mode . gmacs/enable-truncate-lines-no-message)
         (prog-mode . gmacs/enable-company-mode)))

(use-package projectile
  :diminish projectile-mode
  :init
  (setq projectile-switch-project-action 'projectile-dired)
  :config
  (add-hook 'emacs-startup-hook (lambda () (projectile-mode 1))))

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

(use-package rebecca-theme)

(use-package recentf
  :defer t
  :commands counsel-recentf
  :config
  (setq recentf-max-menu-items 0
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
              (concat "^" (expand-file-name "~/\\(.emacs.d\\|emacs\\)/workspace") "/.+$")
              "^#")))

(use-package restart-emacs
  :commands restart-emacs)

(use-package shell
  :commands shell
  :init
  (add-hook 'shell-mode-hook 'gmacs/shell-kill-buffer-on-exit-sentinel))

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

(use-package spacemacs-theme
  :init
  (setq spacemacs-theme-comment-bg nil
        spacemacs-theme-underline-parens nil))

(use-package swiper
  :commands swiper)

(if (and (eq system-type 'gnu/linux) (executable-find "sudo"))
    (use-package sudo-edit))

(use-package suggest
  :commands suggest)

(use-package term
  :init
  (defadvice term-handle-exit
      (after term-kill-buffer-on-exit activate)
    (kill-buffer)))

(use-package tiny
  :config
  (tiny-setup-default))

(use-package too-long-lines-mode
  :diminish too-long-lines-mode)

(use-package try
  :commands try)

(use-package undo-tree
  :diminish undo-tree-mode "ut")

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'reverse
        uniquify-after-kill-buffer-p t
        uniquify-separator ":"
        uniquify-ignore-buffers-re "^\\*"))

(use-package winner
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
    "SPC !" "shell command"
    "SPC @" "async shell command"
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
  :init
  (setq whitespace-display-mappings
        '((tab-mark ?\t [?› ?\t])
          (newline-mark ?\n [?¬ ?\n])
          (space-mark ?\  [?·] [?.])))
  :config
  (setq whitespace-line-column 100))

(use-package wdired
  :init
  (setq wdired-allow-to-change-permissions t
        wdired-allow-to-redirect-links t))

(use-package wgrep)

(use-package yasnippet
  :commands
  yas-minor-mode
  yas-minor-mode-on
  :hook (prog-mode . yas-minor-mode)
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
