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
(setq use-package-always-ensure t)
(require 'use-package)

(use-package abbrev
  :ensure nil
  :diminish abbrev-mode "ab")

(use-package alpha)

(use-package autorevert
  :ensure nil
  :init
  (setq auto-revert-verbose nil)
  (add-hook
   'auto-revert-mode-hook
   (lambda () (diminish 'auto-revert-mode)))
  (global-auto-revert-mode 1))

(use-package avy)

(use-package aggressive-indent
  :diminish "agg"
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
        ("<tab>"    . nil)
        ("<return>" . nil)
        ("RET"      . nil)
        ("\e\e\e"   . nil)
        ("M-n"      . company-select-next-or-abort)
        ("M-p"      . company-select-previous-or-abort)
        ("M-i"      . company-complete-common-or-cycle)
        ("C-n"      . gmacs/company-cancel-complete-next)
        ("C-p"      . gmacs/company-cancel-complete-prev)
        ("M-g"      . company-abort)
        ("M-j"      . company-complete-selection)
        ("M-m"      . company-complete-selection)
        ("C-v"      . company-next-page)
        ("M-v"      . company-previous-page))
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
  :commands dired
  :ensure nil
  :init
  (setq dired-listing-switches "-lah")
  (put 'dired-find-alternate-file 'disabled nil)
  (add-hook 'dired-mode-hook #'gmacs/rename-dired-buffer)
  (add-hook 'dired-mode-hook #'auto-revert-mode)
  (add-hook 'dired-mode-hook #'gmacs/enable-truncate-lines-no-message))

(use-package dired-x
  :ensure nil
  :after dired
  :config
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

(use-package erc
  :commands erc
  :config
  (require 'erc-services nil t)
  (erc-services-mode 1)
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
  (add-hook 'eshell-mode-hook #'gmacs/enable-truncate-lines-no-message)
  :config
  (with-eval-after-load 'em-term
    (progn
      (push '("npm" "install") eshell-visual-subcommands)
      (push '("git" "diff" "log") eshell-visual-subcommands)
      (push 'eshell-truncate-buffer eshell-output-filter-functions)))
  (with-eval-after-load 'em-hist
    (setq eshell-hist-ignoredups t)))

(use-package evil
  :init
  (setq evil-move-cursor-back nil
        evil-insert-state-message nil
        evil-visual-state-message nil
        evil-replace-state-message nil
        evil-normal-state-message nil
        evil-emacs-state-message nil
        evil-motion-state-message nil
        evil-insert-state-message nil
        evil-operator-state-message nil)
  (setq evil--jumps-buffer-targets
        "\\(\\*\\(\\new\\|scratch\\)\\*\\|dired:.+\\)")
  (add-hook 'with-editor-mode-hook 'evil-insert-state)
  (let ((height (eval gmacs/evil-base-cursor-height)))
    (setq evil-normal-state-cursor `("#dd00dd" (hbar . ,(eval height))))
    (setq evil-insert-state-cursor `("#00e000" (hbar . ,(eval height))))
    (setq evil-visual-state-cursor `("#ff8800" (hbar . ,(eval height))))
    (setq evil-emacs-state-cursor `("#ff0000" (hbar . ,(eval height))))
    (setq evil-motion-state-cursor `("#0000ff" (hbar . ,(eval height))))
    (setq evil-replace-state-cursor `("#00bbbb" (hbar . ,(eval height))))
    (setq evil-operator-state-cursor `("#ff66ff" (hbar . ,(eval height)))))
  :config
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
  :commands git-gutter-mode
  :diminish git-gutter-mode "gg"
  :init
  (when (not (eq system-type 'windows-nt))
    (add-hook
     'prog-mode-hook
     (lambda () (git-gutter-mode 1))))
  :config
  (require 'git-gutter-fringe)
  (setq git-gutter:update-interval 1)
  (set-face-foreground 'git-gutter-fr:modified "darkorange")
  (set-face-foreground 'git-gutter-fr:added    "darkorange")
  (set-face-foreground 'git-gutter-fr:deleted  "darkorange")
  (fringe-helper-define 'git-gutter-fr:added nil
    "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX"
    "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX"
    "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX"
    "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX"
    "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX"
    "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX"
    "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX"
    "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX"
    "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX"
    "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX"
    "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX")
  (fringe-helper-define 'git-gutter-fr:deleted nil
    "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX"
    "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX"
    "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX"
    "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX"
    "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX"
    "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX"
    "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX"
    "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX"
    "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX"
    "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX"
    "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX")
  (fringe-helper-define 'git-gutter-fr:modified nil
    "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX"
    "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX"
    "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX"
    "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX"
    "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX"
    "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX"
    "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX"
    "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX"
    "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX"
    "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX"
    "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX" "XXXXXXXX"))

(use-package grep
  :init
  (setq grep-command "grep -rLZE '.{1500}' -R . | xargs -r0 grep --color -n -e "
        grep-use-null-device nil)
  (add-hook 'grep-mode-hook #'gmacs/enable-truncate-lines-no-message))

(use-package hi-lock
  :diminish hi-lock-mode "hi"
  :ensure nil
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
  :ensure ivy-hydra
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
  (add-hook 'multiple-cursors-mode-enabled-hook #'gmacs/mc-evil-switch-to-emacs-state)
  (add-hook 'multiple-cursors-mode-disabled-hook #'gmacs/mc-evil-back-to-previous-state))

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
  (add-hook 'org-indent-mode-hook (lambda () (diminish 'org-indent-mode "in")))
  :config
  (add-to-list 'org-file-apps '(directory . emacs))
  (set-face-attribute 'org-level-1 nil :height 1.0)
  (add-hook 'org-mode-hook #'gmacs/enable-truncate-lines-no-message))

(use-package projectile
  :diminish projectile-mode
  :init
  (setq projectile-switch-project-action 'projectile-dired)
  :config
  (add-hook 'emacs-startup-hook (lambda () (projectile-mode 1))))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package rainbow-mode
  :commands rainbow-mode
  :diminish "rnb")

(use-package rebecca-theme)

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

(use-package restclient
  :commands restclient-mode
  :init
  (add-to-list
   'auto-mode-alist
   '("\\.rest\\'" . restclient-mode)))

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

(if (eq system-type 'gnu/linux)
    (use-package sudo-edit))

(use-package suggest
  :commands suggest)

(use-package telephone-line
  :init
  (require 'telephone-line-config)
  (setq telephone-line-primary-left-separator 'telephone-line-flat
        telephone-line-secondary-left-separator 'telephone-line-flat)
  (defface telephone-line-elscreen
    '((t (:foreground "black" :background "grey70" :weight bold)))
    "Elscreen telephone-line segment theme.")
  (setq telephone-line-faces
        '((evil . telephone-line-modal-face)
          (modal . telephone-line-modal-face)
          (elscreen . (telephone-line-elscreen . mode-line-inactive))
          (ryo . telephone-line-ryo-modal-face)
          (accent . (telephone-line-accent-active . mode-line-inactive))
          (nil . (mode-line . mode-line-inactive))))
  (telephone-line-defsegment* telephone-line-empty-segment ()
    (telephone-line-raw "" t))
  (telephone-line-defsegment* telephone-line-elscreen-mode-line-string-segment ()
    (telephone-line-raw elscreen-mode-line-string t))
  (setq telephone-line-lhs '((elscreen . (telephone-line-elscreen-mode-line-string-segment))
                             (evil . (telephone-line-simple-major-mode-segment))
                             (accent . (telephone-line-simple-minor-mode-segment))
                             (nil . (telephone-line-buffer-segment))
                             (nil . (telephone-line-airline-position-segment))))
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

(use-package too-long-lines-mode
  :diminish too-long-lines-mode
  :ensure nil)

(use-package try
  :commands try)

(use-package undo-tree
  :ensure nil
  :diminish undo-tree-mode "ut")

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
  :ensure nil
  :init
  (setq whitespace-display-mappings
        '((tab-mark ?\t [?› ?\t])
          (newline-mark ?\n [?¬ ?\n])
          (space-mark ?\  [?·] [?.])))
  :config
  (setq whitespace-line-column 100))

(use-package wdired
  :ensure nil
  :init
  (setq wdired-allow-to-change-permissions t
        wdired-allow-to-redirect-linkst t))

(use-package wgrep)

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

(provide 'packages)
;;; packages.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
