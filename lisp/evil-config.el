;;; evil-config.el --- evil config                          -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: gmacs, evil, packages, config

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

(global-evil-leader-mode 1)
(global-evil-matchit-mode 1)
(global-evil-surround-mode 1)
(global-evil-visualstar-mode 1)
(global-company-mode 1)
(dumb-jump-mode 1)
(evil-mode 1)
(evil-commentary-mode 1)
(evil-vimish-fold-mode 1)
(evil-exchange-install)
(evil-escape-mode 1)
(golden-ratio-mode 1)

;; evil binds
(define-key evil-normal-state-map (kbd "j")     'evil-next-visual-line)
(define-key evil-visual-state-map (kbd "j")     'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k")     'evil-previous-visual-line)
(define-key evil-visual-state-map (kbd "k")     'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "RET")   'newline-and-indent)
(define-key evil-insert-state-map (kbd "RET")   'newline-and-indent)
(define-key evil-visual-state-map (kbd "RET")   'newline-and-indent)
(define-key evil-normal-state-map (kbd "M-y")   'counsel-yank-pop)
(define-key evil-insert-state-map (kbd "M-y")   'counsel-yank-pop)
(define-key evil-insert-state-map (kbd "M-\\")  'evil-execute-in-emacs-state)
(define-key evil-normal-state-map (kbd "C-a")   'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-b")   'evil-scroll-page-up)
(define-key evil-insert-state-map (kbd "M-b")   'backward-word)
(define-key evil-normal-state-map (kbd "C-=")   'er/expand-region)
(define-key evil-visual-state-map (kbd "C-=")   'er/expand-region)
(define-key evil-insert-state-map (kbd "TAB")   'tab-to-tab-stop)
(define-key evil-normal-state-map (kbd "U")     'undo-tree-visualize)
(define-key evil-normal-state-map (kbd "M-/")   'yas-expand)
(define-key evil-insert-state-map (kbd "M-/")   'yas-expand)
(define-key evil-insert-state-map (kbd "C-.")   'company-complete)
(define-key evil-normal-state-map (kbd "C-;")   'comment-line)
(define-key evil-visual-state-map (kbd "C-;")   'evil-commentary-line)
(define-key evil-insert-state-map (kbd "C-;")   'evil-commentary-line)
(define-key evil-normal-state-map (kbd "-")     'dired-jump)

;; web-mode
(evil-define-key 'insert web-mode-map (kbd "C-c n") 'emmet-next-edit-point)
(evil-define-key 'insert web-mode-map (kbd "C-c N") 'emmet-prev-edit-point)

;; dired
(eval-after-load 'dired
  '(progn
     ;; use the standard dired bindings as a base
     (evil-make-overriding-map dired-mode-map 'normal t)
     (evil-define-key 'normal dired-mode-map
       "-" 'dired-jump
       "h" 'evil-backward-char
       "j" 'evil-next-line
       "k" 'evil-previous-line
       "l" 'evil-forward-char
       "C-j" 'dired-find-file
       "K" 'dired-do-kill-lines
       "n" 'evil-search-next
       "p" 'magit-status
       "N" 'evil-search-previous
       "r" 'revert-buffer
       "." 'dired-omit-mode)))

;; minibuffer
(setq evil-insert-state-message nil
      evil-visual-state-message nil
      evil-replace-state-message nil
      evil-normal-state-message nil
      evil-emacs-state-message nil
      evil-motion-state-message nil
      evil-insert-state-message nil
      evil-operator-state-message nil)

;; emacs binds
(define-key evil-emacs-state-map (kbd "C-k") 'kill-line)
(define-key evil-emacs-state-map (kbd "C-s") 'isearch-forward)
(define-key evil-emacs-state-map (kbd "C-w") 'kill-region)
(define-key evil-emacs-state-map (kbd "C-y") 'yank)
(define-key evil-emacs-state-map (kbd "M-a") 'backward-sentence)
(define-key evil-emacs-state-map (kbd "M-b") 'backward-word)
(define-key evil-emacs-state-map (kbd "M-w") 'kill-ring-save)
(define-key evil-emacs-state-map (kbd "M-v") 'scroll-down-command)
(define-key evil-emacs-state-map (kbd "M-m") 'back-to-indentation)
(define-key evil-emacs-state-map (kbd "M-k") 'kill-sentence)
(define-key evil-emacs-state-map (kbd "M-u") 'fix-word-upcase)
(define-key evil-emacs-state-map (kbd "M-l") 'fix-word-downcase)
(define-key evil-emacs-state-map (kbd "M-c") 'fix-word-capitalize)
(define-key evil-emacs-state-map (kbd "M-h") 'mark-paragraph)
(define-key evil-emacs-state-map (kbd "M-z") 'zap-to-char)
(define-key evil-emacs-state-map (kbd "M-.") 'xref-find-definitions)
(define-key evil-emacs-state-map (kbd "C-x C-u") 'upcase-region)
(define-key evil-emacs-state-map (kbd "C-x C-l") 'downcase-region)
(define-key evil-emacs-state-map (kbd "C-M-l") 'reposition-window)
(define-key evil-emacs-state-map (kbd "C-M-o") 'split-line)
(define-key evil-emacs-state-map (kbd "C-M-/") 'reposition-window)
(define-key evil-emacs-state-map (kbd "C-M-.") 'xref-find-apropos)

;; unbinds to clean up global bindspace
(global-unset-key (kbd "C-k"))
(global-unset-key (kbd "C-w"))
(global-unset-key (kbd "C-s"))
(global-unset-key (kbd "C-y"))
(global-unset-key (kbd "M-a"))
(global-unset-key (kbd "M-b"))
(global-unset-key (kbd "M-w"))
(global-unset-key (kbd "M-m"))
(global-unset-key (kbd "M-k"))
(global-unset-key (kbd "M-u"))
(global-unset-key (kbd "M-v"))
(global-unset-key (kbd "M-l"))
(global-unset-key (kbd "M-c"))
(global-unset-key (kbd "M-h"))
(global-unset-key (kbd "M-."))
(global-unset-key (kbd "M-z"))
(global-unset-key (kbd "C-x C-u"))
(global-unset-key (kbd "C-x C-l"))
(global-unset-key (kbd "C-M-l"))
(global-unset-key (kbd "C-M-o"))
(global-unset-key (kbd "C-M-/"))
(global-unset-key (kbd "C-M-."))

;; emacs mode for minibuffer
(add-hook
 'minibuffer-setup-hook
 '(lambda () (evil-emacs-state)))

(provide 'evil-config)
;;; evil-config.el ends here
