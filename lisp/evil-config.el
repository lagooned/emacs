;;; evil-config.el --- evil config  -*- lexical-binding: t; -*-

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

;;; Commentary:

;; Gmacs evil-mode configuration.

;;; Code:

(use-package evil-leader
  :config
  (load "leader-config"))

(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1))

(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode 1))

(evil-mode 1)

(use-package exato)

(use-package evil-numbers
  :commands
  evil-numbers/inc-at-point
  evil-numbers/dec-at-point
  :config
  (require 'evil-numbers))

(use-package evil-magit
  :after magit
  :config
  (require 'evil-magit)
  (evil-define-key
    evil-magit-state
    magit-mode-map [escape] 'nil))

(use-package evil-org
  :diminish evil-org-mode
  :after org
  :init
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook 'evil-org-set-key-theme))

(use-package evil-surround
  :diminish evil-surround-mode
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :config
  (evil-collection-init))

;; cursor configs
(let ((height (eval gmacs/evil-base-cursor-height))
      (pipe-width (/ (frame-char-width) 5)))
  (setq evil-normal-state-cursor `("#00ffff" (hbar . ,(eval height))))
  (setq evil-insert-state-cursor `("#00e000" (bar . ,(eval pipe-width))))
  (setq evil-visual-state-cursor `("#ff8800" (hbar . ,(eval height))))
  (setq evil-emacs-state-cursor `("#ff0000" (hbar . ,(eval height))))
  (setq evil-motion-state-cursor `("#0055ff" (hbar . ,(eval height))))
  (setq evil-replace-state-cursor `("#00acff" (hbar . ,(eval height))))
  (setq evil-operator-state-cursor `("#ff66ff" (hbar . ,(eval height)))))

;; insert state
(define-key evil-insert-state-map (kbd "M-;") 'comment-dwim)
(define-key evil-insert-state-map (kbd "C-j") 'newline)
(define-key evil-insert-state-map (kbd "C-m") 'newline-and-indent)
(define-key evil-insert-state-map (kbd "M-.") 'company-complete)
(define-key evil-insert-state-map (kbd "M-/") 'yas-expand)
(define-key evil-insert-state-map (kbd "M-<") 'beginning-of-buffer)
(define-key evil-insert-state-map (kbd "M->") 'end-of-buffer)
(define-key evil-insert-state-map (kbd "M-\\") 'evil-execute-in-emacs-state)
(define-key evil-insert-state-map (kbd "M-b") 'backward-word)
(define-key evil-insert-state-map (kbd "M-j") 'indent-new-comment-line)
(define-key evil-insert-state-map (kbd "M-m") 'indent-new-comment-line)
(define-key evil-insert-state-map (kbd "M-y") 'counsel-yank-pop)
(define-key evil-insert-state-map (kbd "RET") 'newline-and-indent)
(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)

;; motion state
(define-key evil-motion-state-map (kbd "C-=") 'er/expand-region)
(define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up)

;; normal state
(define-key evil-normal-state-map (kbd "-") 'dired-jump)
(define-key evil-normal-state-map (kbd "M-;") 'comment-line)
(define-key evil-normal-state-map (kbd "C-=") 'er/expand-region)
(define-key evil-normal-state-map (kbd "C-]") 'gmacs/xref-find-definitions-symbol)
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-b") 'evil-scroll-page-up)
(define-key evil-normal-state-map (kbd "C-j") 'newline)
(define-key evil-normal-state-map (kbd "C-m") 'newline-and-indent)
(define-key evil-normal-state-map (kbd "C-s") 'swiper)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-normal-state-map (kbd "G") 'end-of-buffer)
(define-key evil-normal-state-map (kbd "M-/") 'yas-expand)
(define-key evil-normal-state-map (kbd "M-j") 'indent-new-comment-line)
(define-key evil-normal-state-map (kbd "M-m") 'indent-new-comment-line)
(define-key evil-normal-state-map (kbd "M-y") 'counsel-yank-pop)
(define-key evil-normal-state-map (kbd "RET") 'newline-and-indent)
(define-key evil-normal-state-map (kbd "U") 'undo-tree-visualize)
(define-key evil-normal-state-map (kbd "gg") 'beginning-of-buffer)
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "gr") 'revert-buffer)
(define-key evil-normal-state-map (kbd "M-t") 'transpose-words)
(define-key evil-normal-state-map (kbd "M-:") 'evil-command-window-ex)

;; visual mode
(define-key evil-visual-state-map (kbd "$") 'evil-end-of-visual-line)
(define-key evil-visual-state-map (kbd "C-;") 'comment-dwim)
(define-key evil-visual-state-map (kbd "C-=") 'er/expand-region)
(define-key evil-visual-state-map (kbd "C-j") 'void)
(define-key evil-visual-state-map (kbd "G") 'end-of-buffer)
(define-key evil-visual-state-map (kbd "M-n") 'evil-visualstar/begin-search-forward)
(define-key evil-visual-state-map (kbd "M-N") 'evil-visualstar/begin-search-backward)
(define-key evil-visual-state-map (kbd "RET") 'void)
(define-key evil-visual-state-map (kbd "gg") 'beginning-of-buffer)
(define-key evil-visual-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-visual-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-visual-state-map (kbd "M-j") 'void)
(define-key evil-visual-state-map (kbd "M-m") 'void)

;; emacs mode
(define-key evil-emacs-state-map (kbd "C-k") 'kill-line)
(define-key evil-emacs-state-map (kbd "C-s") 'isearch-forward)
(define-key evil-emacs-state-map (kbd "C-w") 'kill-region)
(define-key evil-emacs-state-map (kbd "C-y") 'yank)
(define-key evil-emacs-state-map (kbd "M-r") 'move-to-window-line-top-bottom)
(define-key evil-emacs-state-map (kbd "M-a") 'backward-sentence)
(define-key evil-emacs-state-map (kbd "M-b") 'backward-word)
(define-key evil-emacs-state-map (kbd "M-e") 'forward-sentence)
(define-key evil-emacs-state-map (kbd "M-w") 'kill-ring-save)
(define-key evil-emacs-state-map (kbd "M-d") 'kill-word)
(define-key evil-emacs-state-map (kbd "M-v") 'scroll-down-command)
(define-key evil-emacs-state-map (kbd "M-m") 'back-to-indentation)
(define-key evil-emacs-state-map (kbd "M-j") 'back-to-indentation)
(define-key evil-emacs-state-map (kbd "M-k") 'kill-sentence)
(define-key evil-emacs-state-map (kbd "M-u") 'fix-word-upcase)
(define-key evil-emacs-state-map (kbd "M-l") 'fix-word-downcase)
(define-key evil-emacs-state-map (kbd "M-c") 'fix-word-capitalize)
(define-key evil-emacs-state-map (kbd "M-z") 'zop-to-char)
(define-key evil-emacs-state-map (kbd "M-.") 'xref-find-definitions)
(define-key evil-emacs-state-map (kbd "M-&") 'async-shell-command)
(define-key evil-emacs-state-map (kbd "M-^") 'delete-indentation)
(define-key evil-emacs-state-map (kbd "M-,") 'xref-pop-marker-stack)
(define-key evil-emacs-state-map (kbd "M-?") 'xref-find-references)
(define-key evil-emacs-state-map (kbd "M-@") 'mark-word)
(define-key evil-emacs-state-map (kbd "M-!") 'shell-command)
(define-key evil-emacs-state-map (kbd "M-<") 'beginning-of-buffer)
(define-key evil-emacs-state-map (kbd "M->") 'end-of-buffer)
(define-key evil-emacs-state-map (kbd "M-o b") 'facemenu-set-bold)
(define-key evil-emacs-state-map (kbd "M-o d") 'facemenu-set-default)
(define-key evil-emacs-state-map (kbd "M-o i") 'facemenu-set-italic)
(define-key evil-emacs-state-map (kbd "M-o l") 'facemenu-set-bold-italic)
(define-key evil-emacs-state-map (kbd "M-o o") 'facemenu-set-face)
(define-key evil-emacs-state-map (kbd "M-o u") 'facemenu-set-underline)
(define-key evil-emacs-state-map (kbd "M-o M-o") 'font-lock-fontify-block)
(define-key evil-emacs-state-map (kbd "M-o M-s") 'center-line)
(define-key evil-emacs-state-map (kbd "M-o M-S") 'center-paragraph)
(define-key evil-emacs-state-map (kbd "C-x C-u") 'upcase-region)
(define-key evil-emacs-state-map (kbd "C-x C-l") 'downcase-region)
(define-key evil-emacs-state-map (kbd "C-M-h") 'mark-defun)
(define-key evil-emacs-state-map (kbd "C-M-d") 'down-list)
(define-key evil-emacs-state-map (kbd "C-M-u") 'backward-up-list)
(define-key evil-emacs-state-map (kbd "C-M-n") 'forward-list)
(define-key evil-emacs-state-map (kbd "C-M-p") 'backward-list)
(define-key evil-emacs-state-map (kbd "C-M-f") 'forward-sexp)
(define-key evil-emacs-state-map (kbd "C-M-b") 'backward-sexp)
(define-key evil-emacs-state-map (kbd "C-M-k") 'kill-sexp)
(define-key evil-emacs-state-map (kbd "C-M-v") 'scroll-other-window)
(define-key evil-emacs-state-map (kbd "C-M-e") 'end-of-defun)
(define-key evil-emacs-state-map (kbd "C-M-a") 'beginning-of-defun)
(define-key evil-emacs-state-map (kbd "C-M-t") 'transpose-sexps)
(define-key evil-emacs-state-map (kbd "C-M-l") 'reposition-window)
(define-key evil-emacs-state-map (kbd "C-=") 'er/expand-region)
(define-key evil-emacs-state-map (kbd "M-;") 'comment-dwim)
(define-key evil-emacs-state-map (kbd "M-i") 'tab-to-tab-stop)

;; ex mode
(define-key evil-ex-completion-map (kbd "C-b") 'backward-char)
(define-key evil-ex-completion-map (kbd "C-d") 'delete-char)
(define-key evil-ex-completion-map (kbd "C-a") 'move-beginning-of-line)
(define-key evil-ex-completion-map (kbd "M-p") 'previous-complete-history-element)
(define-key evil-ex-completion-map (kbd "M-n") 'next-complete-history-element)

;; company abort on exit insert mode
(add-hook 'evil-insert-state-exit-hook 'gmacs/evil-company-abort-on-insert-leave)

;; add evil-ex-history to .savehist file
(put 'evil-ex-history 'history-length 50)

;; add dired buffers to jumplist
(evil-add-command-properties #'dired-find-file :jump t)

(provide 'evil-config)

;;; evil-config.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
