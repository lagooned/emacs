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

(use-package evil-exchange
  :config
  (evil-exchange-install))

(use-package evil-numbers
  :commands
  evil-numbers/inc-at-point
  evil-numbers/dev-at-point
  :config
  (require 'evil-numbers))

(use-package evil-magit
  :after
  magit
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
  :init
  (evil-collection-minibuffer nil)
  :config
  (evil-collection-init))

;; evil binds
(define-key evil-normal-state-map (kbd "j")     'evil-next-visual-line)
(define-key evil-visual-state-map (kbd "j")     'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k")     'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "k")     'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "gg")    'beginning-of-buffer)
(define-key evil-visual-state-map (kbd "gg")    'beginning-of-buffer)
(define-key evil-normal-state-map (kbd "G")     'end-of-buffer)
(define-key evil-visual-state-map (kbd "G")     'end-of-buffer)
(define-key evil-visual-state-map (kbd "$")     'evil-end-of-visual-line)
(define-key evil-insert-state-map (kbd "M->")   'end-of-buffer)
(define-key evil-insert-state-map (kbd "M-<")   'beginning-of-buffer)
(define-key evil-normal-state-map (kbd "RET")   'newline-and-indent)
(define-key evil-insert-state-map (kbd "RET")   'newline-and-indent)
(define-key evil-visual-state-map (kbd "RET")   'void)
(define-key evil-visual-state-map (kbd "C-j")   'void)
(define-key evil-normal-state-map (kbd "M-y")   'counsel-yank-pop)
(define-key evil-insert-state-map (kbd "M-y")   'counsel-yank-pop)
(define-key evil-visual-state-map (kbd "M-N")   'evil-visualstar/begin-search-backward)
(define-key evil-insert-state-map (kbd "M-\\")  'evil-execute-in-emacs-state)
(define-key evil-normal-state-map (kbd "C-a")   'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-b")   'evil-scroll-page-up)
(define-key evil-normal-state-map (kbd "C-j")   'newline)
(define-key evil-insert-state-map (kbd "C-j")   'newline)
(define-key evil-normal-state-map (kbd "C-m")   'newline-and-indent)
(define-key evil-insert-state-map (kbd "C-m")   'newline-and-indent)
(define-key evil-normal-state-map (kbd "M-j")   'indent-new-comment-line)
(define-key evil-insert-state-map (kbd "M-j")   'indent-new-comment-line)
(define-key evil-normal-state-map (kbd "M-m")   'indent-new-comment-line)
(define-key evil-insert-state-map (kbd "M-m")   'indent-new-comment-line)
(define-key evil-insert-state-map (kbd "M-b")   'backward-word)
(define-key evil-normal-state-map (kbd "C-s")   'swiper)
(define-key evil-normal-state-map (kbd "C-=")   'er/expand-region)
(define-key evil-visual-state-map (kbd "C-=")   'er/expand-region)
(define-key evil-motion-state-map (kbd "C-=")   'er/expand-region)
(define-key evil-insert-state-map (kbd "TAB")   'tab-to-tab-stop)
(define-key evil-normal-state-map (kbd "U")     'undo-tree-visualize)
(define-key evil-normal-state-map (kbd "M-/")   'yas-expand)
(define-key evil-insert-state-map (kbd "M-/")   'yas-expand)
(define-key evil-insert-state-map (kbd "M-.")   'company-complete)
(define-key evil-normal-state-map (kbd "C-;")   'comment-line)
(define-key evil-visual-state-map (kbd "C-;")   'comment-dwim)
(define-key evil-insert-state-map (kbd "C-;")   'comment-dwim)
(define-key evil-normal-state-map (kbd "-")     'dired-jump)
(define-key evil-normal-state-map (kbd "C-u")   'evil-scroll-up)
(define-key evil-motion-state-map (kbd "C-u")   'evil-scroll-up)
(define-key evil-normal-state-map (kbd "C-]")   'gmacs/xref-find-definitions-symbol)

;; ex mode setup
(define-key evil-ex-completion-map (kbd "C-b") 'backward-char)
(define-key evil-ex-completion-map (kbd "C-d") 'delete-char)
(define-key evil-ex-completion-map (kbd "C-a") 'move-beginning-of-line)
(define-key evil-ex-completion-map (kbd "M-p") 'previous-complete-history-element)
(define-key evil-ex-completion-map (kbd "M-n") 'next-complete-history-element)

;; web-mode
(evil-define-key 'insert web-mode-map (kbd "C-c n") 'emmet-next-edit-point)
(evil-define-key 'insert web-mode-map (kbd "C-c p") 'emmet-prev-edit-point)

;; shell
(add-hook
 'shell-mode-hook
 (lambda ()
   (progn
     (define-key evil-normal-state-local-map (kbd "C-d") 'evil-scroll-down)
     (define-key evil-insert-state-local-map (kbd "C-d") 'comint-send-eof)
     (define-key evil-normal-state-local-map (kbd "RET") 'comint-send-input)
     (define-key evil-normal-state-local-map (kbd "C-j") 'comint-send-input)
     (define-key evil-normal-state-local-map (kbd "C-m") 'comint-send-input)
     (define-key evil-insert-state-local-map (kbd "RET") 'comint-send-input)
     (define-key evil-insert-state-local-map (kbd "C-j") 'comint-send-input)
     (define-key evil-insert-state-local-map (kbd "C-m") 'comint-send-input))))

;; term
(add-hook
 'term-mode-hook
 (lambda ()
   (progn
     (define-key evil-insert-state-local-map (kbd "TAB") 'term-send-raw)
     (define-key evil-insert-state-local-map (kbd "C-d") 'term-send-eof)
     (define-key evil-normal-state-local-map (kbd "C-c C-d") 'term-send-eof)
     (define-key evil-normal-state-local-map (kbd "M-r") 'void)
     (define-key evil-insert-state-local-map (kbd "M-r") 'term-send-reverse-search-history)
     (define-key evil-normal-state-local-map (kbd "C-c C-t") 'multi-term)
     (define-key evil-insert-state-local-map (kbd "C-c C-t") 'multi-term)
     (define-key evil-insert-state-local-map (kbd "C-c C-p") 'multi-term-prev)
     (define-key evil-normal-state-local-map (kbd "C-c C-p") 'multi-term-prev)
     (define-key evil-insert-state-local-map (kbd "C-c C-n") 'multi-term-next)
     (define-key evil-normal-state-local-map (kbd "C-c C-n") 'multi-term-next)
     (define-key evil-normal-state-local-map (kbd "RET") 'term-send-input)
     (define-key evil-normal-state-local-map (kbd "C-j") 'term-send-input)
     (define-key evil-normal-state-local-map (kbd "C-m") 'term-send-input)
     (define-key evil-insert-state-local-map (kbd "RET") 'term-send-input)
     (define-key evil-insert-state-local-map (kbd "C-j") 'term-send-input)
     (define-key evil-insert-state-local-map (kbd "C-m") 'term-send-input))))

;; org
(add-hook
 'org-mode-hook
 (lambda ()
   (progn
     (define-key
       evil-normal-state-local-map (kbd "M-i") 'org-cycle))))

;; emacs mode binds
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

;; minibuffer
(add-hook 'minibuffer-setup-hook 'gmacs/evil-minibuffer-setup)

;; eshell
(add-hook 'eshell-mode-hook 'gmacs/evil-eshell-mode-setup)

;; c derivatives comments
(add-hook 'c-mode-common-hook 'gmacs/evil-c-common-mode-setup)

;; company abort on exit insert mode
(add-hook 'evil-insert-state-exit-hook
          (lambda () (if (bound-and-true-p company-mode)
                    (company-abort))))

;; add evil-ex-history to .savehist file
(if (not (member 'evil-ex-history savehist-additional-variables))
    (push 'evil-ex-history savehist-additional-variables))

(provide 'evil-config)

;;; evil-config.el ends here
