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
  (load "leader-config.el"))

(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1))

(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode 1))

(dumb-jump-mode 1)
(evil-mode 1)

(use-package evil-vimish-fold
  :diminish evil-vimish-fold-mode
  :config
  (evil-vimish-fold-mode 1))

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
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme))))

(use-package evil-surround
  :diminish evil-surround-mode
  :config
  (global-evil-surround-mode 1))

(zoom-mode 1)

;; evil binds
(define-key evil-normal-state-map (kbd "j")     'evil-next-visual-line)
(define-key evil-visual-state-map (kbd "j")     'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k")     'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "k")     'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "gg")    'beginning-of-buffer)
(define-key evil-visual-state-map (kbd "gg")    'beginning-of-buffer)
(define-key evil-normal-state-map (kbd "G")     'end-of-buffer)
(define-key evil-visual-state-map (kbd "G")     'end-of-buffer)
(define-key evil-insert-state-map (kbd "M->")   'end-of-buffer)
(define-key evil-insert-state-map (kbd "M-<")   'beginning-of-buffer)
(define-key evil-normal-state-map (kbd "RET")   'newline-and-indent)
(define-key evil-insert-state-map (kbd "RET")   'newline-and-indent)
(define-key evil-visual-state-map (kbd "RET")   'nil)
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
(define-key evil-normal-state-map (kbd "C-`")   'shell-pop)
(define-key evil-insert-state-map (kbd "C-`")   'shell-pop)

;; web-mode
(evil-define-key 'insert web-mode-map (kbd "C-c n") 'emmet-next-edit-point)
(evil-define-key 'insert web-mode-map (kbd "C-c p") 'emmet-prev-edit-point)

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
       "gg" 'beginning-of-buffer
       "G" 'end-of-buffer
       "p" 'magit-status
       "N" 'evil-search-previous
       "r" 'revert-buffer
       "$" 'evil-end-of-line
       "0" 'evil-beginning-of-line
       "b" 'evil-backward-word-begin
       "B" 'evil-backward-WORD-begin
       "w" 'evil-forward-word-begin
       "W" 'evil-forward-WORD-begin
       "v" 'evil-visual-char
       "V" 'evil-visual-line
       "C-v" 'evil-visual-block
       "c" 'dired-do-copy
       "?" 'evil-search-backward
       "." 'dired-omit-mode)))

;; doc-view
(evil-set-initial-state 'doc-view-mode 'normal)
(add-hook
 'doc-view-mode-hook
 (lambda ()
   (set (make-local-variable 'evil-normal-state-cursor) (list nil))
   (set (make-local-variable 'evil-insert-state-cursor) (list nil))
   (set (make-local-variable 'evil-visual-state-cursor) (list nil))
   (set (make-local-variable 'evil-motion-state-cursor) (list nil))
   (set (make-local-variable 'evil-emacs-state-cursor) (list nil))
   (evil-define-key 'normal doc-view-mode-map
     "p" 'doc-view-previous-page
     "n" 'doc-view-next-page
     "d" 'image-scroll-up
     "u" 'image-scroll-down
     "q" 'quit-window
     "v" 'image-scroll-up
     "V" 'image-scroll-down
     "h" 'doc-view-previous-page
     "j" 'doc-view-next-line-or-next-page
     "k" 'doc-view-previous-line-or-previous-page
     "l" 'doc-view-next-page
     "f" 'doc-view-next-line-or-next-page
     "b" 'doc-view-previous-line-or-previous-page
     "gg" 'doc-view-first-page
     "G" 'doc-view-last-page
     "gj" 'doc-view-goto-page
     "-" 'dired-jump
     "wf" 'doc-view-fit-width-to-window
     "wh" 'doc-view-fit-height-to-window
     "wp" 'doc-view-fit-page-to-window
     "ss" 'doc-view-set-slice
     "sm" 'doc-view-set-slice-using-mouse
     "sb" 'doc-view-set-slice-from-bounding-box
     "sr" 'doc-view-reset-slice
     "/" 'doc-view-search
     "?" 'doc-view-search-backward
     "C-t" 'doc-view-show-tooltip
     "C-c C-c" 'doc-view-toggle-display
     "C-c C-t" 'doc-view-open-text
     "gr" 'doc-view-revert-buffer
     "c" 'void)))

;; eshell
(add-hook
 'eshell-mode-hook
 (lambda ()
   (progn
     (define-key evil-normal-state-local-map (kbd "M-r") 'gmacs/counsel-yank-eshell-history)
     (define-key evil-insert-state-local-map (kbd "M-r") 'gmacs/counsel-insert-eshell-history)
     (define-key evil-normal-state-local-map (kbd "C-l") 'gmacs/eshell-clear)
     (define-key evil-insert-state-local-map (kbd "C-l") 'gmacs/eshell-clear)
     (define-key evil-insert-state-local-map (kbd "C-d") 'gmacs/eshell-send-eof)
     (define-key evil-normal-state-local-map (kbd "RET") 'eshell-send-input)
     (define-key evil-normal-state-local-map (kbd "C-j") 'eshell-send-input)
     (define-key evil-normal-state-local-map (kbd "C-m") 'eshell-send-input)
     (define-key evil-insert-state-local-map (kbd "RET") 'eshell-send-input)
     (define-key evil-insert-state-local-map (kbd "C-j") 'eshell-send-input)
     (define-key evil-insert-state-local-map (kbd "C-m") 'eshell-send-input))))

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

;; emacs binds
(define-key evil-emacs-state-map (kbd "C-k") 'kill-line)
(define-key evil-emacs-state-map (kbd "C-s") 'isearch-forward)
(define-key evil-emacs-state-map (kbd "C-w") 'kill-region)
(define-key evil-emacs-state-map (kbd "C-y") 'yank)
(define-key evil-emacs-state-map (kbd "M-r") 'move-to-window-line-top-bottom)
(define-key evil-emacs-state-map (kbd "M-a") 'backward-sentence)
(define-key evil-emacs-state-map (kbd "M-b") 'backward-word)
(define-key evil-emacs-state-map (kbd "M-w") 'kill-ring-save)
(define-key evil-emacs-state-map (kbd "M-v") 'scroll-down-command)
(define-key evil-emacs-state-map (kbd "M-m") 'back-to-indentation)
(define-key evil-emacs-state-map (kbd "M-k") 'kill-sentence)
(define-key evil-emacs-state-map (kbd "M-u") 'fix-word-upcase)
(define-key evil-emacs-state-map (kbd "M-l") 'fix-word-downcase)
(define-key evil-emacs-state-map (kbd "M-c") 'fix-word-capitalize)
(define-key evil-emacs-state-map (kbd "M-z") 'zop-to-char)
(define-key evil-emacs-state-map (kbd "M-.") 'xref-find-definitions)
(define-key evil-emacs-state-map (kbd "M-,") 'xref-pop-marker-stack)
(define-key evil-emacs-state-map (kbd "M-?") 'xref-find-references)
(define-key evil-emacs-state-map (kbd "M-@") 'mark-word)
(define-key evil-emacs-state-map (kbd "M-!") 'shell-command)
(define-key evil-emacs-state-map (kbd "M-<") 'beginning-of-buffer)
(define-key evil-emacs-state-map (kbd "M->") 'end-of-buffer)
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

;; unbinds to clean up global bindspace
(global-unset-key (kbd "C-k"))
(global-unset-key (kbd "C-w"))
(global-unset-key (kbd "C-s"))
(global-unset-key (kbd "C-y"))
(global-unset-key (kbd "M-j"))
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
(global-unset-key (kbd "M-r"))
(global-unset-key (kbd "M-r"))
(global-unset-key (kbd "M-."))
(global-unset-key (kbd "M-,"))
(global-unset-key (kbd "M-?"))
(global-unset-key (kbd "M-z"))
(global-unset-key (kbd "M-!"))
(global-unset-key (kbd "M-@"))
(global-unset-key (kbd "M->"))
(global-unset-key (kbd "M-<"))
(global-unset-key (kbd "C-x C-u"))
(global-unset-key (kbd "C-x C-l"))
(global-unset-key (kbd "C-M-j"))
(global-unset-key (kbd "C-M-k"))
(global-unset-key (kbd "C-M-v"))
(global-unset-key (kbd "C-M-h"))
(global-unset-key (kbd "C-M-u"))
(global-unset-key (kbd "C-M-d"))
(global-unset-key (kbd "C-M-f"))
(global-unset-key (kbd "C-M-e"))
(global-unset-key (kbd "C-M-b"))
(global-unset-key (kbd "C-M-a"))
(global-unset-key (kbd "C-M-t"))
(global-unset-key (kbd "C-M-l"))
(global-unset-key (kbd "C-M-n"))
(global-unset-key (kbd "C-M-p"))
(global-unset-key (kbd "C-M-o"))
(global-unset-key (kbd "C-M-/"))
(global-unset-key (kbd "C-M-."))

;; minibuffer
(setq evil-insert-state-message nil
      evil-visual-state-message nil
      evil-replace-state-message nil
      evil-normal-state-message nil
      evil-emacs-state-message nil
      evil-motion-state-message nil
      evil-insert-state-message nil
      evil-operator-state-message nil)

;; emacs mode for minibuffer
(add-hook
 'minibuffer-setup-hook
 '(lambda () (evil-emacs-state)))

(define-key evil-ex-completion-map (kbd "C-b") 'backward-char)
(define-key evil-ex-completion-map (kbd "C-d") 'delete-char)
(define-key evil-ex-completion-map (kbd "C-a") 'move-beginning-of-line)
(define-key evil-ex-completion-map (kbd "M-p") 'previous-complete-history-element)
(define-key evil-ex-completion-map (kbd "M-n") 'next-complete-history-element)

(add-hook
 'c-mode-common-hook
 (lambda ()
   (progn
     (define-key evil-normal-state-local-map (kbd "M-j") 'c-indent-new-comment-line)
     (define-key evil-insert-state-local-map (kbd "M-j") 'c-indent-new-comment-line)
     (define-key evil-normal-state-local-map (kbd "M-m") 'c-indent-new-comment-line)
     (define-key evil-insert-state-local-map (kbd "M-m") 'c-indent-new-comment-line))))

;; company abort on exit insert mode
(add-hook
 'evil-insert-state-exit-hook
 (lambda ()
   (if (bound-and-true-p company-mode)
       (company-abort))))

(provide 'evil-config)

;;; evil-config.el ends here
