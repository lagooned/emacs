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
(define-key evil-normal-state-map (kbd "M-f")   'avy-goto-char)
(define-key evil-visual-state-map (kbd "M-f")   'avy-goto-char)
(define-key evil-normal-state-map (kbd "M-y")   'counsel-yank-pop)
(define-key evil-insert-state-map (kbd "M-y")   'counsel-yank-pop)
(define-key evil-insert-state-map (kbd "M-\\")  'evil-execute-in-emacs-state)
(define-key evil-insert-state-map (kbd "C-M-n") 'evil-execute-in-emacs-state)
(define-key evil-normal-state-map (kbd "C-a")   'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-b")   'evil-numbers/dec-at-pt)
(define-key evil-normal-state-map (kbd "C-=")   'er/expand-region)
(define-key evil-insert-state-map (kbd "C-=")   'er/expand-region)
(define-key evil-insert-state-map (kbd "TAB")   'tab-to-tab-stop)
(define-key evil-normal-state-map (kbd "U")     'undo-tree-visualize)
(define-key evil-normal-state-map (kbd "M-/")   'yas-expand)
(define-key evil-insert-state-map (kbd "M-/")   'yas-expand)
(define-key evil-normal-state-map (kbd "C-M-/") 'nil)
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
     ;; use the standard Dired bindings as a base
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

(provide 'evil-config)
;;; evil-config.el ends here
