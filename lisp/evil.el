;;; evil.el --- evil config                          -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>

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

;; initial states
(evil-set-initial-state 'term-mode 'emacs)
(evil-set-initial-state 'ansi-term-mode 'emacs)
(evil-set-initial-state 'magit-mode 'emacs)
(evil-set-initial-state 'magit-log-edit-mode 'emacs)

;; evil binds
(define-key evil-normal-state-map "j"                 'evil-next-visual-line)
(define-key evil-visual-state-map "j"                 'evil-next-visual-line)
(define-key evil-normal-state-map "k"                 'evil-previous-visual-line)
(define-key evil-visual-state-map "k"                 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "M-f")         'avy-goto-char)
(define-key evil-visual-state-map (kbd "M-f")         'avy-goto-char)
(define-key evil-insert-state-map (kbd "C-c j")       'avy-goto-char)
(define-key evil-emacs-state-map  (kbd "C-c j")       'avy-goto-char)
(define-key evil-normal-state-map (kbd "M-y")         'counsel-yank-pop)
(define-key evil-insert-state-map (kbd "M-y")         'counsel-yank-pop)
(define-key evil-emacs-state-map  (kbd "M-y")         'counsel-yank-pop)
(define-key evil-insert-state-map (kbd "M-\\")        'evil-execute-in-emacs-state)
(define-key evil-insert-state-map (kbd "C-M-n")       'evil-execute-in-emacs-state)
(define-key evil-normal-state-map (kbd "C-c C-=")     'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-c C--")     'evil-numbers/dec-at-pt)
(define-key evil-normal-state-map (kbd "C-=")         'er/expand-region)
(define-key evil-insert-state-map (kbd "C-=")         'er/expand-region)
(define-key evil-normal-state-map (kbd "M-u")         'fx-word-upcase)
(define-key evil-normal-state-map (kbd "M-l")         'fix-word-downcase)
(define-key evil-normal-state-map (kbd "M-c")         'fix-word-capitalize)
(define-key evil-insert-state-map (kbd "TAB")         'tab-to-tab-stop)
(define-key evil-normal-state-map (kbd "U")           'undo-tree-visualize)
(define-key evil-normal-state-map (kbd "M-/")         'yas-expand)
(define-key evil-insert-state-map (kbd "M-/")         'yas-expand)
(define-key evil-emacs-state-map  (kbd "M-/")         'hippie-expand)
(define-key evil-normal-state-map (kbd "C-M-/")       'nil)

(provide 'evil)
;;; evil.el ends here
