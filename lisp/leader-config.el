;;; leader-config.el --- evil leader config                 -*- lexical-binding: t; -*-

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

(require 'evil-leader)
(evil-leader/set-leader "<SPC>")

(evil-leader/set-key
  ;; buffer
  "b b" 'switch-to-buffer
  "b k" 'kill-buffer
  "b l" 'list-buffers
  ;; emacs
  "e r" 'restart-emacs
  "e l" 'my/load-config
  ;; file
  "f f" 'find-file
  "f a" 'find-alternate-file
  "f r" 'counsel-recentf
  "f l" 'counsel-locate
  "f s" 'save-buffer
  "f w" 'write-file
  ;; help
  "h d b" 'describe-bindings
  "h d d" 'describe-distribution
  "h d f" 'describe-function
  "h d k" 'describe-key
  "h d m" 'describe-mode
  "h d v" 'describe-variable
  "h d s" 'describe-symbol
  "h d S" 'describe-syntax
  "h d p" 'describe-package
  "h v l" 'view-lossage
  "h a" 'about-emacs
  ;; insert
  "i f" 'insert-file
  "i s" 'yas-insert-snippet
  "i l" 'org-insert-link-global
  ;; org
  "o l" 'org-open-at-point
  "o a" 'org-agenda
  "o b" 'org-iswitchb
  ;; project
  "p g" 'my/counsel-rg-region
  "p f" 'my/counsel-git-region
  "p s" 'magit-status
  "p d" 'magit-diff-popup
  ;; search
  "s s" 'my/swiper-region
  "s c" 'avy-goto-char
  "s t" 'my/swiper-thing
  ;; toggles
  "t l" 'linum-relative-mode
  "t w" 'whitespace-mode
  "t -" 'centered-cursor-mode
  "t g" 'golden-ratio-mode
  "t I" 'aggressive-indent-mode
  "t i" 'indent-guide-mode
  "t A" 'auto-revert-mode
  ;; universal
  "u" 'universal-argument
  ;; window
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
  ;; yank
  "y l" 'org-store-link
  ;; command
  "SPC" 'counsel-M-x)

(evil-leader/set-key-for-mode 'org-mode
  "e e" 'org-export-dispatch)

(evil-leader/set-key-for-mode 'emacs-lisp-mode
  "e e" 'eval-last-sexp)

(evil-leader/set-key-for-mode 'web-mode
  "n" 'emmet-next-edit-point
  "N" 'emmet-prev-edit-point)

(provide 'leader-config)
;;; leader-config.el ends here


