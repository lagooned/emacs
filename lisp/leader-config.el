;;; leader-config.el --- evil leader config                 -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: gmacs, evil, leader, config

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

;;; Gmacs evil-leader configuration.

;;; Code:

(require 'evil-leader)

(global-evil-leader-mode 1)

(setq evil-leader/in-all-states 1)

(evil-leader/set-leader "<SPC>")

(evil-leader/set-key
  ;; buffer
  "b b" 'switch-to-buffer
  "b k" 'kill-buffer
  "b K" 'kill-this-buffer
  "b C-k" 'kill-matching-buffers
  "b h" 'gmacs/switch-to-scratch-buffer
  "b m" 'gmacs/switch-to-messages-buffer
  "b l" 'ibuffer
  "b r" 'revert-buffer
  "b R" 'rename-buffer
  "b c" 'clean-buffer-list
  "b s" 'save-some-buffers
  "TAB" 'evil-switch-to-windows-last-buffer

  ;; config
  "c i" 'gmacs/open-init-config
  "c f" 'gmacs/open-functions-config
  "c v" 'gmacs/open-variables-config
  "c ," 'gmacs/open-leader-config
  "c ." 'gmacs/open-custom-config
  "c l" 'gmacs/open-language-config
  "c g" 'gmacs/open-global-config
  "c e" 'gmacs/open-environment-config
  "c k" 'gmacs/open-evil-config
  "c p" 'gmacs/open-packages-config
  "c c" 'gmacs/load-config

  ;; file
  "f f" 'find-file
  "f a" 'find-alternate-file
  "f c" 'gmacs/cleanup-file
  "f r" 'counsel-recentf
  "f l" 'counsel-locate
  "f s" 'save-buffer
  "f w" 'write-file

  ;; grep
  "g" 'gmacs/counsel-rg-region
  "G" 'gmacs/ripgrep-regexp-git

  ;; help
  "h a"   'about-emacs
  "h i"   'info
  "h h"   'help-for-help
  "h I"   'info-other-window
  "h n"   'view-emacs-news
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
  "h v l" 'view-lossage

  ;; insert
  "i f" 'insert-file
  "i s" 'yas-insert-snippet
  "i h" 'auto-insert
  "i l s" 'lorem-ipsum-insert-sentences
  "i l p" 'Lorem-ipsum-insert-paragraphs

  ;; link
  "l s" 'org-store-link
  "l l" 'browse-url-at-point

  ;; narrow
  "n r" 'narrow-to-region
  "n f" 'narrow-to-defun
  "n p" 'narrow-to-page
  "n w" 'widen

  ;; open
  "o a" 'org-agenda
  "o o" 'gmacs/open-org-dir
  "o d" 'gmacs/open-downloads-dir
  "o c" 'gmacs/open-code-dir
  "o h" 'gmacs/open-home-dir

  ;; project
  "p s" 'gmacs/magit-status
  "p e" 'projectile-run-eshell
  "p p" 'counsel-projectile-switch-project
  "p r" 'gmacs/projectile-root-dir
  "p f" 'gmacs/counsel-git-region
  "p d" 'gmacs/counsel-projectile-find-dir-region
  "p D" 'magit-diff-popup
  "p F" 'magit-file-popup
  "p b" 'magit-blame

  ;; search
  "s s" 'gmacs/swiper-region-thing
  "s c" 'avy-goto-char
  "s l" 'avy-goto-line
  "s j" 'avy-goto-word-0

  ;; toggles
  "t l" 'linum-relative-mode
  "t w" 'whitespace-mode
  "t -" 'centered-cursor-mode
  "t I" 'aggressive-indent-mode
  "t i" 'indent-guide-mode
  "t A" 'auto-revert-mode
  "t t" 'toggle-truncate-lines
  "t p" 'smartparens-mode
  "t g" 'git-gutter-mode
  "t f" 'focus-mode
  "t c" 'flycheck-mode
  "t s" 'gmacs/toggle-spelling
  "t h" 'highlight-symbol-at-point
  "t H" 'gmacs/unhighlight-all
  "t z" 'zoom-mode

  ;; universal
  "u" 'universal-argument
  "U" 'negative-argument

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
  "w f" 'reposition-window
  "w m" 'gmacs/switch-to-minibuffer
  "w u" 'winner-undo
  "w r" 'winner-redo
  "w RET" 'toggle-frame-fullscreen
  "]" 'gmacs/enlarge-window-horizontally
  "[" 'gmacs/shrink-window-horizontally
  "=" 'gmacs/enlarge-window
  "-" 'gmacs/shrink-window

  ;; command
  ";" 'counsel-M-x
  "`" 'eshell
  "!" 'shell-command
  "@" 'async-shell-command

  ;; elscreen
  "1"   'elscreen-jump
  "2"   'elscreen-jump
  "3"   'elscreen-jump
  "4"   'elscreen-jump
  "5"   'elscreen-jump
  "6"   'elscreen-jump
  "7"   'elscreen-jump
  "8"   'elscreen-jump
  "9"   'elscreen-jump
  "0"   'elscreen-jump
  "z z" 'elscreen-toggle
  "z l" 'elscreen-display-screen-name-list
  "z c" 'elscreen-create
  "z C" 'elscreen-clone
  "z k" 'elscreen-kill
  "z r" 'elscreen-screen-nickname
  "z K" 'elscreen-kill-others
  "z n" 'elscreen-next
  "z p" 'elscreen-previous
  "z ;" 'elscreen-execute-extended-command
  "z d" 'elscreen-dired
  "z s" 'elscreen-swap

  ;; quit
  "q q" 'save-buffers-kill-terminal
  "q r" 'restart-emacs)

(evil-leader/set-key-for-mode 'org-mode
  "l l" 'gmacs/org-link-follow
  "l i" 'org-insert-link-global
  "m c" 'org-toggle-checkbox
  "m e" 'org-export-dispatch
  "n e" 'org-narrow-to-element
  "n b" 'org-narrow-to-block
  "n s" 'org-narrow-to-subtree)

(provide 'leader-config)
;;; leader-config.el ends here
