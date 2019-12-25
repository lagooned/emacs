;;; leader-config.el --- evil leader config  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: jeemacs, config, evil, leader, binds

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

(evil-leader/set-leader (concat "<" je/leader-key ">"))

(evil-leader/set-key

  ;; buffer
  "b b" 'switch-to-buffer
  "b k" 'kill-this-buffer
  "b K" 'kill-buffer
  "b l" 'ibuffer
  "b r" 'rename-buffer
  "b c" 'clean-buffer-list
  "b s" 'save-some-buffers
  "TAB" 'evil-switch-to-windows-last-buffer

  ;; config
  "c i" 'je/open-init-config
  "c f" 'je/open-functions-config
  "c v" 'je/open-variables-config
  "c ," 'je/open-leader-config
  "c ." 'je/open-custom-config
  "c l" 'je/open-language-config
  "c g" 'je/open-global-config
  "c k" 'je/open-evil-config
  "c p" 'je/open-packages-config
  "c c" 'je/load-config

  ;; file
  "f f" 'find-file
  "f a" 'find-alternate-file
  "f c" 'je/cleanup-file
  "f r" 'counsel-recentf
  "f s" 'save-buffer
  "f w" 'write-file

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
  "h l" 'view-lossage

  ;; insert
  "i f" 'insert-file
  "i s" 'yas-insert-snippet
  "i h" 'auto-insert

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
  "o o" 'je/open-org-dir
  "o d" 'je/open-downloads-dir
  "o c" 'je/open-code-dir
  "o h" 'je/open-home-dir
  "o s" 'je/switch-to-scratch-buffer
  "o m" 'je/switch-to-messages-buffer

  ;; project
  "p s" 'projectile-vc
  "p e" 'projectile-run-eshell
  "p p" 'counsel-projectile-switch-project
  "p r" 'je/projectile-root-dir
  "p f" 'je/counsel-projectile-find-file-region
  "p d" 'je/counsel-projectile-find-dir-region
  "p g" 'je/counsel-git-grep-region

  ;; search
  "s s" 'je/swiper-region-thing
  "s c" 'avy-goto-char
  "s l" 'avy-goto-line
  "s j" 'avy-goto-word-0
  "s r" 'ivy-resume

  ;; toggles
  "t l" 'linum-relative-mode
  "t w" 'whitespace-mode
  "t i" 'aggressive-indent-mode
  "t t" 'toggle-truncate-lines
  "t p" 'smartparens-mode
  "t h" 'highlight-symbol-at-point
  "t H" 'je/unhighlight-all

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
  "w m" 'je/switch-to-minibuffer
  "w RET" 'toggle-frame-fullscreen
  "]" 'je/enlarge-window-horizontally
  "[" 'je/shrink-window-horizontally
  "=" 'je/enlarge-window
  "-" 'je/shrink-window

  ;; command
  ";" 'counsel-M-x
  "`" 'eshell
  "!" 'shell-command
  "@" 'async-shell-command

  ;; quit
  "q q" 'save-buffers-kill-terminal)

(global-evil-leader-mode 1)

(provide 'leader-config)
;;; leader-config.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
