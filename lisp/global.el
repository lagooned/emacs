;;; global.el --- main emacs config                  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: c, data, docs, faces, wp, local, multimedia, vc, calendar, maint

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

;; use utf-8
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; set default warning level
(setq warning-minimum-level :emergency)

;; new fringe arrows
(define-fringe-bitmap 'right-curly-arrow
  [#b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000])

(define-fringe-bitmap 'left-curly-arrow
  [#b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000])

(define-fringe-bitmap 'right-arrow
  [#b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000])

;; setup user
(setq user-full-name "Jared M. Engler"
      user-mail-address "jared.lite@gmail.com"
      calendar-latitude 40.52
      calendar-longitude -88.99
      calendar-location-name "Normal, IL")

;; frame title format
(setq frame-title-format
      '((buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; backup files to temp
;; (setq auto-save-default nil)
(setq backup-directory-alist `((".*" . , temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" , temporary-file-directory t)))
;; (setq version-control t)
;; (setq delete-old-versions t)

;; show column too
(column-number-mode 1)

;; unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse
      uniquify-after-kill-buffer-p t
      uniquify-separator ":"
      uniquify-ignore-buffers-re "^\\*")

;; show inputs immediately
(setq echo-keystrokes 0.01)

;; bury scratch on kill
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

;; no tool bar
(tool-bar-mode -1)

;; no tabs
(setq-default indent-tabs-mode nil)

;; no scroll bar
(scroll-bar-mode -1)

;; symbols
(global-prettify-symbols-mode t)

;; truncate and wrap lines
;; (setq-default truncate-lines 0)
(setq-default word-wrap t)

;; show parens
(show-paren-mode 1)
(setq show-paren-delay 0)

;; set default tab display width to 4 spaces
(setq-default tab-width 4)
(setq-default c-basic-offset 4)

;; reload init
(global-set-key (kbd "C-c i") 'my/load-config)

;; make prompts easier
(defalias 'yes-or-no-p 'y-or-n-p)

;; highlight line
;; (when window-system
;; (global-hl-line-mode))

;; sentence navigation
(setq sentence-end-double-space nil)

;; some custom binds
(global-set-key (kbd "C-;") 'comment-line)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-c C-c C-l") 'my/cleanup-buffer)
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-x f") 'find-file)
(global-set-key (kbd "M-q") 'toggle-truncate-lines)

;; insert newlines with C-n at end of buffer
(setq next-line-add-newlines t)

;; auto-insert
(auto-insert-mode 1)

;; platform specific options
(when (eq system-type 'darwin)
  ;; railwaycat/homebrew-emacsmacport
  (set-face-attribute 'default nil :family "Source Code Pro")
  ;; (set-face-attribute 'default nil :family "Monaco")
  (set-face-attribute 'default nil :height 120)
  (unless (display-graphic-p) (menu-bar-mode -1))
  (setq ispell-program-name "aspell"))

(when (eq system-type 'gnu/linux)
  (set-face-attribute 'default nil :family "Ubuntu Mono")
  (set-face-attribute 'default nil :height 110)
  (unless (display-graphic-p) (menu-bar-mode -1)))

(when (eq system-type 'windows-nt)
  (set-face-attribute 'default nil :family "Consolas")
  (set-face-attribute 'default nil :height 130)
  (menu-bar-mode -1))

(provide 'global)
;;; global.el ends here
