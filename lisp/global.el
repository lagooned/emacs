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
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type
        '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; set max mem alloc before gc
(setq gc-cons-threshold 50000000)

;; no warnings
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

;; let text breath
(fringe-mode 20)

;; no fringes in minibuffer
(add-hook
 'after-init-hook
 '(lambda ()
    (progn
      (set-window-fringes (minibuffer-window) 0 0 nil)
      (add-hook
       'minibuffer-setup-hook
       '(lambda () (set-window-fringes (minibuffer-window) 0 0 nil))))))

;; frame title format
(setq frame-title-format
      '((buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; backups
(setq version-control t
      kept-new-versions 10
      kept-old-versions 0
      delete-old-versions t
      backup-by-copying t
      vc-make-backup-files t
      backup-directory-alist '(("" . "~/.emacs.d/backup/save")))

;; auto save to temp
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; show column too
(column-number-mode 1)

;; cursor blink
(blink-cursor-mode t)
(setq blink-cursor-blinks -1)

;; cursor stretch
(setq x-stretch-cursor t)

;; no cursor in other windows
(setq default-cursor-in-non-selected-windows nil)

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
(setq indent-tabs-mode nil)

;; no scroll bar
(scroll-bar-mode -1)

;; symbols
(global-prettify-symbols-mode t)

;; truncate and wrap lines
(add-hook
 'prog-mode-hook
 (lambda ()
   (progn
     (toggle-truncate-lines 1)
     (message nil))))

(setq-default word-wrap t)

;; set default tab display width to 4 spaces
(setq-default tab-width 4)
(setq-default c-basic-offset 4)

;; reload init
(global-set-key (kbd "C-c i") 'gmacs/load-config)

;; make prompts easier
(defalias 'yes-or-no-p 'y-or-n-p)

;; sentence navigation
(setq sentence-end-double-space nil)

;; line spaceing
(setq-default line-spacing 3)

;; no tooltips
(tooltip-mode 0)

;; remember place
(save-place-mode 1)
(setq save-place-forget-unreadable-files nil)

;; create directories with find file
(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir)))))

;; enable narrow
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;; preferred fonts list
(let ((fonts '("Input"
               "Source Code Pro"
               "Monaco"
               "Deja Vu Sans Mono"
               "Consolas"
               "Monospace")))
  (dolist (font (reverse fonts) t)
    (if (member font (font-family-list))
        (set-face-attribute 'default nil :family font))))

;; disable electric indent
(electric-indent-mode -1)

(provide 'global)
;;; global.el ends here
