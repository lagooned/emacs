;;; global.el --- jeemacs main config  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: jeemacs, config, defaults, encoding, fringe, bell, cursor, warnings

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

;;; Gmacs sane defaults configuration.

;;; Code:

;; use utf-8
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type
        '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

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

(define-fringe-bitmap 'left-arrow
  [#b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000])

;; let text breath
(fringe-mode (frame-char-width))

;; decouple window and minibuffer fringes
(add-hook 'after-init-hook #'je/minibuffer-fringe-setup)
(je/disable-minibuffer-fringe)

;; backups
(setq version-control t
      kept-new-versions 10
      kept-old-versions 0
      delete-old-versions t
      backup-by-copying t
      vc-make-backup-files t
      backup-directory-alist '(("" . "~/.backup/save")))

(add-hook 'before-save-hook 'je/force-buffer-backup)

;; auto save to temp
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; cursor blink
(blink-cursor-mode -1)

;; cursor stretch
(setq x-stretch-cursor t)

;; disable eldoc to enable it later
(global-eldoc-mode -1)

;; no cursor in other windows
(setq-default cursor-in-non-selected-windows nil)

;; show inputs immediately
(setq echo-keystrokes 0.01)

;; bury scratch on kill
(add-hook 'kill-buffer-query-functions 'je/dont-kill-scratch-or-dired)

;; no tool bar
(tool-bar-mode -1)

;; no tabs
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)

;; no scroll bar
(scroll-bar-mode -1)

;; wrap words
(setq-default word-wrap t)

;; set default tab display width to 4 spaces
(setq-default tab-width 4)
(setq-default c-basic-offset 4)

;; reload init
(global-set-key (kbd "C-c i") 'je/load-config)

;; make prompts easier
(defalias 'yes-or-no-p 'y-or-n-p)

;; sentence navigation
(setq sentence-end-double-space nil)

;; line spaceing
(setq-default line-spacing 6)

;; no tooltips
(tooltip-mode 0)

;; remember place
(save-place-mode 1)

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

;; enable erase buffer
(put 'erase-buffer 'disabled nil)

;; disable electric indent
(electric-indent-mode -1)

;; weird next-line bug
(setq auto-window-vscroll nil)

;; disable bidirectional text
(setq bidi-display-reordering nil)
(setq-default bidi-display-reordering nil)

;; fast but imprecise scrolling
(setq fast-but-imprecise-scrolling t)

;; history
(setq history-length 10)
(setq savehist-additional-variables '(search-ring regexp-search-ring))

;; no bell
(setq ring-bell-function 'ignore)
(setq visible-bell nil)

;; no percent in modeline
(setq mode-line-percent-position 'nil)
(size-indication-mode -1)
(line-number-mode 1)
(column-number-mode 1)

(provide 'global)
;;; global.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
