;;; global.el --- jeemacs main config  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Jared M. Engler

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

;;; Je/Emacs sane defaults configuration.

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
(setq
 blink-cursor-delay 1.2
 blink-cursor-interval 0.2
 blink-cursor-blinks 1)
(blink-cursor-mode 1)

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
(if (eq system-type 'darwin)
    (push '(tool-bar-lines . 0) default-frame-alist)
  (tool-bar-mode -1))

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

;; make prompts easier
(defalias 'yes-or-no-p 'y-or-n-p)

;; sentence navigation
(setq sentence-end-double-space nil)

;; line spaceing
(setq-default line-spacing 6)

;; no tooltips
(tooltip-mode 0)

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
(setq
 history-length 10
 savehist-file "~/.emacs.d/.savehist"
 history-delete-duplicates t
 savehist-additional-variables '(search-ring regexp-search-ring))
(savehist-mode 1)

;; no bell
(setq ring-bell-function 'ignore)
(setq visible-bell nil)

;; no percent in modeline
(setq mode-line-percent-position 'nil)
(size-indication-mode -1)
(line-number-mode 1)
(column-number-mode -1)

;; menu-bar-display
(when (or (not (eq system-type 'darwin))
          (not (display-graphic-p)))
  (menu-bar-mode -1))

;; title bar name command
(setq frame-title-format 'buffer-file-truename)

;; don't pass commands to osx
(setq mac-pass-command-to-system nil)

;; no mode line dashes in -nw
(setq-default mode-line-front-space " ")
(setq-default mode-line-end-spaces " ")

(provide 'global)
;;; global.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
