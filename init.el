;;; init.el --- Jared Engler Emacs Config  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: jeemacs, config, init

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

;; Gmacs initial configuration file.

;;; Code:

;; set max mem alloc before gc for startup
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("marmalade" . "https://marmalade-repo.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ;; ("melpa-stable-mirror" . "https://www.mirrorservice.org/sites/stable.melpa.org/packages/")
        ;; ("melpa-mirror" . "https://www.mirrorservice.org/sites/melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

(if (version< emacs-version "27")
    (package-initialize))

;; add load path
(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; load custom vars
(load "variables")

;; load custom functions
(load "functions")

;; load global config
(load "global")

;; load package config
(load "packages")

;; load language packages config
(load "packages-lang")

;; enable savehist
(setq savehist-file "~/.emacs.d/.savehist"
      history-delete-duplicates t)
(savehist-mode 1)

;; write startup log, fix messages modeline, set dir to ~/.emacs.d
(add-hook 'emacs-startup-hook #'je/emacs-startup-hook)

(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq
 initial-scratch-message
 (format
  je/scratch-message
  (je/calculate-startup-info-string)
  je/current-user
  je/leader-key))

;; custom file
(setq custom-file je/custom-file-location)
(if (not (file-exists-p custom-file))
    (setq custom-file "~/.emacs.d/custom.el.example"))
(load custom-file 'noerror)

(provide 'init)
;;; init.el ends here
