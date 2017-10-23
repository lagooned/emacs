;;; init.el --- Jared Engler Emacs Config            -*- lexical-binding: t; -*-

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

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ;; ("melpa-stable-mirror" . "https://www.mirrorservice.org/sites/stable.melpa.org/packages/")
                         ;; ("melpa-mirror" . "https://www.mirrorservice.org/sites/melpa.org/packages/")
 						 ("org" . "http://orgmode.org/elpa/"))) 
(package-initialize)

;; add load path
(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; load custom functions
(load "functions.el")

;; load global config
(load "global.el")

;; load package config
(load "packages.el")

;; customs
(setq custom-file "~/.emacs.d/.custom.el")
(load custom-file 'noerror)

(provide 'init)
;;; init.el ends here

