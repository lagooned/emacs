;;; package-lang.el --- language tools defs  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: languages, lisp, tools

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

;; Gmacs language-specific package definitions.

;;; Code:

(use-package dumb-jump
  :after smart-jump
  :bind
  (:map dumb-jump-mode-map
        ("C-M-g" . nil)
        ("C-M-p" . nil)
        ("C-M-q" . nil))
  :init
  (setq dumb-jump-selector 'ivy
        dumb-jump-prefer-searcher 'rg
        dumb-jump-quiet t)
  (when (eq system-type 'windows-nt)
    (setq dumb-jump-force-searcher 'rg)))

(use-package flycheck
  :diminish flycheck-mode "chk"
  :init
  (custom-set-faces
   '(flycheck-error ((t (:foreground "red" :underline nil))))
   '(flycheck-info ((t (:foreground "green" :underline nil))))
   '(flycheck-warning ((t (:foreground "yellow" :underline nil)))))
  (defvar flycheck-indication-mode)
  (defvar flycheck-highlighting-mode)
  (setq flycheck-indication-mode nil
        flycheck-highlighting-mode 'lines))

(use-package smart-jump
  :init
  (defvar smart-jump-find-references-fallback-function)
  (defvar smart-jump-bind-keys-for-evil)
  (defvar smart-jump-bind-keys)
  (defvar smart-jump-refs-key)
  (defvar smart-jump-pop-key)
  (setq smart-jump-find-references-fallback-function nil
        smart-jump-bind-keys-for-evil nil
        smart-jump-bind-keys nil
        smart-jump-refs-key nil
        smart-jump-pop-key nil))

(use-package xref
  :ensure nil
  :bind
  (:map xref--button-map
        ("C-j" . xref-goto-xref))
  :init
  (setq xref-after-jump-hook '(recenter)
        xref-after-return-hook '(recenter)))

(load "elisp-lang")
(load "php-lang")
(load "js-lang")

(provide 'packages-lang)
;;; packages-lang.el ends here
