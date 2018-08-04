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

(use-package xref
  :ensure nil
  :bind
  (:map xref--button-map
        ("C-j" . xref-goto-xref))
  :init
  (setq xref-after-jump-hook '(recenter)
        xref-after-return-hook '(recenter)))

(use-package lsp-mode
  :commands lsp-mode
  :init
  (setq lsp-enable-eldoc nil
        lsp-inhibit-message t)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (add-hook
   'lsp-mode-hook
   (lambda ()
     (diminish 'lsp-mode "lsp"))))

(use-package lsp-ui
  :commands lsp-ui-mode
  :init
  (setq lsp-ui-flycheck-enable nil
        lsp-ui-doc-enable nil
        lsp-ui-sideline-enable nil))

(use-package company-lsp
  :after lsp-mode
  :config
  (setq company-lsp-cache-candidates 'auto
        company-lsp-enable-snippet t
        company-lsp-enable-recompletion t))

(load "elisp-lang")
(load "php-lang")
(load "js-lang")
(load "web-lang")
(load "java-lang")

(add-hook
 'prog-mode-hook
 (lambda () (company-mode 1)))

(provide 'packages-lang)
;;; packages-lang.el ends here
