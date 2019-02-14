;;; package-lang.el --- gmacs language-specific tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: gmacs, config, languages, packages, syntax, xref, lsp, debug

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

(use-package apache-mode
  :commands apache-mode)

(use-package dockerfile-mode
  :commands dockerfile-mode
  :init
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package prog-mode
  :ensure nil
  :commands prog-mode
  :init
  (add-hook 'prog-mode-hook #'too-long-lines-mode)
  (add-hook 'prog-mode-hook #'gmacs/enable-truncate-lines-no-message)
  (add-hook 'prog-mode-hook #'gmacs/enable-company-mode))

(use-package flycheck
  :diminish flycheck-mode "flyc"
  :init
  (custom-set-faces
   '(flycheck-error ((t (:foreground "red" :underline nil))))
   '(flycheck-info ((t (:foreground "green" :underline nil))))
   '(flycheck-warning ((t (:foreground "yellow" :underline nil)))))
  (setq flycheck-indication-mode nil
        flycheck-highlighting-mode 'lines
        flycheck-emacs-lisp-load-path 'inherit))

(use-package groovy-mode
  :commands groovy-mode)

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
  (setq lsp-eldoc-render-all nil
        lsp-inhibit-message t)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (add-hook 'lsp-mode-hook (lambda () (diminish 'lsp-mode "lsp"))))

(use-package lsp-ui
  :commands lsp-ui-mode
  :init
  (setq lsp-ui-flycheck-enable t
        lsp-ui-doc-enable nil
        lsp-ui-sideline-enable nil)
  :config
  (remove-hook 'lsp-eldoc-hook 'lsp-document-highlight))

(use-package company-lsp
  :after lsp-mode
  :config
  (setq company-lsp-cache-candidates t
        company-lsp-enable-snippet t
        company-lsp-enable-recompletion t))

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode 1)
  (dap-ui-mode 1))

(use-package nxml-mode
  :ensure nil
  :init
  (setq nxml-child-indent 4))

(use-package puppet-mode
  :commands puppet-mode)

(use-package sgml-mode
  :init
  (setq sgml-basic-offset 4))

(use-package yaml-mode)

(load "elisp-lang")
(load "php-lang")
(load "python-lang")
(load "js-lang")
(load "web-lang")
(load "java-lang")

(provide 'packages-lang)
;;; packages-lang.el ends here
