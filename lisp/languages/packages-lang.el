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

(use-package emmet-mode
  :diminish "emm"
  :commands
  emmet-mode
  :init
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode)
  (add-hook 'nxml-mode-hook 'emmet-mode)
  (add-hook 'emmet-mode-hook 'gmacs/evil-emmet-mode-setup))

(use-package prog-mode
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
  :bind
  (:map xref--button-map
        ("C-j" . xref-goto-xref))
  :init
  (setq xref-after-jump-hook '(recenter)
        xref-after-return-hook '(recenter)))

(use-package nxml-mode
  :init
  (setq nxml-child-indent 4))

(use-package puppet-mode
  :commands puppet-mode)

(use-package restclient
  :commands restclient-mode
  :init
  (add-to-list
   'auto-mode-alist
   '("\\.rest\\'" . restclient-mode)))

(use-package sgml-mode
  :init
  (setq sgml-basic-offset 4))

(use-package yaml-mode)

(load "elisp-lang")
(load "clj-lang")
(load "php-lang")
(load "python-lang")
(load "js-lang")
(load "web-lang")
(load "java-lang")

(provide 'packages-lang)
;;; packages-lang.el ends here
