;;; elisp-lang.el --- elisp language tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: languages, lisp, help

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

;; Gmacs Elisp language tools.

;;; Code:

(use-package eldoc
  :diminish eldoc-mode
  :commands eldoc-mode
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode))

(use-package smart-jump
  :config
  (smart-jump-register :modes '(emacs-lisp-mode lisp-interaction-mode)
                       :jump-fn 'xref-find-definitions
                       :pop-fn 'pop-tag-mark
                       :refs-fn 'gmacs/xref-find-apropos-symbol
                       :should-jump t
                       :heuristic 'error
                       :async nil))

(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   (set (make-local-variable 'company-backends)
        (list (cons 'company-capf
                    (car company-backends))))))

(provide 'elisp-lang)
;;; elisp-lang.el ends here
