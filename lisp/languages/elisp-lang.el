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

(use-package emacs-lisp-mode
  :ensure nil
  :init
  (defun gmacs/emacs-lisp-mode-hook ()
    (add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
    (make-variable-buffer-local 'company-backends)
    (push '(company-capf company-yasnippet) company-backends))
  (add-hook 'emacs-lisp-mode-hook 'gmacs/emacs-lisp-mode-hook))

(use-package eldoc
  :defer t
  :diminish eldoc-mode
  :commands eldoc-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode))

(evil-leader/set-key-for-mode 'emacs-lisp-mode
  "C-e" 'gmacs/move-eol-eval-last-sexp
  "e" 'eval-last-sexp)

(evil-leader/set-key-for-mode 'lisp-interaction-mode
  "C-e" 'gmacs/move-eol-eval-last-sexp
  "e" 'eval-last-sexp)

(which-key-add-key-based-replacements
  "SPC e" "eval-sexp"
  "SPC C-e" "eval-eol")

(provide 'elisp-lang)
;;; elisp-lang.el ends here
