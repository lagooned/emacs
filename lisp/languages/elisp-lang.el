;;; elisp-lang.el --- elisp language tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: jeemacs, config, languages, elisp

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

(use-package elisp-mode
  :hook ((emacs-lisp-mode . aggressive-indent-mode)
         (emacs-lisp-mode . jeemacs/emacs-lisp-setup)))

(use-package eldoc
  :diminish eldoc-mode
  :commands eldoc-mode
  :hook ((emacs-lisp-mode . turn-on-eldoc-mode)
         (lisp-interaction-mode . turn-on-eldoc-mode)))

(evil-leader/set-key-for-mode 'emacs-lisp-mode
  "e" 'eval-last-sexp)

(evil-leader/set-key-for-mode 'lisp-interaction-mode
  "e" 'eval-last-sexp)

(which-key-add-key-based-replacements
  "SPC e" "eval-sexp")

(provide 'elisp-lang)
;;; elisp-lang.el ends here
