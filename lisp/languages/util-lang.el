;;; util-lang.el --- je/emacs language utils         -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: utils, jeemacs, lang, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Je/emacs language utils.

;;; Code:

(use-package lsp-mode
  :commands lsp
  :init
  (setq
   lsp-inhibit-message 1
   lsp-enable-indention nil))

(use-package company-lsp
  :after lsp company yasnippet
  :init
  (when (version<= "27" emacs-version)
    (define-advice c-clear-string-fences
        (:around (fn) inhibit-args-out-of-range-error)
      (ignore-errors
        (funcall fn)))))

(provide 'util-lang)
;;; util-lang.el ends here
