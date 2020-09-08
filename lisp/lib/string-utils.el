;;; string-utils.el --- string utility functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: utils, functions, lib, string

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

;; Useful string utility functions.

;;; Code:

(require 'functional)
(require 'cl-seq)

(defun string-utils/add-quotes (str)
  "Surround `STR' in quotes."
  (concat "\"" str "\""))

(defun string-utils/escape-str-for-command (str)
  "Escape parens, space, and quotes in `STR'."
  (string-utils/escape-command-str str ["\"" "`"]))

(defun string-utils/escape-command-str (str charlist)
  "Escapes all instances of each element of `CHARLIST' in `STR'."
  (funcall (--string-utils/big-escape-char-func charlist) str))

(defun --string-utils/big-escape-char-func (charlist)
  (cl-reduce #'compose (--string-utils/escape-char-funcs charlist)))

(defun --string-utils/escape-char-funcs (charlist)
  (mapcar #'--string-util/escape-char-func charlist))

(defun --string-util/escape-char-func (char)
  (curry 'string-utils/escape-character-str char))

(defun string-utils/escape-character-str (char str)
  "Escapes every instance of `CHAR' in `STR'."
  (string-utils/replace-in-string char (concat "\\" char) str))

(defun string-utils/replace-in-string (what with in)
  "Replace `WHAT' `WITH' `IN'."
  (replace-regexp-in-string (regexp-quote what) with in nil 'literal))

(provide 'string-utils)
;;; string-utils.el ends here
