;;; functional.el --- functional-style utility functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: map, flatmap, reduce, lan, helpers, functions

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

;; Gmacs functional-style utility functions

;;; Code:

(defun flatten (list)
  (mapcan (lambda (x) (if (listp x) x nil)) list))

(defun flatmap (func list)
  (flatten (mapcar func list)))

(provide 'functional)
;;; functional.el ends here

