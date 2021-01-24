;;; functional.el --- functional-style utility functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: reduce, lan, helpers, functions

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

;; Je/Emacs functional-style utility functions

;;; Code:

(defsubst curry (function &rest arguments)
  (let ((function function)
        (arguments arguments))
    (lambda (&rest more) (apply function (append arguments more)))))

(defsubst rcurry (function &rest arguments)
  (let ((function function)
        (arguments arguments))
    (lambda (&rest more) (apply function (append more arguments)))))

(defsubst compose (function &rest more-functions)
  (cl-reduce (lambda (f g)
               (let ((f f) (g g))
                 (lambda (&rest arguments)
                   (funcall f (apply g arguments)))))
             more-functions
             :initial-value function))

(provide 'functional)
;;; functional.el ends here

