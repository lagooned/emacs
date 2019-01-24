;;; php-lang.el --- php langauge tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Jared Matthew Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: gmacs, config, languages, php, syntax

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

;; Gmacs PHP langauge tools.

;;; Code:

(use-package php-mode)

(flycheck-define-checker gmacs-php
  "A PHP syntax checker using the PHP command line interpreter."
  :command
  ("php" "-l"
   "-d" "error_reporting=E_ALL"
   "-d" "display_errors=1"
   "-d" "log_errors=0"
   source)
  :error-patterns
  ((error
    line-start
    (or "Parse" "Fatal" "syntax") " error" (any ":" ",") " "
    (message) " in " (file-name) " on line " line line-end))
  :modes
  (php-mode)
  :next-checkers
  ((warning . php-phpmd)
   (warning . php-phpcs)))

(provide 'php-lang)
;;; php-lang.el ends here
