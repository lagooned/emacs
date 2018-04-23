;;; java-lang.el --- Gmacs java language tools       -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: languages, tools

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

;; Gmacs java language tools.

;;; Code:

(use-package lsp-java)

(add-hook
 'java-mode-hook
 (lambda () (if (not (member 'company-capf company-backends))
           (push 'company-capf company-backends))))

(add-hook
 'java-mode-hook #'lsp-java-enable)

(provide 'java-lang)
;;; java-lang.el ends here
