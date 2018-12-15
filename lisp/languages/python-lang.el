;;; python-lang.el --- gmacs python language tools   -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Jared M. Engler

;; Author: Jared M. Engler;;; python-lang.el --- gmacs python language tools   -*- lexical-binding: t; -*- <jared.lite@gmail.com>
;; Keywords: languages, convenience, languages, tools

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

;; Gmacs Python languages tools

;;; Code:

(use-package lsp-python
  :commands lsp-python-enable
  :init
  (add-hook 'python-mode-hook #'gmacs/lsp-python-enable))

(provide 'python-lang)
;;; python-lang.el ends here