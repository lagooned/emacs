;;; java-lang.el --- gmacs java language tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: gmacs, config, languages, jav

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

(use-package lsp-java
  :commands lsp-java-enable
  :init
  (setq lsp-java-save-action-organize-imports nil)
  (add-hook 'java-mode-hook #'gmacs/lsp-java-enable))

(use-package dap-java
  :ensure nil
  :after lsp-java)

(provide 'java-lang)
;;; java-lang.el ends here
