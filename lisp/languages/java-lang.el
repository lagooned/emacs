;;; java-lang.el --- jeemacs java language tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: jeemacs, config, languages, java

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

(use-package java-mode
  :hook (java-mode . lsp)
  :commands java-mode)

(use-package lsp-java
  :after lsp
  :when (and (not (eq system-type 'windows-nt))
             (executable-find "java"))
  :init
  (setq lsp-java-autobuild-enabled nil
        lsp-java-format-enabled nil
        lsp-java-inhibit-message t
        lsp-java-completion-overwrite nil
        lsp-java-folding-range-enabled nil
        lsp-java-format-on-type-enabled nil
        lsp-java-import-gradle-enabled nil
        lsp-java-import-maven-enabled t
        lsp-java-references-code-lens-enabled nil
        lsp-java-save-actions-organize-imports nil
        lsp-java-selection-enabled nil))

(use-package groovy-mode
  :commands groovy-mode)

(provide 'java-lang)
;;; java-lang.el ends here
