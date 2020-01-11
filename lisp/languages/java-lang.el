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
  :init
  (when (je/java-lsp-deps-p)
    (add-hook 'java-mode-hook #'lsp)
    (add-hook 'java-mode-hook #'je/configure-company-lsp-backends))
  :commands java-mode)

(use-package lsp-java
  :after lsp company-lsp
  :when (je/java-lsp-deps-p)
  :init
  (setq
   lsp-java-autobuild-enabled nil
   lsp-java-format-enabled nil
   lsp-java-inhibit-message t
   lsp-java-folding-range-enabled nil
   lsp-java-format-on-type-enabled nil
   lsp-java-references-code-lens-enabled nil)
  :config
  (evil-leader/set-key-for-mode 'java-mode
    "m m" 'lsp-execute-code-action
    "m o" 'lsp-java-organize-imports
    "m r s" 'lsp-java-convert-to-static-import
    "m r m" 'lsp-java-extract-method
    "m r c" 'lsp-java-extract-to-constant
    "m r l" 'lsp-java-extract-to-local-variable)
  (which-key-add-major-mode-key-based-replacements 'java-mode
    "SPC m r" "refactor"))

(use-package groovy-mode
  :commands groovy-mode)

(provide 'java-lang)
;;; java-lang.el ends here
