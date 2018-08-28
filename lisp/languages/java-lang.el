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

(defun gmacs/lsp-java-enable ()
  (make-variable-buffer-local 'company-backends)
  (push 'company-lsp company-backends)
  (flycheck-mode 1)
  (lsp-java-enable)
  (evil-leader/set-key-for-mode 'java-mode
    "m r" 'lsp-rename
    "m R" 'lsp-restart-workspace
    "m f" 'lsp-format-buffer
    "m h" 'lsp-describe-thing-at-point
    "m H" 'lsp-highlight-symbol-at-point
    "m o" 'lsp-java-organize-imports
    "m b" 'lsp-java-build-project))

(use-package lsp-java
  :commands lsp-java-enable
  :init
  (setq lsp-java-save-action-organize-imports nil)
  (add-hook 'java-mode-hook #'gmacs/lsp-java-enable))

(provide 'java-lang)
;;; java-lang.el ends here
