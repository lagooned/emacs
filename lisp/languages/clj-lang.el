;;; clj-lang.el --- Je/Emacs Clojure language tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: jeemacs, languages, config, clojure

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

;; Je/Emacs clojure language tools.

;;; Code:

(use-package clojure-mode
  :after evil-leader
  :commands clojure-mode
  :init
  (when (executable-find "clojure-lsp")
    (add-hook 'clojure-mode-hook #'lsp))
  :config
  (when (je/cider-deps-p)
    (evil-leader/set-key-for-mode 'clojure-mode
      "j" 'cider-jack-in
      "e" 'cider-eval-last-sexp
      "r" 'cider-switch-to-repl-buffer)))

(use-package cider
  :after evil-leader
  :commands
  cider-jack-in
  cider-eval-last-sexp
  cider-switch-to-repl-buffer
  :when
  (je/cider-deps-p)
  :hook
  ((cider-repl-mode . je/cider-repl-mode-setup)
   (cider-repl-mode . eldoc-mode))
  :init
  (setq cider-repl-display-help-banner 'nil)
  :config
  (evil-leader/set-key-for-mode 'cider-repl-mode
    "r" 'cider-switch-to-last-clojure-buffer
    "m q" 'cider-quit))

(use-package clj-lsp
  :after lsp company-lsp
  :when (executable-find "clojure-lsp")
  :config
  (add-to-list
   'lsp-language-id-configuration
   (clojure-mode . "clojure")))

(provide 'clj-lang)
;;; clj-lang.el ends here
