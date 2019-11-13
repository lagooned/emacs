;;; clj-lang.el --- Gmacs Clojure language tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Jared M. Engler

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

;; Gmacs clojure language tools.

;;; Code:

(use-package clojure-mode)

(use-package cider
  :when
  (and (executable-find "clj")
       (executable-find "lein"))
  :hook
  ((cider-repl-mode . jeemacs/cider-repl-mode-setup)
   (cider-repl-mode . eldoc-mode))
  :init
  (setq cider-repl-display-help-banner 'nil)
  :config
  (evil-leader/set-key-for-mode 'cider-repl-mode
    "m q" 'cider-quit)
  (evil-leader/set-key-for-mode 'clojure-mode
    "C-e" 'jeemacs/cider-move-eol-eval-last-sexp
    "e" 'cider-eval-last-sexp))

(provide 'clj-lang)
;;; clj-lang.el ends here
