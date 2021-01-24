;;; scala-lang.el --- Je/Emacs scala language tools   -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: languages, tools, convenience, local, lisp, scala, abbrev

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

;; Je/Emacs scala language tools.

;;; Code:

(use-package scala-mode
  :interpreter ("scala" . scala-mode))

(use-package sbt-mode
  :when (executable-find "sbt")
  :commands sbt-start sbt-command
  :config (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package lsp-metals
  :when (and (executable-find "metals-emacs")
             (executable-find "bloop"))
  :hook (scala-mode . lsp))

(provide 'scala-lang)
;;; scala-lang.el ends here
