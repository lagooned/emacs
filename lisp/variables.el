;;; variables.el --- gmacs vars                      -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: gmacs, vars, init

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

;; gmacs config and helper vars

;;; Code:

(defvar gmacs/evil-base-cursor-height 999
  "set the cursor height to be used, note
anything over the current line height
will be capped at the current line height
set by (line-pixel-height)")

(defvar gmacs/use-line-height-cursor t
  "enable or disable line height cursor")

(defvar gmacs/large-file-size 1
  "Size (in MB) above which the user will be prompted to open the file literally
to avoid performance issues. Opening literally means that no major or minor
modes are active and the buffer is read-only.")

(defvar gmacs/large-file-modes-list
  '(archive-mode
    tar-mode
    jka-compr
    git-commit-mode
    image-mode
    doc-view-mode
    doc-view-mode-maybe
    ebrowse-tree-mode
    pdf-view-mode)
  "Major modes that `gmacs/check-large-file' will ignore.")

(provide 'variables)
;;; variables.el ends here
