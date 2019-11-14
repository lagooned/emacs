;;; variables.el --- jeemacs vars  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: jeemacs, config, var

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

;; Gmacs custom variable definitions.

;;; Code:

(defvar jeemacs/evil-base-cursor-height #xffff
  "Set the cursor height to be used, note \
anything over the current line height \
will be capped at the current line height \
set by (line-pixel-height)")

(defvar jeemacs/large-file-size 2
  "Size (in MB) above which the user will be prompted to open the file literally \
to avoid performance issues. Opening literally means that no major or minor \
modes are active and the buffer is read-only.")

(defvar jeemacs/eshell-message "Jeeshell \\(^.^\\)\n"
  "Message shown when starting Eshell.")

(defvar jeemacs/eshell-top-prompt-regexp "^\\[.*\\].*"
  "A regexp which matches the top line in the eshell prompt.")

(defvar jeemacs/eshell-prompt-regexp "^[#$] "
  "A regexp which fully matches your eshell prompt.

This setting is important, since it affects how eshell will interpret \
the lines that are passed to it. \
If this variable is changed, all Eshell buffers must be exited and \
re-entered for it to take effect.")

(defvar jeemacs/mc-evil-prev-state nil
  "Saves the previous evil state as a string.")

(defvar jeemacs/mc-evil-mark-was-active nil
  "Saves the previous state of the mark.")

(defvar jeemacs/js2-xref-accept-ag nil
  "Determines whether or not to show the xref-ag confirmation.")

(defvar jeemacs/force-basic-grep nil
  "Determines whether or not to use basic grep.")

(defvar jeemacs/git-ls-tree-head-cmd "git ls-tree -rt HEAD"
  "Git command for showing ls-tree")

(provide 'variables)
;;; variables.el ends here
