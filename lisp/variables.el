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

(defvar gmacs/evil-base-cursor-height 500
  "set the cursor height to be used, note
anything over the current line height
will be capped at the current line height
set by (line-pixel-height)")

(defvar gmacs/use-line-height-cursor t
  "enable or disable fixed cursor height")

(defvar gmacs/evil-total-cursor-height nil
  "helper var which holds calculated total cursor height")

(provide 'variables)
;;; variables.el ends here
