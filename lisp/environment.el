;;; environment.el --- emacs per-environment config  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: faces

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

;;; Code:

;; preferred fonts list
(let ((fonts '("Input"
               "Source Code Pro"
               "Monaco"
               "Deja Vu Sans Mono"
               "Consolas"
               "Monospace")))
  (dolist (font (reverse fonts) t)
    (if (member font (font-family-list))
        (set-face-attribute 'default nil :family font))))

;; osx
(when (eq system-type 'darwin)
  (setq mac-pass-command-to-system nil)
  (set-face-attribute 'default nil :weight 'bold)
  (set-face-attribute 'default nil :height 130)
  (unless (display-graphic-p) (menu-bar-mode -1))
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra"))
  (setenv "SHELL" "/bin/zsh"))

;; linux
(when (eq system-type 'gnu/linux)
  (set-face-attribute 'default nil :weight 'normal)
  (set-face-attribute 'default nil :height 100)
  (if (display-graphic-p) (menu-bar-mode -1))
  (setenv "SHELL" "/bin/bash"))

;; win
(when (eq system-type 'windows-nt)
  (set-face-attribute 'default nil :weight 'normal)
  (set-face-attribute 'default nil :height 105)
  (if (display-graphic-p) (menu-bar-mode -1)))

(provide 'environment)
;;; environment.el ends here
