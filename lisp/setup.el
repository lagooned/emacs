;;; setup.el --- jeemacs pre-config setup  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: jeemacs, config, setup, binds

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

;; Gmacs pre-config setup

;;; Code:

;; unbinds to clean up global bindspace
(global-unset-key (kbd "C-k"))
(global-unset-key (kbd "C-w"))
(global-unset-key (kbd "C-s"))
(global-unset-key (kbd "C-y"))
(global-unset-key (kbd "M-j"))
(global-unset-key (kbd "M-a"))
(global-unset-key (kbd "M-b"))
(global-unset-key (kbd "M-e"))
(global-unset-key (kbd "M-w"))
(global-unset-key (kbd "M-d"))
(global-unset-key (kbd "M-m"))
(global-unset-key (kbd "M-k"))
(global-unset-key (kbd "M-u"))
(global-unset-key (kbd "M-v"))
(global-unset-key (kbd "M-l"))
(global-unset-key (kbd "M-i"))
(global-unset-key (kbd "M-c"))
(global-unset-key (kbd "M-t"))
(global-unset-key (kbd "M-h"))
(global-unset-key (kbd "M-r"))
(global-unset-key (kbd "M-m"))
(global-unset-key (kbd "M-."))
(global-unset-key (kbd "M-^"))
(global-unset-key (kbd "M-,"))
(global-unset-key (kbd "M-;"))
(global-unset-key (kbd "M-?"))
(global-unset-key (kbd "M-z"))
(global-unset-key (kbd "M-!"))
(global-unset-key (kbd "M-@"))
(global-unset-key (kbd "M-&"))
(global-unset-key (kbd "M->"))
(global-unset-key (kbd "M-<"))
(global-unset-key (kbd "C-x C-u"))
(global-unset-key (kbd "C-x C-l"))
(global-unset-key (kbd "C-M-j"))
(global-unset-key (kbd "C-M-k"))
(global-unset-key (kbd "C-M-v"))
(global-unset-key (kbd "C-M-h"))
(global-unset-key (kbd "C-M-u"))
(global-unset-key (kbd "C-M-d"))
(global-unset-key (kbd "C-M-f"))
(global-unset-key (kbd "C-M-e"))
(global-unset-key (kbd "C-M-b"))
(global-unset-key (kbd "C-M-a"))
(global-unset-key (kbd "C-M-t"))
(global-unset-key (kbd "C-M-l"))
(global-unset-key (kbd "C-M-n"))
(global-unset-key (kbd "C-M-p"))
(global-unset-key (kbd "C-M-/"))
(global-unset-key (kbd "C-M-."))
(global-unset-key (kbd "M-o"))
(global-unset-key (kbd "C-M-o"))

(provide 'setup)
;;; setup.el ends here
