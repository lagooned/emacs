;;; xml-lang.el --- Gmacs XML tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: gmacs, config, languages, xml

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

;; Gmacs xml tools.

;;; Code:

(use-package emmet-mode
  :diminish "emm"
  :commands emmet-mode
  :hook ((sgml-mode . emmet-mode)
         (web-mode . emmet-mode)
         (css-mode . emmet-mode)
         (nxml-mode . emmet-mode)
         (emmet-mode . gmacs/evil-emmet-mode-setup)))

(use-package nxml-mode
  :init
  (setq nxml-child-indent 4))

(use-package sgml-mode
  :init
  (setq sgml-basic-offset 4))

(provide 'xml-lang)
;;; xml-lang.el ends here
