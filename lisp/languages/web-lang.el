;;; web-lang.el --- jeemacs web tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: jeemacs, config, languages, web, css, vue, emmet, html

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

;; Gmacs web tools.

;;; Code:

(use-package web-mode
  :commands web-mode
  :hook (web-mode . je/web-mode-setup)
  :init
  (setq web-mode-enable-auto-pairing nil
        web-mode-markup-indent-offset 4
        web-mode-enable-css-colorization nil)
  (add-to-list 'auto-mode-alist '("\\.x?html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.blade.php\\'" . web-mode)))

(use-package css-mode
  :commands css-mode
  :init
  (setq css-fontify-colors nil))

(provide 'web-lang)
;;; web-lang.el ends here
