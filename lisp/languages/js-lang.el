;;; js-lang.el --- gmacs javascript langauge tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Jared Matthew Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: gmacs, config, languages, javascript

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

;; Gmacs javascript language tools.

;;; Code:

(use-package json-mode
  :commands json-mode)

(use-package rjsx-mode
  :commands
  rjsx-minor-mode
  rjsx-mode
  :init
  (add-hook 'js2-mode-hook 'rjsx-minor-mode)
  (add-hook 'js2-mode-hook 'emmet-mode)
  (add-hook 'js2-mode-hook (lambda () (setq-local emmet-expand-jsx-className? t))))

(use-package js2-mode
  :commands js2-mode
  :init
  (setq js2-strict-missing-semi-warning nil
        js2-missing-semi-one-line-override nil)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package xref-js2
  :after js2-mode
  :init
  (setq xref-js2-ignored-dirs nil)
  :config
  (if (executable-find "ag")
      (add-hook 'js2-mode-hook #'gmacs/add-xref-js2-xref-backend)))

(use-package js2-refactor
  :after js2-mode
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode))

(evil-leader/set-key-for-mode 'js2-mode
  "m f s" 'js2-mode-toggle-hide-functions
  "m f c" 'js2-mode-toggle-hide-comments
  "m f f" 'js2-mode-toggle-element
  "m v" 'js2r-rename-var)

(which-key-add-key-based-replacements
  "SPC m f" "fold")

(provide 'js-lang)
;;; js-lang.el ends here
