;;; web-lang.el --- gmacs web tools                  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: lisp, tools, convenience

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

(use-package emmet-mode
  :commands
  emmet-mode
  :init
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook  'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode))

(use-package impatient-mode
  :init
  (defvar gmacs/auto-minor-mode-alist)
  (add-to-list
   'gmacs/auto-minor-mode-alist
   '("\\.x?html\\'" . impatient-mode)))

(use-package web-mode
  :commands web-mode
  :init
  (setq web-mode-enable-auto-pairing nil)
  (add-to-list
   'auto-mode-alist
   '("\\.x?html\\'" . web-mode))
  (add-to-list
   'auto-mode-alist
   '("\\.blade.php\\'" . web-mode))
  :config
  (emmet-mode 1)
  (add-hook 'web-mode-hook (lambda () (sp-local-pair 'web-mode "<" ""))))

(provide 'web-lang)
;;; web-lang.el ends here
