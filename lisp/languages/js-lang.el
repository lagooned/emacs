;;; js-lang.el --- javascript langauge tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Jared Matthew Engler

;; Author: Jared Matthew Engler <jared@MacBook-Pro>
;; Keywords: tools, languages, lisp, help

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

;; Gmacs javascript langauge tools.

;;; Code:

(use-package js2-mode
  :commands
  js2-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package xref-js2
  :after js2-mode
  :config
  (add-hook
   'js2-mode-hook
   (lambda ()
     (add-hook
      'xref-backend-functions
      #'xref-js2-xref-backend nil t))))

(use-package smart-jump
  :config
  (smart-jump-register :modes '(js2-mode)
                       :jump-fn 'xref-find-definitions
                       :pop-fn 'pop-tag-mark
                       :should-jump t
                       :heuristic 'error
                       :async nil))

(use-package js2-refactor
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode))

(provide 'js-lang)
;;; js-lang.el ends here
