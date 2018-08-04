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

(use-package css-mode
  :config
  (defun css--fontify-region (start end &optional loudly)
    "Fontify a CSS buffer between START and END.
START and END are buffer positions."
    (let ((extended-region (font-lock-default-fontify-region start end loudly)))
      (when css-fontify-colors
        (when (and (consp extended-region)
		           (eq (car extended-region) 'jit-lock-bounds))
	      (setq start (cadr extended-region))
	      (setq end (cddr extended-region)))
        (save-excursion
	      (let ((case-fold-search t))
	        (goto-char start)
	        (while (re-search-forward css--colors-regexp end t)
	          ;; Skip comments and strings.
	          (unless (nth 8 (syntax-ppss))
	            (let* ((start (match-beginning 0))
                       (color (css--compute-color start (match-string 0))))
		          (when color
		            (with-silent-modifications
                      ;; why the fuck is this face hardcoded
                      ;; the border is ugly af
		              (add-text-properties
		               start (point)
		               (list 'face (list :background color
				                         :foreground (css--contrasty-color color))))))))))))
      extended-region)))

(use-package mmm-mode
  :init
  (setq mmm-submode-decoration-level 0))

(use-package vue-mode
  :config
  (add-hook 'vue-mode-hook 'turn-off-evil-matchit-mode))

(provide 'web-lang)
;;; web-lang.el ends here
