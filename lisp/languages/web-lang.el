;;; web-lang.el --- gmacs web tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: gmacs, config, languages, web, css, vue, emmet, html

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
  :diminish "emm"
  :commands
  emmet-mode
  :init
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode))

(use-package web-mode
  :commands web-mode
  :init
  (setq web-mode-enable-auto-pairing nil
        web-mode-markup-indent-offset 4)
  (add-to-list 'auto-mode-alist '("\\.x?html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.blade.php\\'" . web-mode))
  :config
  (emmet-mode 1)
  (add-hook 'web-mode-hook (lambda () (sp-local-pair 'web-mode "<" ""))))

(use-package css-mode
  :commands
  css-mode
  :config
  (gmacs/css-mode-setup))

(use-package mmm-mode
  :init
  (setq mmm-submode-decoration-level 0))

(use-package vue-mode
  :init
  (setq vue-html-tab-width 4)
  :config
  (add-hook 'vue-mode-hook 'turn-off-evil-matchit-mode))

(provide 'web-lang)
;;; web-lang.el ends here
