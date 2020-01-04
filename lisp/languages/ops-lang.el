;;; ops-lang.el --- jeemacs ops tools                  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: jeemacs, config, languages, ops

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

;; Gmacs Ops Tools

;;; Code:

(use-package apache-mode
  :commands apache-mode)

(use-package dockerfile-mode
  :commands dockerfile-mode
  :init
  (add-to-list
   'auto-mode-alist
   '("Dockerfile\\'" . dockerfile-mode)))

(use-package feature-mode
  :commands feature-mode
  :init
  (add-to-list
   'auto-mode-alist
   '("\.feature$" . feature-mode)))

(use-package puppet-mode
  :commands puppet-mode)

(use-package restclient
  :after evil-leader
  :commands restclient-mode
  :init
  (add-to-list
   'auto-mode-alist
   '("\\.rest\\'" . restclient-mode))
  :config
  (evil-leader/set-key-for-mode 'restclient-mode
    "e" 'restclient-http-send-current))

(use-package yaml-mode
  :commands yaml-mode)

(provide 'ops-lang)
;;; ops-lang.el ends here
