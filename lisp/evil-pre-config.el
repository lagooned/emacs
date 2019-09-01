;;; evil-pre-config.el --- Gmacs packages before evil mode start  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: gmacs, config, evil

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

;; Packages to load before (evil-mode 1)

;;; Code:

(use-package evil-leader
  :config
  (load "leader-config"))

(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1))

(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode 1))

(provide 'evil-pre-config)
;;; before-evil-config.el ends here
