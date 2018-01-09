;;; evil-init.el --- evil preloads                   -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: gmacs, evil, init

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

;; this file is executed by use-package before evil loads

;;; Code:

(setq evil-move-cursor-back nil)
(setq evil-want-C-u-scroll t)

(if gmacs/use-line-height-cursor
    ;; todo make a minor mode that uses the line-pixel-height 
    (let ((height (eval gmacs/evil-base-cursor-height)))
      ;; fixed height cursors
      (setq evil-normal-state-cursor `("#dd00dd" (hbar . ,(eval height))))
      (setq evil-insert-state-cursor `("#00e000" (hbar . ,(eval height))))
      (setq evil-visual-state-cursor `("#ff8800" (hbar . ,(eval height))))
      (setq evil-emacs-state-cursor `("#ff0000" (hbar . ,(eval height))))
      (setq evil-motion-state-cursor `("#0000ff" (hbar . ,(eval height))))
      (setq evil-replace-state-cursor `("#00bbbb" (hbar . ,(eval height))))
      (setq evil-operator-state-cursor `("#ff66ff" (hbar . ,(eval height)))))
  (progn
    (setq evil-normal-state-cursor `("#dd00dd" box))
    (setq evil-insert-state-cursor `("#00e000" box))
    (setq evil-visual-state-cursor `("#ff8800" box))
    (setq evil-emacs-state-cursor `("#ff0000" box))
    (setq evil-motion-state-cursor `("#0000ff" box))
    (setq evil-replace-state-cursor `("#00bbbb" box))
    (setq evil-operator-state-cursor `("#ff66ff" box))))

;; initial states
(add-hook 'with-editor-mode-hook 'evil-insert-state)
(add-hook 'term-mode-hook 'evil-emacs-state)
(add-hook 'ansi-term-mode-hook 'evil-emacs-state)

(provide 'evil-init)
;;; evil-init.el ends here
