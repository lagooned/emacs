;;; functions.el --- describe custom functions       -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: lisp, custom, config, minor-mode, tabify

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

;;; Code:

;; load config
(defun my/load-config ()
  (interactive)
  (save-some-buffers)
  (load-file "~/.emacs.d/init.el")
  (revert-buffer t t))

;; create my/auto-minor-mode-alist for files
(defvar my/auto-minor-mode-alist ()
  "Alist of filename patterns vs correpsonding minor mode functions, see
  `auto-mode-alist' All elements of this alist are checked, meaning you can
  enable multiple minor modes for the same regexp.")

;; enable minor modes
(defun my/enable-minor-mode-based-on-extension ()
  "check file name against my/auto-minor-mode-alist to enable minor modes the
checking happens for all pairs in my/auto-minor-mode-alist"
  (when buffer-file-name
    (let ((name buffer-file-name)
          (remote-id (file-remote-p buffer-file-name))
          (alist my/auto-minor-mode-alist))
      ;; Remove backup-suffixes from file name.
      (setq name (file-name-sans-versions name))
      ;; Remove remote file name identification.
      (when (and (stringp remote-id)
                 (string-match-p (regexp-quote remote-id) name))
        (setq name (substring name (match-end 0))))
      (while (and alist (caar alist) (cdar alist))
        (if (string-match (caar alist) name)
            (funcall (cdar alist) 1))
        (setq alist (cdr alist))))))

;; hook to new buffer
(add-hook 'find-file-hook 'my/enable-minor-mode-based-on-extension)

;; untabify
(defun my/untabify-except-makefiles ()
  "Replace tabs with spaces except in makefiles."
  (unless (derived-mode-p 'makefile-mode)
    (untabify (point-min) (point-max))))

;; cleanup
(defun my/cleanup-buffer ()
  (interactive)
  (my/untabify-except-makefiles)
  (delete-trailing-whitespace))

;; rg region
(defun my/counsel-rg-region ()
  "runs counsel-rg optionally on the region"
  (interactive)
  (if (use-region-p) (counsel-rg (buffer-substring (region-beginning) (region-end)))
    (counsel-rg)))

;; git file region
(defun my/counsel-git-region ()
  "runs counsel-git optionally on the region"
  (interactive)
  (if (use-region-p) (counsel-git (buffer-substring (region-beginning) (region-end)))
    (counsel-git)))

;; cleanup on save
;; (add-hook 'before-save-hook 'my/cleanup-buffer)

(provide 'functions)
;;; functions.el ends here
