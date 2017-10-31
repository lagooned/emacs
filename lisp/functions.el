;;; functions.el --- describe custom functions       -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: gmacs, lisp, custom, config, minor-mode, tabify

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

(defvar my/evil-cursor-height 15
  "set the cursor height to be used across all evil")

(defun my/load-config ()
  "load init.el"
  (interactive)
  (save-some-buffers)
  (load-file "~/.emacs.d/init.el")
  (revert-buffer t t))

(defun my/force-buffer-backup ()
  "Make a special /per session/ backup at the first save of each macs session."
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . "~/.emacs.d/backup/session")))
          (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))
(add-hook 'before-save-hook 'my/force-buffer-backup)

(defvar my/auto-minor-mode-alist ()
  "Alist of filename patterns vs correpsonding minor mode functions, see
  `auto-mode-alist' All elements of this alist are checked, meaning you can
  enable multiple minor modes for the same regexp.")

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
(add-hook 'find-file-hook 'my/enable-minor-mode-based-on-extension)

(defun my/untabify-except-makefiles ()
  "Replace tabs with spaces except in makefiles."
  (unless (derived-mode-p 'makefile-mode)
    (untabify (point-min) (point-max))))

(defun my/cleanup-buffer ()
  (interactive)
  (my/untabify-except-makefiles)
  (delete-trailing-whitespace))

(defun my/counsel-rg-region (beg end)
  "optionally run ripgrep on region"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list nil nil)))
  (my/opt-region-helper #'counsel-rg beg end))

(defun my/counsel-git-region (beg end)
  "optionally run counsel-git on region"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list nil nil)))
  (my/opt-region-helper #'counsel-git beg end))

(defun my/swiper-region (beg end)
  "optionally run swiper on region"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list nil nil)))
  (my/opt-region-helper #'swiper beg end))

(defun my/opt-region-helper (func beg end)
  "run func on the region"
  (if (and beg end)
      (progn (deactivate-mark)
             (funcall func (buffer-substring-no-properties beg end)))
    (funcall func)))

(defun my/swiper-thing ()
  "swiper current word or line"
  (interactive
   (if (word-at-point) (swiper (word-at-point))
     (swiper (sentence-at-point)))))

;; cleanup on save
;; (add-hook 'before-save-hook 'my/cleanup-buffer)

(provide 'functions)
;;; functions.el ends here
