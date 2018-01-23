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

(defun gmacs/load-config ()
  "load init.el"
  (interactive)
  (save-some-buffers)
  (load-file "~/.emacs.d/init.el")
  (revert-buffer t t))

(defun gmacs/force-buffer-backup ()
  "Make a special per session and per save backup at the
first save of each gmacs session."
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist
           '(("" . "~/.emacs.d/backup/session")))
          (kept-new-versions 3))
      (backup-buffer)))
  (let ((buffer-backed-up nil))
    (backup-buffer)))
(add-hook 'before-save-hook 'gmacs/force-buffer-backup)

(defun gmacs/check-large-file ()
  "Check if the buffer's file is large (see `gmacs/large-file-size'). If so, ask
for confirmation to open it literally (read-only, disabled undo and in
fundamental-mode) for performance sake."
  (let* ((filename (buffer-file-name))
         (size (nth 7 (file-attributes filename))))
    (when (and (not (memq major-mode gmacs/large-file-modes-list))
               size (> size (* 1024 1024 gmacs/large-file-size))
               (y-or-n-p
                (format (concat "%s is a large file, open literally to "
                                "avoid performance issues?")
                        (file-relative-name filename))))
      (setq buffer-read-only t)
      (buffer-disable-undo)
      (fundamental-mode))))

(add-hook 'find-file-hook #'gmacs/check-large-file)

(defvar gmacs/auto-minor-mode-alist ()
  "Alist of filename patterns vs correpsonding minor mode functions, see
  `auto-mode-alist' All elements of this alist are checked, meaning you can
  enable multiple minor modes for the same regexp.")

(defun gmacs/enable-minor-mode-based-on-extension ()
  "check file name against gmacs/auto-minor-mode-alist to enable minor modes the
checking happens for all pairs in gmacs/auto-minor-mode-alist"
  (when buffer-file-name
    (let ((name buffer-file-name)
          (remote-id (file-remote-p buffer-file-name))
          (alist gmacs/auto-minor-mode-alist))
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
(add-hook 'find-file-hook 'gmacs/enable-minor-mode-based-on-extension)

(defun gmacs/untabify-except-makefiles ()
  "Replace tabs with spaces except in makefiles."
  (unless (derived-mode-p 'makefile-mode)
    (untabify (point-min) (point-max))))

(defun gmacs/switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

(defun gmacs/cleanup-file ()
  (interactive)
  (gmacs/untabify-except-makefiles)
  (delete-trailing-whitespace))

(defun gmacs/counsel-rg-region ()
  "optionally run ripgrep on region"
  (interactive)
  (gmacs/opt-region-helper 'counsel-rg))

(defun gmacs/counsel-git-region ()
  "optionally run counsel-git on region"
  (interactive)
  (gmacs/opt-region-helper 'counsel-git))

(defun gmacs/counsel-projectile-region ()
  "optionally run counsel-projectile on region"
  (interactive)
  (gmacs/opt-region-helper 'gmacs/counsel-projectile))

(defun gmacs/counsel-projectile-find-dir-region ()
  "optionally run counsel-find-dir on region"
  (interactive)
  (gmacs/opt-region-helper 'gmacs/counsel-projectile-find-dir))

(defun gmacs/opt-region-helper (func)
  "add region to kill ring and run func with optional region arg"
  (if (use-region-p)
      (let ((string (buffer-substring-no-properties
                     (region-beginning) (region-end))))
        (progn (kill-new string)
               (deactivate-mark)
               (funcall-interactively func string)))
    (funcall-interactively func)))

(defun gmacs/swiper-region-thing (beg end)
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list nil nil)))
  (if (and beg end)
      (progn (deactivate-mark)
             (swiper (buffer-substring-no-properties beg end)))
    (if (word-at-point) (swiper (word-at-point))
      (error "No region or thing selected"))))

(defun gmacs/xref-find-apropos-symbol ()
  (interactive)
  (if (symbol-at-point)
      (xref-find-apropos (symbol-name (symbol-at-point)))
    (error "No symbol selected")))

(defun gmacs/ripgrep-regexp-git (regexp &optional args)
  "ripgrep with `regexp' from the nearest git project directory.
`args' provides ripgrep command line arguments."
  (interactive
   (list (read-from-minibuffer
          "rg buffer: "
          (if (use-region-p)
              (let ((string (buffer-substring-no-properties
                             (region-beginning) (region-end))))
                (progn (kill-new string) (deactivate-mark) string))))))
  (let ((directory (locate-dominating-file default-directory ".git")))
    (if (not directory)
        (message "not in a git project: using default-directory")
      (setq default-directory directory))
    (compilation-start
     (mapconcat 'identity
                (append (list ripgrep-executable)
                        ripgrep-arguments
                        args
                        '("--no-heading --vimgrep -ni ")
                        (when ripgrep-highlight-search '("--color=always"))
                        (list (shell-quote-argument regexp) ".")) " ")
     'ripgrep-search-mode)))

(defun gmacs/org-link-jump ()
  "push marker stack and follow org link"
  (interactive)
  (let ((org-link-frame-setup
         '((file . (lambda (args)
                     (progn (xref-push-marker-stack)
                            (find-file args)))))))
    (org-open-at-point)))

(defun gmacs/org-link-jump-back ()
  "pop marker stack to jump back to source org link"
  (interactive)
  (xref-pop-marker-stack))

(defun gmacs/counsel-projectile (&optional initial-input)
  "Jump to a buffer or file in the current project.
If not inside a project, call `counsel-projectile-switch-project'."
  (interactive)
  (if (not (projectile-project-p))
      (counsel-projectile-switch-project)
    (ivy-read (projectile-prepend-project-name "Load buffer or file: ")
              (counsel-projectile--project-buffers-and-files)
              :initial-input initial-input
              :matcher #'counsel-projectile--matcher
              :require-match t
              :action counsel-projectile-action
              :caller 'counsel-projectile)))

(defun gmacs/counsel-projectile-find-dir (&optional initial-input)
  "Jump to a directory in the current project."
  (interactive)
  (if (not (projectile-project-p))
      (counsel-projectile-switch-project)
    (ivy-read (projectile-prepend-project-name "Find dir: ")
              (counsel-projectile--project-directories)
              :initial-input initial-input
              :require-match t
              :action counsel-projectile-find-dir-action
              :caller 'counsel-projectile-find-dir)))

(defun gmacs/toggle-spelling ()
  (interactive)
  (ispell-set-spellchecker-params)
  (if (bound-and-true-p flyspell-mode)
      (flyspell-mode 0)
    (if (use-region-p)
        (save-excursion
          (deactivate-mark)
          (flyspell-large-region (region-beginning) (region-end))
          (flyspell-mode +1))
      (save-excursion
        (flyspell-large-region (point-min) (point-max))
        (flyspell-mode +1)))))

(defun gmacs/unhighlight-all ()
  (interactive)
  (unhighlight-regexp t)
  (hi-lock-mode 0))

(defun gmacs/switch-to-scratch-buffer ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(provide 'functions)
;;; functions.el ends here
