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

;;; Commentary:

;; Gmacs custom function definitions.

;;; Code:

(defun void ()
  "No-op."
  (interactive))

(defun gmacs/load-config ()
  "Load init.el."
  (interactive)
  (save-some-buffers)
  (load-file "~/.emacs.d/init.el")
  (revert-buffer t t))

(defun gmacs/open-init-config ()
  "Open init.el."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun gmacs/open-variables-config ()
  "Open variables.el."
  (interactive)
  (find-file "~/.emacs.d/lisp/variables.el"))

(defun gmacs/open-functions-config ()
  "Open functions.el."
  (interactive)
  (find-file "~/.emacs.d/lisp/functions.el"))

(defun gmacs/open-leader-config ()
  "Open leader-config.el."
  (interactive)
  (find-file "~/.emacs.d/lisp/leader-config.el"))

(defun gmacs/open-global-config ()
  "Open global.el."
  (interactive)
  (find-file "~/.emacs.d/lisp/global.el"))

(defun gmacs/open-environment-config ()
  "Open environment.el."
  (interactive)
  (find-file "~/.emacs.d/lisp/environment.el"))

(defun gmacs/open-evil-config ()
  "Open evil-config.el."
  (interactive)
  (find-file "~/.emacs.d/lisp/evil-config.el"))

(defun gmacs/open-packages-config ()
  "Open packages.el."
  (interactive)
  (find-file "~/.emacs.d/lisp/packages.el"))

(defun gmacs/open-language-config ()
  "Open languages/packages-lang.el."
  (interactive)
  (find-file "~/.emacs.d/lisp/languages/packages-lang.el"))

(defun gmacs/force-buffer-backup ()
  "Make a special per session and per save backup \
at the first save of each gmacs session."
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
  "Check if the buffer's file is large (see `gmacs/large-file-size').
If so, ask for confirmation to open it literally (read-only, disabled
undo and in `fundamental-mode' for performance sake."
  (let* ((filename (buffer-file-name))
         (size (nth 7 (file-attributes filename))))
    (defvar gmacs/large-file-size)
    (defvar gmacs/large-file-modes-list)
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
  "Alist of filename patterns vs correpsonding minor mode functions, see \
`auto-mode-alist' All elements of this alist are checked, meaning you can
enable multiple minor modes for the same regexp.")

(defun gmacs/enable-minor-mode-based-on-extension ()
  "Check file name against gmacs/auto-minor-mode-alist to enable minor \
modes the checking happens for all pairs in `gmacs/auto-minor-mode-alist'."
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
  "Remove tabs and trailing whitespace from buffer."
  (interactive)
  (gmacs/untabify-except-makefiles)
  (delete-trailing-whitespace))

(defun gmacs/counsel-rg-region ()
  "Optionally run ripgrep on region."
  (interactive)
  (gmacs/opt-region-helper 'counsel-rg))

(defun gmacs/counsel-git-region ()
  "Optionally run counsel-git on region."
  (interactive)
  (gmacs/opt-region-helper 'gmacs/counsel-git-projectile))

(defun gmacs/counsel-projectile-find-dir-region ()
  "Optionally run counsel-find-dir on region."
  (interactive)
  (gmacs/opt-region-helper 'gmacs/counsel-projectile-find-dir))

(defun gmacs/opt-region-helper (func)
  "Add region to kill ring and run `FUNC' with optional region arg."
  (if (use-region-p)
      (let ((string (buffer-substring-no-properties
                     (region-beginning) (region-end))))
        (progn (kill-new string)
               (deactivate-mark)
               (funcall-interactively func string)))
    (funcall-interactively func)))

(defun gmacs/swiper-region-thing ()
  "Call swiper on the selected region or thing under cursor."
  (interactive)
  (if (use-region-p)
      (progn (deactivate-mark)
             (swiper (buffer-substring-no-properties
                      (region-beginning) (region-end))))
    (if (word-at-point) (swiper (word-at-point))
      (error "No region or thing selected"))))

(defun gmacs/xref-find-apropos-symbol ()
  "X-ref made to be used with smart jump."
  (interactive)
  (if (symbol-at-point)
      (xref-find-apropos (symbol-name (symbol-at-point)))
    (message "No symbol selected")))

(defun gmacs/ripgrep-regexp-git (regexp &optional args)
  "Ripgrep with `REGEXP' from the nearest git project directory.
`ARGS' provides ripgrep command line arguments."
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
    (defvar ripgrep-executable)
    (defvar ripgrep-arguments)
    (defvar ripgrep-highlight-search)
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
  "Push marker stack and follow org link."
  (interactive)
  (defvar org-link-frame-setup)
  (let ((org-link-frame-setup
         '((file . (lambda (args)
                     (progn (xref-push-marker-stack)
                            (find-file args)))))))
    (call-interactively #'org-open-at-point)))

(defun gmacs/org-link-jump-back ()
  "Pop marker stack to jump back to source org link."
  (interactive)
  (xref-pop-marker-stack))

(defun gmacs/counsel-git-projectile (&optional initial-input)
  "Find file in the current Git repository with initial input `INITIAL-INPUT'."
  (interactive)
  (defvar counsel-require-program)
  (defvar counsel-prompt-function)
  (defvar counsel-git-cmd)
  (defvar counsel--git-dir)
  (counsel-require-program (car (split-string counsel-git-cmd)))
  (ivy-set-prompt 'counsel-git counsel-prompt-function)
  (setq counsel--git-dir (expand-file-name
                          (counsel-locate-git-root)))
  (let* ((default-directory counsel--git-dir)
         (cands (split-string
                 (shell-command-to-string counsel-git-cmd) "\n" t)))
    (ivy-read (projectile-prepend-project-name "find file") cands
              :initial-input initial-input
              :action #'counsel-git-action
              :caller 'counsel-git)))

(defun gmacs/counsel-projectile-find-dir (&optional initial-input)
  "Jump to a directory in the current project with initial input `INITIAL-INPUT'."
  (interactive)
  (defvar counsel-projectile-find-dir-action)
  (if (not (projectile-project-p))
      (error "Not in a git repository")
    (ivy-read (projectile-prepend-project-name "find dir: ")
              (counsel-projectile--project-directories)
              :initial-input initial-input
              :require-match t
              :action counsel-projectile-find-dir-action
              :caller 'counsel-projectile-find-dir)))

(defun gmacs/toggle-spelling ()
  "Toggle flyspell."
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
  "Unhighlight all currently highlighted symbols and \
disable `hi-lock-mode'."
  (interactive)
  (unhighlight-regexp t)
  (hi-lock-mode 0))

(defun gmacs/switch-to-scratch-buffer ()
  "Switch to initial scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun gmacs/switch-to-messages-buffer ()
  "Switch to Messages buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun gmacs/eshell-send-eof ()
  "Send EOF to Eshell with newline."
  (interactive)
  (newline)
  (eshell-send-eof-to-process))

(defun gmacs/projectile-root-dir ()
  "Jump to the root directory of the current project."
  (interactive)
  (if (not (projectile-project-p))
      (error "Not in a git repository")
    (dired (projectile-project-root))))

(defun gmacs/create-visit-dir (dir)
  "Open/create `DIR'"
  (if (file-directory-p dir)
      (dired dir)
    (progn
      (make-directory dir t)
      (dired dir))))

(defun gmacs/open-org-dir ()
  "Open ~/org."
  (interactive)
  (gmacs/create-visit-dir "~/org"))

(defun gmacs/write-startup-log ()
  (save-current-buffer
    (set-buffer "*Messages*")
    (append-to-file (point-min) (point-max) "~/.emacs.d/startup.log")))

(provide 'functions)
;;; functions.el ends here
