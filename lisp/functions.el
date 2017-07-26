;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM FUNCTIONS ;;
;;;;;;;;;;;;;;;;;;;;;;

;; load config
(defun my/load-config ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;; create my/auto-minor-mode-alist for files
(defvar my/auto-minor-mode-alist ()
  "Alist of filename patterns vs correpsonding minor mode functions, see `auto-mode-alist'
All elements of this alist are checked, meaning you can enable multiple minor modes for the same regexp.")

(defun my/enable-minor-mode-based-on-extension ()
  "check file name against my/auto-minor-mode-alist to enable minor modes
the checking happens for all pairs in my/auto-minor-mode-alist"
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

;; cleanup on save
(add-hook 'before-save-hook 'my/cleanup-buffer)
