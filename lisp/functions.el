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

(require 'cl)
(require 'curry-compose)

(defun void ()
  "Interactive No-op."
  (interactive))

(defun gmacs/emacs-startup-hook ()
  (gmacs/reset-gc-threshold-percentage)
  (gmacs/write-startup-log)
  (kill-buffer "*Messages*")
  (setq default-directory "~/.emacs.d/"))

(defun gmacs/reset-gc-threshold-percentage ()
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1))

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

(defun gmacs/open-custom-config ()
  "Open .custom.el."
  (interactive)
  (find-file "~/.emacs.d/.custom.el"))

(defun gmacs/open-startup-log ()
  "Open startup.log."
  (interactive)
  (find-file "~/.emacs.d/startup.log")
  (end-of-buffer))

(defun gmacs/minibuffer-fringe-setup ()
  (set-window-fringes (minibuffer-window) 0 0 nil)
  (add-hook 'minibuffer-setup-hook #'gmacs/disable-minibuffer-fringe))

(defun gmacs/disable-minibuffer-fringe ()
  (set-window-fringes (minibuffer-window) 0 0 nil))

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

(defun gmacs/check-large-file ()
  "Check if the buffer's file is large (see `gmacs/large-file-size').
If so, ask for confirmation to open it literally (read-only, disabled
undo and in `fundamental-mode' for performance sake."
  (let* ((filename (buffer-file-name))
         (size (nth 7 (file-attributes filename))))
    (defvar gmacs/large-file-size)
    (defvar gmacs/large-file-modes-list)
    (when (and (not (memq major-mode gmacs/large-file-modes-ignore-list))
               size (> size (* 1024 1024 gmacs/large-file-size))
               (y-or-n-p
                (format (concat "%s is a large file, open literally to "
                                "avoid performance issues?")
                        (file-relative-name filename))))
      (setq buffer-read-only t)
      (buffer-disable-undo)
      (fundamental-mode))))

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

(defun gmacs/run-grep ()
  "Gmacs grep function. Will try `gmacs/counsel-rg-region', \
`gmacs/counsel-git-grep-region', and `gmacs/grep-region' \
in order."
  (interactive)
  (defvar projectile-project-p)
  (if (executable-find "rg")
      (call-interactively 'gmacs/counsel-rg-region)
    (if projectile-project-p
        (call-interactively 'gmacs/counsel-git-grep-region)
      (call-interactively 'gmacs/lrgrep-region))))

(defun gmacs/grep (&optional initial grep-args)
  (if initial
      (let ((args
             (concat
              (eval grep-command)
              " " grep-args " -e "
              (if initial (concat (string-utils/escape-command-str initial))
                nil))))
        (progn (message args) (grep args)))
    (grep
     (string-utils/escape-command-str
      (read-string
       "Grep Command: "
       (concat (eval grep-command) " " grep-args " -e "))))))

(defun string-utils/add-quotes (str)
  (concat "\"" str "\""))

(defun string-utils/escape-command-str (str)
  (funcall
   (reduce #'compose
           (mapcar (lambda (char) (curry 'string-utils/escape-character-str char)) ["(" ")"])) str))

(defun string-utils/escape-character-str (char str)
  (string-utils/replace-in-string char (concat "\\" char) str))

(defun string-utils/replace-in-string (what with in)
  (replace-regexp-in-string (regexp-quote what) with in nil 'literal))

(defun gmacs/counsel-rg-region ()
  "Optionally run ripgrep on region."
  (interactive)
  (gmacs/opt-region-helper 'counsel-rg))

(defun gmacs/counsel-git-grep-region ()
  "Optionally run `counsel-git-grep' on region."
  (interactive)
  (gmacs/opt-region-helper
   '(lambda (&optional initial)
      (counsel-git-grep nil initial))))

(defun gmacs/grep-region ()
  "Optionally run grep on region."
  (interactive)
  (gmacs/opt-region-helper
   '(lambda (&optional initial)
      (gmacs/grep initial "-F"))))

(defun gmacs/counsel-projectile-find-file-region ()
  "Optionally run counsel-git on region."
  (interactive)
  (gmacs/opt-region-helper 'gmacs/counsel-git))

(defun gmacs/counsel-projectile-find-dir-region ()
  "Optionally run counsel-projectile-find-dir on region."
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

(defun gmacs/xref-find-definitions-symbol ()
  "X-ref-find-definitions that doesn't fall back."
  (interactive)
  (if (symbol-at-point)
      (xref-find-definitions (symbol-name (symbol-at-point)))
    (message "No symbol selected")))

(defun gmacs/xref-find-apropos-symbol ()
  "X-ref-find-apropos that doesn't fall back."
  (interactive)
  (if (symbol-at-point)
      (xref-find-apropos (symbol-name (symbol-at-point)))
    (message "No symbol selected")))

(defun gmacs/org-link-follow ()
  "Push marker stack and follow org link."
  (interactive)
  (defvar org-link-frame-setup)
  (if (thing-at-point-url-at-point)
      (browse-url-at-point)
    (let ((org-link-frame-setup '((file . (lambda (args) (progn (find-file args)))))))
      (call-interactively #'org-open-at-point))))

(defun gmacs/counsel-git (&optional initial-input)
  "Find file in the current Git repository.
INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive)
  (counsel-require-program (car (split-string counsel-git-cmd)))
  (let* ((default-directory (expand-file-name (counsel-locate-git-root)))
         (cands (split-string
                 (shell-command-to-string counsel-git-cmd)
                 "\n"
                 t)))
    (ivy-read (projectile-prepend-project-name "Find file") cands
              :initial-input initial-input
              :action #'counsel-git-action
              :caller 'counsel-git)))

(defun gmacs/counsel-projectile-find-dir (&optional initial-input)
  "Jump to a directory in the current project with initial input `INITIAL-INPUT'."
  (interactive)
  (defvar counsel-projectile-find-dir-action)
  (if (not (projectile-project-p))
      (error "Not in a git repository")
    (ivy-read (projectile-prepend-project-name "Find dir: ")
              (counsel-projectile--project-directories)
              :initial-input initial-input
              :require-match t
              :action counsel-projectile-find-dir-action
              :caller 'counsel-projectile-find-dir)))

(defun gmacs/magit-status ()
  "Wrap magit-status with projectile-project-p."
  (interactive)
  (if (not (projectile-project-p))
      (error "Not in a git repository")
    (magit-status)))

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
  (call-interactively 'newline)
  (call-interactively 'eshell-send-eof-to-process))

(defun gmacs/projectile-root-dir ()
  "Jump to the root directory of the current project."
  (interactive)
  (if (not (projectile-project-p))
      (error "Not in a git repository")
    (dired (projectile-project-root))))

(defun gmacs/open-home-dir ()
  "Open ~."
  (interactive)
  (dired "~"))

(defun gmacs/create-visit-dir (dir)
  "Open/create `DIR'."
  (if (file-directory-p dir)
      (dired dir)
    (progn
      (make-directory dir t)
      (dired dir))))

(defun gmacs/open-org-dir ()
  "Open ~/org."
  (interactive)
  (gmacs/create-visit-dir "~/org"))

(defun gmacs/open-downloads-dir ()
  "Open ~/Downloads."
  (interactive)
  (gmacs/create-visit-dir "~/Downloads"))

(defun gmacs/open-code-dir ()
  (interactive)
  (gmacs/create-visit-dir "~/code"))

(defun gmacs/write-startup-log ()
  "Write ~/.emacs.d/startup.log."
  (save-current-buffer
    (set-buffer "*Messages*")
    (append-to-file (point-min) (point-max) "~/.emacs.d/startup.log")
    (message
     (substitute-command-keys
      "To view starup log, type \\[gmacs/open-startup-log]"))))

(defun gmacs/company-cancel-complete-prev ()
  (interactive)
  (company-abort)
  (evil-complete-previous))

(defun gmacs/company-cancel-complete-next ()
  (interactive)
  (company-abort)
  (evil-complete-next))

(defun gmacs/counsel-yank-eshell-history ()
  "Yank from eshell history."
  (interactive)
  (let (collection val)
    (setq collection
          (nreverse
           (split-string
            (with-temp-buffer
              (insert-file-contents (file-truename "~/.emacs.d/eshell/history"))
              (buffer-string)) "\n" t)))
    (when (and collection (> (length collection) 0)
               (setq val (if (= 1 (length collection)) (car collection)
                           (ivy-read (format "yank eshell history: ") collection))))
      (kill-new val)
      (message "%s => kill-ring" val))))

(defun gmacs/counsel-insert-eshell-history ()
  "Insert at point from eshell history."
  (interactive)
  (let (collection val)
    (setq collection
          (nreverse
           (split-string
            (with-temp-buffer
              (insert-file-contents (file-truename "~/.emacs.d/eshell/history"))
              (buffer-string)) "\n" t)))
    (when (and collection (> (length collection) 0)
               (setq val (if (= 1 (length collection)) (car collection)
                           (ivy-read (format "insert eshell history: ") collection))))
      (insert val))))

(defun gmacs/eshell-clear ()
  "Clear terminal."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq-local gmacs/eshell-message (string-trim gmacs/eshell-message))
    (eshell-banner-initialize)
    (eshell-send-input)))

(defun gmacs/eshell-top-prompt-function ()
  (concat "[" (abbreviate-file-name (eshell/pwd)) "]"))

(defun gmacs/eshell-bottom-prompt-function ()
  (if (= (user-uid) 0) "# " "$ "))

(defun gmacs/eshell-prompt-function ()
  "A function that returns the Eshell prompt string. Make
sure to update `gmacs/eshell-prompt-regexp' so that it will
match your prompt."
  (concat "\n" (gmacs/eshell-top-prompt-function) " \n"
          (gmacs/eshell-bottom-prompt-function)))

(defun gmacs/evil-visual-or-normal-p ()
  "True if evil mode is enabled, and we are in normal or visual mode."
  (and (bound-and-true-p evil-mode)
       (not (memq evil-state '(insert emacs)))))

(defun gmacs/mc-evil-switch-to-emacs-state ()
  (when (gmacs/evil-visual-or-normal-p)
    (setq gmacs/mc-evil-prev-state evil-state)
    (when (region-active-p)
      (setq gmacs/mc-evil-mark-was-active t))
    (let ((mark-before (mark))
          (point-before (point)))
      (evil-emacs-state 1)
      (when (or gmacs/mc-evil-mark-was-active (region-active-p))
        (goto-char point-before)
        (set-mark mark-before)))))

(defun gmacs/mc-evil-back-to-previous-state ()
  (when gmacs/mc-evil-prev-state
    (unwind-protect
        (case gmacs/mc-evil-evil-prev-state
          ((normal visual) (evil-force-normal-state))
          (t (message "Don't know how to handle previous state: %S"
                      gmacs/mc-evil-evil-prev-state)))
      (setq gmacs/mc-evil-prev-state nil)
      (setq gmacs/mc-evil-mark-was-active nil))))

(defun gmacs/shrink-window-horizontally ()
  (interactive)
  (let ((current-prefix-arg `(4)))
    (call-interactively 'shrink-window-horizontally)))

(defun gmacs/enlarge-window-horizontally ()
  (interactive)
  (let ((current-prefix-arg `(4)))
    (call-interactively 'enlarge-window-horizontally)))

(defun gmacs/move-eol-eval-last-sexp ()
  (interactive)
  (save-excursion
    (call-interactively 'end-of-line)
    (call-interactively 'eval-last-sexp)))

(defun gmacs/enlarge-window ()
  (interactive)
  (let ((current-prefix-arg `(4)))
    (call-interactively 'enlarge-window)))

(defun gmacs/shrink-window ()
  (interactive)
  (let ((current-prefix-arg `(4)))
    (call-interactively 'shrink-window)))

(defun gmacs/dont-kill-scratch ()
  "Don't kill but bury *scratch* buffer."
  (if (equal (buffer-name (current-buffer)) "*scratch*")
      (progn (bury-buffer) nil)
    t))

(defun gmacs/emacs-lisp-setup ()
  (add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
  (add-hook 'lisp-interaction-mode-hook 'prettify-symbols-mode)
  (push '(company-capf company-yasnippet) company-backends))

(defun gmacs/lsp-java-enable ()
  (if (projectile-project-p)
      (gmacs/lsp-java-prompt-maybe-enable)))

(defun gmacs/lsp-java-prompt-maybe-enable ()
  (gmacs/prompt-maybe-run
   'gmacs/java-lsp-dialog-confirmed-p
   "Enable Java LSP on this ENV?"
   'gmacs/java-enable-lsp-p
   #'gmacs/lsp-java-setup))

(defun gmacs/lsp-java-setup ()
  (if gmacs/java-enable-lsp-p
      (progn (push 'company-lsp company-backends)
             (flycheck-mode 1)
             (lsp-java-enable)
             (evil-leader/set-key-for-mode 'java-mode
               "m a" 'lsp-execute-code-action
               "m r" 'lsp-rename
               "m R" 'lsp-restart-workspace
               "m f" 'lsp-format-buffer
               "m h" 'lsp-describe-thing-at-point
               "m H" 'lsp-highlight-symbol-at-point
               "m o" 'lsp-java-organize-imports
               "m b" 'lsp-java-build-project))))

(defun gmacs/evil-eshell-mode-setup ()
  (evil-define-operator evil-eshell-delete (beg end type register yank-handler)
    "Like evil-delete, but inhibit read only and when the eshell prompt is
involved re-emit it."
    (interactive "<R><x><y>")
    (let ((inhibit-read-only t)
          (total-prompt-length (length (gmacs/eshell-prompt-function)))
          (bottom-prompt-length (length (gmacs/eshell-bottom-prompt-function))))
      (if (gmacs/looking-at-eshell-prompt-regexp-p beg)
          (progn
            (evil-delete
             (+ beg bottom-prompt-length)
             end type register yank-handler)
            (delete-region
             (- (+ beg bottom-prompt-length) total-prompt-length)
             (+ beg bottom-prompt-length))
            (eshell-emit-prompt))
        (evil-delete beg end type register yank-handler))))
  ;; todo custom paste (p) operator too pls
  (evil-define-key 'normal eshell-mode-map (kbd "d") 'evil-eshell-delete)
  (setq-local inhibit-read-only t)
  (define-key evil-normal-state-local-map (kbd "M-r") 'gmacs/counsel-yank-eshell-history)
  (define-key evil-insert-state-local-map (kbd "M-r") 'gmacs/counsel-insert-eshell-history)
  (define-key evil-normal-state-local-map (kbd "C-l") 'gmacs/eshell-clear)
  (define-key evil-insert-state-local-map (kbd "C-l") 'gmacs/eshell-clear)
  (define-key evil-insert-state-local-map (kbd "C-d") 'gmacs/eshell-send-eof)
  (define-key evil-insert-state-local-map (kbd "C-i") 'eshell-pcomplete)
  (define-key evil-insert-state-local-map (kbd "C-c C-d") 'gmacs/eshell-send-eof)
  (define-key evil-normal-state-local-map (kbd "C-c C-d") 'gmacs/eshell-send-eof)
  (define-key evil-insert-state-local-map (kbd "C-k") 'eshell-life-is-too-much)
  (define-key evil-normal-state-local-map (kbd "C-k") 'eshell-life-is-too-much)
  (define-key evil-normal-state-local-map (kbd "RET") 'eshell-send-input)
  (define-key evil-normal-state-local-map (kbd "C-j") 'eshell-send-input)
  (define-key evil-normal-state-local-map (kbd "C-m") 'eshell-send-input)
  (define-key evil-insert-state-local-map (kbd "RET") 'eshell-send-input)
  (define-key evil-insert-state-local-map (kbd "C-j") 'eshell-send-input)
  (define-key evil-insert-state-local-map (kbd "C-m") 'eshell-send-input))

(defun gmacs/looking-at-eshell-prompt-regexp-p (loc)
  (save-excursion
    (goto-char loc)
    (looking-at-p gmacs/eshell-prompt-regexp)))

(defun gmacs/evil-minibuffer-setup ()
  (evil-emacs-state)
  (define-key evil-emacs-state-local-map (kbd "M-m") 'void)
  (define-key evil-emacs-state-local-map (kbd "M-j") 'void)
  (define-key evil-emacs-state-local-map (kbd "C-s") 'void)
  (define-key evil-emacs-state-local-map (kbd "M-o") 'ivy-dispatching-done-hydra))

(defun gmacs/evil-org-mode-setup ()
  (define-key evil-normal-state-local-map (kbd "M-i") 'org-cycle))

(defun gmacs/evil-c-common-mode-setup ()
  (define-key evil-normal-state-local-map (kbd "M-j") 'c-indent-new-comment-line)
  (define-key evil-insert-state-local-map (kbd "M-j") 'c-indent-new-comment-line)
  (define-key evil-normal-state-local-map (kbd "M-m") 'c-indent-new-comment-line)
  (define-key evil-insert-state-local-map (kbd "M-m") 'c-indent-new-comment-line))

(defun gmacs/evil-company-abort-on-insert-leave ()
  (if (bound-and-true-p company-mode)
      (company-abort)))

(defun gmacs/python-mode-hook ()
  (prettify-symbols-mode 1)
  (message nil))

(defun gmacs/lsp-python-mode-hook ()
  (gmacs/lsp-python-enable))

(defun gmacs/lsp-python-enable ()
  (if (projectile-project-p)
      (gmacs/lsp-python-prompt-maybe-enable)))

(defun gmacs/lsp-python-prompt-maybe-enable ()
  (gmacs/prompt-maybe-run
   'gmacs/python-lsp-dialog-confirmed-p
   "Enable Python LSP on this ENV?"
   'gmacs/python-enable-lsp-p
   #'gmacs/lsp-python-setup))

(defun gmacs/lsp-python-setup ()
  (if gmacs/python-enable-lsp-p
      (progn (push 'company-lsp company-backends)
             (flycheck-mode 1)
             (lsp-python-enable)
             (eldoc-mode 0))))

(defun gmacs/prompt-maybe-run (confirmed-var question enabled-var init-func)
  (if (not (eval confirmed-var))
      (let ((answer (y-or-n-p question)))
        (customize-save-variable confirmed-var t)
        (customize-save-variable enabled-var answer)))
  (funcall init-func))

(defun gmacs/toggle-truncate-lines-mode-no-message (arg)
  (progn (toggle-truncate-lines arg)
         (message nil)))

(defun gmacs/disable-truncate-lines-no-message ()
  (gmacs/toggle-truncate-lines-mode-no-message 0))

(defun gmacs/enable-truncate-lines-no-message ()
  (gmacs/toggle-truncate-lines-mode-no-message 1))

(defun gmacs/add-xref-js2-xref-backend ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))

(defun gmacs/enable-company-mode ()
  (make-variable-buffer-local 'company-backends)
  (company-mode))

(provide 'functions)
;;; functions.el ends here
