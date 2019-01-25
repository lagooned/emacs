;;; functions.el --- custom gmacs functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: gmacs, config, function

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
  "Gmacs startup function."
  (gmacs/reset-gc-threshold-percentage)
  (gmacs/write-startup-log)
  (kill-buffer "*Messages*")
  (setq default-directory "~/.emacs.d/"))

(defun gmacs/reset-gc-threshold-percentage ()
  "Reset `gc-cons-threshold' and `gc-cons-percentage' \
to sane runtime defaults."
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
  (goto-char (point-max)))

(defun gmacs/minibuffer-fringe-setup ()
  "Decouples the minibuffer's fringe from that of the main buffer."
  (set-window-fringes (minibuffer-window) 0 0 nil)
  (add-hook 'minibuffer-setup-hook #'gmacs/disable-minibuffer-fringe))

(defun gmacs/disable-minibuffer-fringe ()
  "Disable minibuffer fringe."
  (set-window-fringes (minibuffer-window) 0 0 nil))

(defun gmacs/force-buffer-backup ()
  "Make a special per session and per save backup \
at the first save of each gmacs session."
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist
           '(("" . "~/.backup/session")))
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
    (when (and (not (memq major-mode gmacs/large-file-modes-ignore-list))
               size (> size (* 1024 1024 gmacs/large-file-size))
               (y-or-n-p
                (format
                 (concat
                  "%s is a large file, open literally"
                  " to avoid performance issues?")
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
then `gmacs/grep-region' in order."
  (interactive)
  (if (and (executable-find "rg") (not (eval 'gmacs/force-basic-grep)))
      (call-interactively 'gmacs/counsel-rg-region)
    (if (projectile-project-p)
        (let ((default-directory (projectile-project-p)))
          (funcall 'gmacs/grep-region))
      (funcall 'gmacs/grep-region))))

(defun gmacs/grep (&optional initial)
  "Gmacs grep wrapper to take optional `INITIAL' input or \
prompt for grep command."
  (if initial
      (let ((args
             (concat
              (eval grep-command) " -F -e "
              (if initial
                  (concat (string-utils/escape-str-for-command initial))
                nil))))
        (progn (message args) (grep args)))
    (grep
     (read-string
      "Grep Command: "
      (concat (eval grep-command) " -e ")))))

(defun string-utils/add-quotes (str)
  "Surround `STR' in quotes."
  (concat "\"" str "\""))

(defun string-utils/escape-str-for-command (str)
  "Escape parens, space, and quotes in `STR'."
  (string-utils/escape-command-str str [";" " " "\"" "(" ")" "'" "`"]))

(defun string-utils/escape-command-str (str charlist)
  "Escapes all instances of each element of `CHARLIST' in `STR'."
  (funcall
   (reduce
    #'compose
    (mapcar (lambda (char) (curry 'string-utils/escape-character-str char)) charlist))
   str))

(defun string-utils/escape-character-str (char str)
  "Escapes every instance of `CHAR' in `STR'."
  (string-utils/replace-in-string char (concat "\\" char) str))

(defun string-utils/replace-in-string (what with in)
  "Replace `WHAT' `WITH' `IN'."
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
  "Optionally run `grep' on region."
  (interactive)
  (gmacs/opt-region-helper
   '(lambda (&optional initial)
      (gmacs/grep initial))))

(defun gmacs/counsel-projectile-find-file-region ()
  "Optionally run `counsel-git' on region."
  (interactive)
  (gmacs/opt-region-helper 'gmacs/counsel-git))

(defun gmacs/counsel-projectile-find-dir-region ()
  "Optionally run `counsel-projectile-find-dir' on region."
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
  "Call `swiper' on the selected region or thing under cursor."
  (interactive)
  (if (use-region-p)
      (progn (deactivate-mark)
             (swiper (buffer-substring-no-properties
                      (region-beginning) (region-end))))
    (if (word-at-point) (swiper (word-at-point))
      (error "No region or thing selected"))))

(defun gmacs/xref-find-definitions-symbol ()
  "`xref-find-definitions' that doesn't fall back."
  (interactive)
  (if (symbol-at-point)
      (xref-find-definitions (symbol-name (symbol-at-point)))
    (message "No symbol selected")))

(defun gmacs/xref-find-apropos-symbol ()
  "`xref-find-apropos' that doesn't fall back."
  (interactive)
  (if (symbol-at-point)
      (xref-find-apropos (symbol-name (symbol-at-point)))
    (message "No symbol selected")))

(defun gmacs/org-link-follow ()
  "Push marker stack and follow org link."
  (interactive)
  (if (thing-at-point-url-at-point)
      (browse-url-at-point)
    (let ((org-link-frame-setup '((file . (lambda (args) (progn (find-file args)))))))
      (call-interactively #'org-open-at-point))))

(defun gmacs/counsel-git (&optional initial-input)
  "Find file in the current Git repository. `INITIAL-INPUT' \
can be given as the initial minibuffer input."
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
  "Jump to a directory in the current project with \
initial input `INITIAL-INPUT'."
  (interactive)
  (if (not (projectile-project-p))
      (error "Not in a git repository")
    (ivy-read (projectile-prepend-project-name "Find dir: ")
              (counsel-projectile--project-directories)
              :initial-input initial-input
              :require-match t
              :action counsel-projectile-find-dir-action
              :caller 'counsel-projectile-find-dir)))

(defun gmacs/magit-status ()
  "Wrap `magit-status' with `projectile-project-p'."
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
disable command `hi-lock-mode'."
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
  "Open ~/code."
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
  "Exit company mode and use evil complete to autocomplete upwards."
  (interactive)
  (company-abort)
  (evil-complete-previous))

(defun gmacs/company-cancel-complete-next ()
  "Exit company mode and use evil complete to autocomplete downwards."
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
  "Builds the top line of the gmacs eshell prompt."
  (concat "[" (abbreviate-file-name (eshell/pwd)) "]"))

(defun gmacs/eshell-bottom-prompt-function ()
  "Builds the bottom line of the gmacs eshell prompt."
  (if (= (user-uid) 0) "# " "$ "))

(defun gmacs/eshell-prompt-function ()
  "Builds the Eshell prompt string. Make sure to \
update `gmacs/eshell-prompt-regexp' so that it will \
match your prompt."
  (concat "\n" (gmacs/eshell-top-prompt-function) " \n"
          (gmacs/eshell-bottom-prompt-function)))

(defun gmacs/evil-visual-or-normal-p ()
  "True if evil mode is enabled, and we are in \
normal or visual mode."
  (and (bound-and-true-p evil-mode)
       (not (memq evil-state '(insert emacs)))))

(defun gmacs/mc-evil-switch-to-emacs-state ()
  "When using multiple-cursors, switch to evil-mode Emacs state."
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
  "When done using multiple-cursors, switch back previous \
evil-mode state."
  (when gmacs/mc-evil-prev-state
    (unwind-protect
        (case gmacs/mc-evil-evil-prev-state
          ((normal visual) (evil-force-normal-state))
          (t (message "Don't know how to handle previous state: %S"
                      gmacs/mc-evil-evil-prev-state)))
      (setq gmacs/mc-evil-prev-state nil)
      (setq gmacs/mc-evil-mark-was-active nil))))

(defun gmacs/shrink-window-horizontally ()
  "Shrink the active window horizontally."
  (interactive)
  (let ((current-prefix-arg `(4)))
    (call-interactively 'shrink-window-horizontally)))

(defun gmacs/enlarge-window-horizontally ()
  "Enlarge the active window horizontally."
  (interactive)
  (let ((current-prefix-arg `(4)))
    (call-interactively 'enlarge-window-horizontally)))

(defun gmacs/move-eol-eval-last-sexp ()
  "Eval the current line as if you were at the eol."
  (interactive)
  (save-excursion
    (call-interactively 'end-of-line)
    (call-interactively 'eval-last-sexp)))

(defun gmacs/enlarge-window ()
  "Enlarge the active window vertically."
  (interactive)
  (let ((current-prefix-arg `(4)))
    (call-interactively 'enlarge-window)))

(defun gmacs/shrink-window ()
  "Shrink the active window vertically."
  (interactive)
  (let ((current-prefix-arg `(4)))
    (call-interactively 'shrink-window)))

(defun gmacs/dont-kill-scratch ()
  "Don't kill but bury *scratch* buffer."
  (if (equal (buffer-name (current-buffer)) "*scratch*")
      (progn (bury-buffer) nil)
    t))

(defun gmacs/emacs-lisp-setup ()
  "Setup Emacs Lisp Mode."
  (add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
  (add-hook 'lisp-interaction-mode-hook 'prettify-symbols-mode)
  (push '(company-capf company-yasnippet) company-backends))

(defun gmacs/lsp-java-enable ()
  "Enable Java LSP if in project and confirmation prompt \
has been accepted."
  (if (projectile-project-p)
      (gmacs/prompt-maybe-run
       'gmacs/java-lsp-dialog-confirmed-p
       "Enable Java LSP on this ENV?"
       'gmacs/java-enable-lsp-p
       #'gmacs/lsp-java-setup)))

(defun gmacs/lsp-java-setup ()
  "Initialize Java LSP."
  (if gmacs/java-enable-lsp-p
      (progn (push 'company-lsp company-backends)
             (flycheck-mode 1)
             (lsp-java-enable)
             (gmacs/lsp-java-leader-setup))))

(defun gmacs/lsp-java-leader-setup ()
  "Setup Evil-Leader for Java LSP."
  (evil-leader/set-key-for-mode 'java-mode
    "m a" 'lsp-execute-code-action
    "m r" 'lsp-rename
    "m R" 'lsp-restart-workspace
    "m f" 'lsp-format-buffer
    "m h" 'lsp-describe-thing-at-point
    "m H" 'lsp-highlight-symbol-at-point
    "m o" 'lsp-java-organize-imports
    "m b" 'lsp-java-build-project))

(defun gmacs/evil-eshell-mode-setup ()
  "Setup Gshell."
  (setq-local inhibit-read-only t)
  (gmacs/gshell-evil-mode-keys-setup))

(defun gmacs/gshell-evil-mode-keys-setup ()
  "Setup Gshell evil-mode keys."
  (evil-define-key 'normal eshell-mode-map (kbd "d") 'evil-eshell-delete)
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
  "Truthy value for evil-eshell-delete which determines if \
the `LOC' is `looking-at-p' `gmacs/eshell-prompt-regexp'."
  (save-excursion
    (goto-char loc)
    (looking-at-p gmacs/eshell-prompt-regexp)))

(defun gmacs/evil-minibuffer-setup ()
  "Setup the minibuffer."
  (evil-emacs-state)
  (define-key evil-emacs-state-local-map (kbd "M-m") 'void)
  (define-key evil-emacs-state-local-map (kbd "M-j") 'void)
  (define-key evil-emacs-state-local-map (kbd "C-s") 'void)
  (define-key evil-emacs-state-local-map (kbd "C-j") 'ivy-alt-done)
  (define-key evil-emacs-state-local-map (kbd "M-o") 'ivy-dispatching-done-hydra)
  (define-key evil-emacs-state-local-map (kbd "M-n") 'ivy-next-history-element)
  (define-key evil-emacs-state-local-map (kbd "M-p") 'ivy-previous-history-element)
  (define-key evil-emacs-state-local-map (kbd "M-i") 'ivy-insert-current)
  (define-key evil-emacs-state-local-map (kbd "M-j") 'ivy-yank-word)
  (define-key evil-emacs-state-local-map (kbd "M-w") 'ivy-kill-ring-save)
  (define-key evil-emacs-state-local-map (kbd "M-r") 'ivy-reverse-i-search)
  (define-key evil-emacs-state-local-map (kbd "S-SPC") 'ivy-restrict-to-matches)
  (define-key evil-emacs-state-local-map (kbd "C-M-j") 'ivy-immediate-done)
  (define-key evil-emacs-state-local-map (kbd "C-M-m") 'ivy-call)
  (define-key evil-emacs-state-local-map (kbd "C-M-n") 'ivy-next-line-and-call)
  (define-key evil-emacs-state-local-map (kbd "C-M-p") 'ivy-previous-line-and-call)
  (define-key evil-emacs-state-local-map (kbd "C-M-o") 'ivy-dispatching-call)
  (define-key evil-emacs-state-local-map (kbd "C-M-h") 'ivy-help))

(defun gmacs/evil-org-mode-setup ()
  "Setup org mode."
  (define-key evil-normal-state-local-map (kbd "M-i") 'org-cycle))

(defun gmacs/evil-c-common-mode-setup ()
  "Setup C mode and it's derivatives."
  (define-key evil-normal-state-local-map (kbd "M-j") 'c-indent-new-comment-line)
  (define-key evil-insert-state-local-map (kbd "M-j") 'c-indent-new-comment-line)
  (define-key evil-normal-state-local-map (kbd "M-m") 'c-indent-new-comment-line)
  (define-key evil-insert-state-local-map (kbd "M-m") 'c-indent-new-comment-line))

(defun gmacs/evil-company-abort-on-insert-leave ()
  "If company mode is currently enabled, run company abort. \
Note: effective as an evil-insert-state-exit-hook."
  (if (bound-and-true-p company-mode)
      (company-abort)))

(defun gmacs/python-mode-hook ()
  "Python mode setup."
  (prettify-symbols-mode 1)
  (message nil))

(defun gmacs/lsp-python-mode-hook ()
  "Python LSP Enable."
  (gmacs/lsp-python-enable))

(defun gmacs/lsp-python-enable ()
  "Enable Python LSP if in project and confirmation prompt \
has been accepted."
  (if (projectile-project-p)
      (gmacs/prompt-maybe-run
       'gmacs/python-lsp-dialog-confirmed-p
       "Enable Python LSP on this ENV?"
       'gmacs/python-enable-lsp-p
       #'gmacs/lsp-python-setup)))

(defun gmacs/lsp-python-setup ()
  "Setup Python LSP mode."
  (if gmacs/python-enable-lsp-p
      (progn (push 'company-lsp company-backends)
             (flycheck-mode 1)
             (lsp-python-enable)
             (eldoc-mode 0))))

(defun gmacs/prompt-maybe-run (confirmed-var question enabled-var init-func)
  "Defines an interface which one can adhear to create a environmentally \
stateful confirmation dialog.

`CONFIRMED-VAR': var that keeps track of the user confirmation of the dialog.
`QUESTION': string question to ask in the confirmation dialog.
`ENABLED-VAR': var which keeps track of the response from the user.
`INIT-FUNC': function to run when the dialog has been confirmed."
  (if (not (eval confirmed-var))
      (let ((answer (y-or-n-p question)))
        (customize-save-variable confirmed-var t)
        (customize-save-variable enabled-var answer)))
  (funcall init-func))

(defun gmacs/toggle-truncate-lines-mode-no-message (arg)
  "Run `toggle-truncate-lines' with `ARG' and swallow the message."
  (progn (toggle-truncate-lines arg)
         (message nil)))

(defun gmacs/disable-truncate-lines-no-message ()
  "Disable `toggle-truncate-lines' and swallow the message."
  (gmacs/toggle-truncate-lines-mode-no-message 0))

(defun gmacs/enable-truncate-lines-no-message ()
  "Enable `toggle-truncate-lines' and swallow the message."
  (gmacs/toggle-truncate-lines-mode-no-message 1))

(defun gmacs/add-xref-js2-xref-backend ()
  "Add `js2-xref-backend' to `xref-backend-functions'."
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))

(defun gmacs/enable-company-mode ()
  "Enable Company Mode."
  (make-variable-buffer-local 'company-backends)
  (company-mode))

(defun gmacs/php-mode-setup ()
  "Setup php-mode."
  (flycheck-define-checker gmacs-php
    "A PHP syntax checker using the PHP command line interpreter."
    :command
    ("php" "-l"
     "-d" "error_reporting=E_ALL"
     "-d" "display_errors=1"
     "-d" "log_errors=0"
     source)
    :error-patterns
    ((error
      line-start
      (or "Parse" "Fatal" "syntax") " error" (any ":" ",") " "
      (message) " in " (file-name) " on line " line line-end))
    :modes
    (php-mode)
    :next-checkers
    ((warning . php-phpmd)
     (warning . php-phpcs))))

(defun gmacs/css-mode-setup ()
  "Setup CSS mode."
  (defun css--fontify-region (start end &optional loudly)
    "Fontify a CSS buffer between START and END.
START and END are buffer positions."
    (let ((extended-region (font-lock-default-fontify-region start end loudly)))
      (when css-fontify-colors
        (when (and (consp extended-region)
                   (eq (car extended-region) 'jit-lock-bounds))
          (setq start (cadr extended-region))
          (setq end (cddr extended-region)))
        (save-excursion
          (let ((case-fold-search t))
            (goto-char start)
            (while (re-search-forward css--colors-regexp end t)
              ;; Skip comments and strings.
              (unless (nth 8 (syntax-ppss))
                (let* ((start (match-beginning 0))
                       (color (css--compute-color start (match-string 0))))
                  (when color
                    (with-silent-modifications
                      (add-text-properties
                       start (point)
                       (list 'face (list :background color
                                         :foreground (css--contrasty-color color))))))))))))
      extended-region)))


(provide 'functions)
;;; functions.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars cl-functions make-local)
;; End:

