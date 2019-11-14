;;; functions.el --- custom jeemacs functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: jeemacs, config, function

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

(require 'seq)
(require 'functional)
(require 'cl)
(require 'string-utils)

(defun void ()
  "Interactive No-op."
  (interactive))

(defun jeemacs/emacs-startup-hook ()
  "Gmacs startup function."
  (jeemacs/reset-gc-threshold-percentage)
  (setq default-directory "~/.emacs.d/"))

(defun jeemacs/reset-gc-threshold-percentage ()
  "Reset `gc-cons-threshold' and `gc-cons-percentage' \
to sane runtime defaults."
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1))

(defun jeemacs/load-config ()
  "Load init.el."
  (interactive)
  (save-some-buffers)
  (load-file "~/.emacs.d/init.el")
  (revert-buffer t t))

(defun jeemacs/open-init-config ()
  "Open init.el."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun jeemacs/open-variables-config ()
  "Open variables.el."
  (interactive)
  (find-file "~/.emacs.d/lisp/variables.el"))

(defun jeemacs/open-functions-config ()
  "Open functions.el."
  (interactive)
  (find-file "~/.emacs.d/lisp/functions.el"))

(defun jeemacs/open-leader-config ()
  "Open leader-config.el."
  (interactive)
  (find-file "~/.emacs.d/lisp/leader-config.el"))

(defun jeemacs/open-global-config ()
  "Open global.el."
  (interactive)
  (find-file "~/.emacs.d/lisp/global.el"))

(defun jeemacs/open-evil-config ()
  "Open evil-config.el."
  (interactive)
  (find-file "~/.emacs.d/lisp/evil-config.el"))

(defun jeemacs/open-packages-config ()
  "Open packages.el."
  (interactive)
  (find-file "~/.emacs.d/lisp/packages.el"))

(defun jeemacs/open-language-config ()
  "Open languages/packages-lang.el."
  (interactive)
  (find-file "~/.emacs.d/lisp/languages/packages-lang.el"))

(defun jeemacs/open-custom-config ()
  "Open .custom.el."
  (interactive)
  (find-file "~/.emacs.d/.custom.el"))

(defun jeemacs/minibuffer-fringe-setup ()
  "Decouples the minibuffer's fringe from that of the main buffer."
  ;; (set-window-fringes (minibuffer-window) 0 0 nil)
  (add-hook 'minibuffer-setup-hook #'jeemacs/disable-minibuffer-fringe)
  (add-hook 'minibuffer-exit-hook #'jeemacs/disable-minibuffer-fringe))

(defun jeemacs/disable-minibuffer-fringe ()
  "Disable minibuffer fringe."
  (set-window-fringes (minibuffer-window) 0 0 nil))

(defun jeemacs/force-buffer-backup ()
  "Make a special per session and per save backup \
at the first save of each jeemacs session."
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist
           '(("" . "~/.backup/session")))
          (kept-new-versions 3))
      (backup-buffer)))
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(defun jeemacs/untabify-except-makefiles ()
  "Replace tabs with spaces except in makefiles."
  (unless (derived-mode-p 'makefile-mode)
    (untabify (point-min) (point-max))))

(defun jeemacs/switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

(defun jeemacs/cleanup-file ()
  "Remove tabs and trailing whitespace from buffer."
  (interactive)
  (jeemacs/untabify-except-makefiles)
  (delete-trailing-whitespace))

(defun jeemacs/run-grep ()
  "jeemacs grep function. Will try `jeemacs/counsel-rg-region', \
then `jeemacs/grep-region' in order."
  (interactive)
  (if (and (executable-find "rg") (not (eval 'jeemacs/force-basic-grep)))
      (call-interactively 'jeemacs/counsel-rg-region)
    (if (projectile-project-p)
        (let ((default-directory (projectile-project-p)))
          (funcall 'jeemacs/grep-region))
      (funcall 'jeemacs/grep-region))))

(defun jeemacs/grep (&optional initial)
  "Gmacs grep wrapper to take optional `INITIAL' input or \
prompt for grep command."
  (if initial
      (jeemacs/build-grep-command-with-region)
    (jeemacs/build-grep-command-with-input)))

(defun jeemacs/build-grep-command-with-region ()
  "Use currently selected region to build grep command."
  (jeemacs/grep-concat-command
   (lambda ()
     (string-utils/add-quotes
      (string-utils/escape-str-for-command initial)))))

(defun jeemacs/build-grep-command-with-input ()
  "Use `read-string' to build grep command."
  (jeemacs/grep-concat-command
   (lambda ()
     (string-utils/add-quotes
      (read-string "grep regexp: ")))))

(defun jeemacs/grep-concat-command (func)
  "Constuct grep command with `FUNC' and truncate with cut."
  (grep (concat (eval grep-command) (funcall func) " | cut -c -1500")))

(defun jeemacs/counsel-rg-region ()
  "Optionally run ripgrep on region."
  (interactive)
  (jeemacs/opt-region-helper 'counsel-rg))

(defun jeemacs/counsel-git-grep-region ()
  "Optionally run `counsel-git-grep' on region."
  (interactive)
  (jeemacs/opt-region-helper
   '(lambda (&optional initial)
      (counsel-git-grep nil initial))))

(defun jeemacs/grep-region ()
  "Optionally run `grep' on region."
  (interactive)
  (jeemacs/opt-region-helper
   '(lambda (&optional initial)
      (jeemacs/grep initial))))

(defun jeemacs/counsel-projectile-find-file-region ()
  "Optionally run `counsel-git' on region."
  (interactive)
  (jeemacs/opt-region-helper 'jeemacs/counsel-git))

(defun jeemacs/counsel-projectile-find-dir-region ()
  "Optionally run `counsel-projectile-find-dir' on region."
  (interactive)
  (jeemacs/opt-region-helper 'jeemacs/counsel-projectile-find-dir))

(defun jeemacs/opt-region-helper (func)
  "Add region to kill ring and run `FUNC' with optional region arg."
  (if (use-region-p)
      (let ((string (buffer-substring-no-properties
                     (region-beginning) (region-end))))
        (progn (kill-new string)
               (deactivate-mark)
               (funcall-interactively func string)))
    (funcall-interactively func)))

(defun jeemacs/swiper-region-thing ()
  "Call `swiper' on the selected region or thing under cursor."
  (interactive)
  (if (use-region-p)
      (progn (deactivate-mark)
             (swiper (buffer-substring-no-properties
                      (region-beginning) (region-end))))
    (if (word-at-point) (swiper (word-at-point))
      (error "No region or thing selected"))))

(defun jeemacs/xref-find-definitions-symbol ()
  "`xref-find-definitions' that doesn't fall back."
  (interactive)
  (if (symbol-at-point)
      (xref-find-definitions (symbol-name (symbol-at-point)))
    (message "No symbol selected")))

(defun jeemacs/xref-find-apropos-symbol ()
  "`xref-find-apropos' that doesn't fall back."
  (interactive)
  (if (symbol-at-point)
      (xref-find-apropos (symbol-name (symbol-at-point)))
    (message "No symbol selected")))

(defun jeemacs/org-link-follow ()
  "Push marker stack and follow org link."
  (interactive)
  (if (thing-at-point-url-at-point)
      (browse-url-at-point)
    (lexical-let ((org-link-frame-setup '((file . (lambda (args) (progn (find-file args)))))))
      (call-interactively #'org-open-at-point))))

(defun jeemacs/counsel-git (&optional initial-input)
  "Find file in the current Git repository. `INITIAL-INPUT' \
can be given as the initial minibuffer input."
  (interactive)
  (counsel-require-program (car (split-string counsel-git-cmd)))
  (let* ((default-directory (expand-file-name (counsel-locate-git-root)))
         (cands (split-string
                 (shell-command-to-string counsel-git-cmd)
                 "\n"
                 t)))
    (ivy-read (projectile-prepend-project-name "Find file: ") cands
              :initial-input initial-input
              :action #'counsel-git-action
              :caller 'counsel-git)))

(defun jeemacs/counsel-projectile-find-dir (&optional initial-input)
  "Jump to a directory in the current project with \
initial input `INITIAL-INPUT'."
  (interactive)
  (counsel-require-program (car (split-string counsel-git-cmd)))
  (let* ((default-directory (expand-file-name (counsel-locate-git-root)))
         (cands (flatmap
                 'last
                 (seq-filter
                  #'jeemacs/directory-ls-tree-entry-p
                  (mapcar
                   'split-string
                   (cdr
                    (split-string
                     (shell-command-to-string jeemacs/git-ls-tree-head-cmd)
                     "\n"
                     t)))))))
    (ivy-read (projectile-prepend-project-name "Find dir: ") cands
              :initial-input initial-input
              :action #'counsel-projectile-find-dir-action
              :caller 'counsel-projectile-find-dir)))

(defun jeemacs/magit-status ()
  "Wrap `magit-status' with `projectile-project-p'."
  (interactive)
  (if (not (projectile-project-p))
      (error "Not in a git repository")
    (magit-status)))

(defun jeemacs/toggle-spelling ()
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

(defun jeemacs/unhighlight-all ()
  "Unhighlight all currently highlighted symbols and \
disable command `hi-lock-mode'."
  (interactive)
  (unhighlight-regexp t)
  (hi-lock-mode 0))

(defun jeemacs/switch-to-scratch-buffer ()
  "Switch to initial scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun jeemacs/switch-to-messages-buffer ()
  "Switch to Messages buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun jeemacs/eshell-send-eof ()
  "Send EOF to Eshell with newline."
  (interactive)
  (call-interactively 'newline)
  (call-interactively 'eshell-send-eof-to-process))

(defun jeemacs/projectile-root-dir ()
  "Jump to the root directory of the current project."
  (interactive)
  (if (not (projectile-project-p))
      (error "Not in a git repository")
    (dired (projectile-project-root))))

(defun jeemacs/open-home-dir ()
  "Open ~."
  (interactive)
  (dired "~"))

(defun jeemacs/create-visit-dir (dir)
  "Open/create `DIR'."
  (if (file-directory-p dir)
      (dired dir)
    (progn
      (make-directory dir t)
      (dired dir))))

(defun jeemacs/open-org-dir ()
  "Open ~/org."
  (interactive)
  (jeemacs/create-visit-dir "~/org"))

(defun jeemacs/open-downloads-dir ()
  "Open ~/Downloads."
  (interactive)
  (jeemacs/create-visit-dir "~/Downloads"))

(defun jeemacs/open-code-dir ()
  "Open ~/code."
  (interactive)
  (jeemacs/create-visit-dir "~/code"))

(defun jeemacs/company-cancel-complete-prev ()
  "Exit company mode and use evil complete to autocomplete upwards."
  (interactive)
  (company-abort)
  (evil-complete-previous))

(defun jeemacs/company-cancel-complete-next ()
  "Exit company mode and use evil complete to autocomplete downwards."
  (interactive)
  (company-abort)
  (evil-complete-next))

(defun jeemacs/counsel-func-eshell-history (msg func)
  "Call `FUNC' and display `MSG' on value from from eshell history."
  (let (collection val)
    (setq collection
          (nreverse
           (split-string
            (with-temp-buffer
              (insert-file-contents (file-truename "~/.emacs.d/eshell/history"))
              (buffer-string)) "\n" t)))
    (when (and collection (> (length collection) 0)
               (setq val (if (= 1 (length collection)) (car collection)
                           (ivy-read (format msg) collection))))
      (funcall func val))))

(defun jeemacs/counsel-insert-eshell-history ()
  "Insert at point from eshell history."
  (interactive)
  (jeemacs/counsel-func-eshell-history
   "insert eshell history: "
   (lambda (v) (insert v))))

(defun jeemacs/counsel-yank-eshell-history ()
  "Yank from eshell history."
  (interactive)
  (jeemacs/counsel-func-eshell-history
   "yank eshell history: "
   (lambda (v)
     (progn
       (kill-new v)
       (message "%s yanked" v)))))

(defun jeemacs/eshell-clear ()
  "Clear terminal."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq-local jeemacs/eshell-message (string-trim jeemacs/eshell-message))
    (eshell-banner-initialize)
    (eshell-send-input)))

(defun jeemacs/eshell-top-prompt-function ()
  "Builds the top line of the jeemacs eshell prompt."
  (concat "[" (abbreviate-file-name (eshell/pwd)) "]"))

(defun jeemacs/eshell-bottom-prompt-function ()
  "Builds the bottom line of the jeemacs eshell prompt."
  (if (= (user-uid) 0) "# " "$ "))

(defun jeemacs/eshell-prompt-function ()
  "Builds the Eshell prompt string. Make sure to \
update `jeemacs/eshell-prompt-regexp' so that it will \
match your prompt."
  (concat "\n" (jeemacs/eshell-top-prompt-function) " \n"
          (jeemacs/eshell-bottom-prompt-function)))

(defun jeemacs/mc-evil-emacs-state ()
  "When using multiple-cursors, switch to Emacs state."
  (if (region-active-p)
      (delete-selection-mode 1))
  (evil-emacs-state 1))

(defun jeemacs/mc-evil-normal-state ()
  "When done using multiple-cursors, switch to normal mode."
  (delete-selection-mode 0)
  (evil-normal-state 1))

(defun jeemacs/shrink-window-horizontally ()
  "Shrink the active window horizontally."
  (interactive)
  (let ((current-prefix-arg `(4)))
    (call-interactively 'shrink-window-horizontally)))

(defun jeemacs/enlarge-window-horizontally ()
  "Enlarge the active window horizontally."
  (interactive)
  (let ((current-prefix-arg `(4)))
    (call-interactively 'enlarge-window-horizontally)))

(defun jeemacs/move-eol-eval-last-sexp ()
  "Eval the current line as if you were at the eol."
  (interactive)
  (save-excursion
    (call-interactively 'end-of-line)
    (call-interactively 'eval-last-sexp)))

(defun jeemacs/cider-move-eol-eval-last-sexp ()
  "Eval the current line as if you were at the eol."
  (interactive)
  (save-excursion
    (call-interactively 'end-of-line)
    (call-interactively 'cider-eval-last-sexp)))

(defun jeemacs/enlarge-window ()
  "Enlarge the active window vertically."
  (interactive)
  (let ((current-prefix-arg `(4)))
    (call-interactively 'enlarge-window)))

(defun jeemacs/shrink-window ()
  "Shrink the active window vertically."
  (interactive)
  (let ((current-prefix-arg `(4)))
    (call-interactively 'shrink-window)))

(defun jeemacs/dont-kill-scratch-or-dired ()
  "Don't kill but bury *scratch* and \"dired:\" buffers."
  (if (or (string-match-p "dired:" (buffer-name))
          (equal (buffer-name (current-buffer)) "*scratch*"))
      (progn (bury-buffer) nil)
    t))

(defun jeemacs/emacs-lisp-setup ()
  "Setup Emacs Lisp Mode."
  (add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
  (add-hook 'lisp-interaction-mode-hook 'prettify-symbols-mode)
  (push '(company-capf company-yasnippet) company-backends))

(defun jeemacs/evil-eshell-mode-setup ()
  "Setup Jeeshell."
  (setq-local inhibit-read-only t)
  (jeemacs/eshell-def-evil-eshell-delete)
  (jeemacs/jeeshell-evil-mode-keys-setup))

(defun jeemacs/jeeshell-evil-mode-keys-setup ()
  "Setup Jeeshell evil-mode keys."
  (evil-define-key 'normal eshell-mode-map (kbd "d") 'evil-eshell-delete)
  (define-key evil-normal-state-local-map (kbd "M-r") 'jeemacs/counsel-yank-eshell-history)
  (define-key evil-insert-state-local-map (kbd "M-r") 'jeemacs/counsel-insert-eshell-history)
  (define-key evil-normal-state-local-map (kbd "C-l") 'jeemacs/eshell-clear)
  (define-key evil-insert-state-local-map (kbd "C-l") 'jeemacs/eshell-clear)
  (define-key evil-insert-state-local-map (kbd "C-d") 'jeemacs/eshell-send-eof-kill-on-empty-prompt)
  (define-key evil-insert-state-local-map (kbd "C-i") 'eshell-pcomplete)
  (define-key evil-insert-state-local-map (kbd "C-c C-d") 'jeemacs/eshell-send-eof)
  (define-key evil-normal-state-local-map (kbd "C-c C-d") 'jeemacs/eshell-send-eof)
  (define-key evil-insert-state-local-map (kbd "C-k") 'eshell-life-is-too-much)
  (define-key evil-normal-state-local-map (kbd "C-k") 'eshell-life-is-too-much)
  (define-key evil-normal-state-local-map (kbd "RET") 'eshell-send-input)
  (define-key evil-normal-state-local-map (kbd "C-j") 'eshell-send-input)
  (define-key evil-normal-state-local-map (kbd "C-m") 'eshell-send-input)
  (define-key evil-insert-state-local-map (kbd "RET") 'eshell-send-input)
  (define-key evil-insert-state-local-map (kbd "C-j") 'eshell-send-input)
  (define-key evil-insert-state-local-map (kbd "C-m") 'eshell-send-input))

(defun jeemacs/eshell-def-evil-eshell-delete ()
  "Define custom delete operation for Eshell when on the line with the prompt."
  (evil-define-operator evil-eshell-delete (beg end type register yank-handler)
    "Like evil-delete, but inhibit read only and when the eshell prompt is \
involved re-emit it."
    (interactive "<R><x><y>")
    (let ((inhibit-read-only t)
          (total-prompt-length (length (jeemacs/eshell-prompt-function)))
          (bottom-prompt-length (length (jeemacs/eshell-bottom-prompt-function))))
      (cond
       ((jeemacs/looking-at-eshell-prompt-regexp-p beg)
        (progn
          (evil-delete
           (+ beg bottom-prompt-length)
           end type register yank-handler)
          (delete-region
           (- (+ beg bottom-prompt-length) total-prompt-length)
           (+ beg bottom-prompt-length))
          (eshell-emit-prompt)))
       ((jeemacs/looking-at-eshell-top-prompt-regexp-p beg) (void))
       (t (evil-delete beg end type register yank-handler))))))

(defun jeemacs/looking-at-eshell-prompt-regexp-p (loc)
  "Truthy value for evil-eshell-delete which determines if \
the `LOC' is `looking-at-p' `jeemacs/eshell-prompt-regexp'."
  (save-excursion
    (goto-char loc)
    (looking-at-p jeemacs/eshell-prompt-regexp)))

(defun jeemacs/looking-at-empty-eshell-prompt-regexp-p (loc)
  "Truthy value for evil-eshell-delete which determines if \
the `LOC' is `looking-at-p' `jeemacs/eshell-prompt-regexp' \
followed by nothing."
  (save-excursion
    (goto-char loc)
    (looking-at-p (concat jeemacs/eshell-prompt-regexp "$"))))

(defun jeemacs/eshell-send-eof-kill-on-empty-prompt ()
  "Send eshell-life-is-too-much if there is no pending \
eshell command string, and EOF if there is a pending command string."
  (interactive)
  (if (jeemacs/looking-at-empty-eshell-prompt-p)
      (eshell-life-is-too-much)
    (jeemacs/eshell-send-eof)))

(defun jeemacs/looking-at-empty-eshell-prompt-p ()
  "Test if looking an empty eshell prompt."
  (let ((beginning-of-line-pos
         (progn
           (save-excursion
             (call-interactively 'move-beginning-of-line)
             (point)))))
    (jeemacs/looking-at-empty-eshell-prompt-regexp-p beginning-of-line-pos)))

(defun jeemacs/looking-at-eshell-top-prompt-regexp-p (loc)
  "Truthy value for evil-eshell-delete which determines if \
the `LOC' is `looking-at-p' `jeemacs/eshell-top-prompt-regexp'."
  (save-excursion
    (goto-char loc)
    (looking-at-p jeemacs/eshell-top-prompt-regexp)))

(defun jeemacs/evil-minibuffer-setup ()
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

(defun jeemacs/evil-org-mode-setup ()
  "Setup org mode."
  (define-key evil-normal-state-local-map (kbd "M-i") 'org-cycle)
  (define-key evil-normal-state-local-map (kbd "C-M-l") 'jeemacs/org-cycle-list-bullet-forward)
  (define-key evil-normal-state-local-map (kbd "C-M-h") 'jeemacs/org-cycle-list-bullet-backward)
  (define-key evil-insert-state-local-map (kbd "C-d") 'evil-shift-left-line))

(defun jeemacs/org-cycle-list-bullet-forward ()
  "Cycle org list bullet type forward."
  (interactive)
  (funcall-interactively 'org-cycle-list-bullet 'nil))

(defun jeemacs/org-cycle-list-bullet-backward ()
  "Cycle org list bullet type backward."
  (interactive)
  (funcall-interactively 'org-cycle-list-bullet 'previous))

(defun jeemacs/evil-c-common-mode-setup ()
  "Setup C mode and it's derivatives."
  (define-key evil-normal-state-local-map (kbd "M-j") 'c-indent-new-comment-line)
  (define-key evil-insert-state-local-map (kbd "M-j") 'c-indent-new-comment-line)
  (define-key evil-normal-state-local-map (kbd "M-m") 'c-indent-new-comment-line)
  (define-key evil-insert-state-local-map (kbd "M-m") 'c-indent-new-comment-line))

(defun jeemacs/evil-emmet-mode-setup ()
  "Setup emmet-mode for evil."
  (define-key evil-normal-state-local-map (kbd "M-j") 'emmet-expand-line)
  (define-key evil-insert-state-local-map (kbd "M-j") 'emmet-expand-line)
  (define-key evil-normal-state-local-map (kbd "M-l") 'emmet-next-edit-point)
  (define-key evil-insert-state-local-map (kbd "M-l") 'emmet-next-edit-point)
  (define-key evil-normal-state-local-map (kbd "M-h") 'emmet-prev-edit-point)
  (define-key evil-insert-state-local-map (kbd "M-h") 'emmet-prev-edit-point))

(defun jeemacs/evil-company-abort-on-insert-leave ()
  "If company mode is currently enabled, run company abort. \
Note: effective as an evil-insert-state-exit-hook."
  (if (bound-and-true-p company-mode)
      (company-abort)))

(defun jeemacs/python-mode-hook ()
  "Python mode setup."
  (prettify-symbols-mode 1)
  (message nil))

(defun jeemacs/prompt-maybe-run (confirmed-var question enabled-var init-func)
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

(defun jeemacs/toggle-truncate-lines-mode-no-message (arg)
  "Run `toggle-truncate-lines' with `ARG' and swallow the message."
  (progn (toggle-truncate-lines arg)
         (message nil)))

(defun jeemacs/disable-truncate-lines-no-message ()
  "Disable `toggle-truncate-lines' and swallow the message."
  (jeemacs/toggle-truncate-lines-mode-no-message 0))

(defun jeemacs/enable-truncate-lines-no-message ()
  "Enable `toggle-truncate-lines' and swallow the message."
  (jeemacs/toggle-truncate-lines-mode-no-message 1))

(defun jeemacs/add-xref-js2-xref-backend ()
  "Add `js2-xref-backend' to `xref-backend-functions'."
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))

(defun jeemacs/enable-company-mode ()
  "Enable Company Mode."
  (make-variable-buffer-local 'company-backends)
  (company-mode))

(defun jeemacs/evil-command-window-setup ()
  "Setup Evil command window."
  (define-key evil-normal-state-local-map (kbd "M-:") 'evil-window-delete))

(defun jeemacs/rename-dired-buffer ()
  "Rename Dired buffers so that they can be referred to in the Evil Jumplist."
  (interactive)
  (unless (string-match-p (concat "dired" uniquify-separator) (buffer-name))
    (rename-buffer (concat "dired" uniquify-separator (generate-new-buffer-name dired-directory)))))

(defun jeemacs/directory-ls-tree-entry-p (entry)
  "Function to `mapcar' to filter `ENTRY' in 'git ls-tree' for directories."
  (string= (nth 1 entry) "tree"))

(defun jeemacs/shell-kill-buffer-on-exit-sentinel ()
  "Create sentinal to wait for shell process to exit, \
then kill buffer."
  ;; Kill the buffer when the shell process exits.
  (let* ((proc (get-buffer-process (current-buffer)))
         (sentinel (process-sentinel proc)))
    (set-process-sentinel
     proc
     `(lambda (process signal)
        ;; Call the original process sentinel first.
        (funcall #',sentinel process signal)
        ;; Kill the buffer on an exit signal.
        (and (memq (process-status process) '(exit signal))
             (buffer-live-p (process-buffer process))
             (kill-buffer (process-buffer process)))))))

(defun jeemacs/web-mode-setup ()
  "Configure `web-mode'."
  (sp-local-pair 'web-mode "<" "")
  (emmet-mode 1))

(defun jeemacs/auto-revert-mode-setup ()
  "Configure `auto-revert-mode' setup."
  (diminish 'auto-revert-mode))

(defun jeemacs/evil-jumplist-setup ()
  "Configure evil-jumplist."
  (setq evil--jumps-buffer-targets "\\(\\*\\(\\new\\|scratch\\)\\*\\|dired:.+\\)")
  (evil-add-command-properties #'dired-find-file :jump t))

(defun jeemacs/org-indent-setup ()
  "Configure `org-intent-mode'."
  (diminish 'org-indent-mode "in"))

(defun jeemacs/cider-repl-mode-setup ()
  "Configure `cider-repl-mode'"
  (define-key evil-normal-state-local-map (kbd "C-m") 'cider-repl-return)
  (define-key evil-insert-state-local-map (kbd "C-m") 'cider-repl-return)
  (define-key evil-normal-state-local-map (kbd "M-m") 'cider-repl-return)
  (define-key evil-insert-state-local-map (kbd "M-m") 'cider-repl-return))

(provide 'functions)
;;; functions.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars cl-functions make-local unresolved)
;; End:
