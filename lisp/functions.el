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

(defun je/ensure-use-package ()
  "Install `use-package' if not installed."
  (if (not (package-installed-p 'use-package))
      (progn (package-refresh-contents)
             (package-install 'use-package))))

(defun je/emacs-startup-hook ()
  "Gmacs startup function."
  (je/reset-gc-threshold-percentage)
  (setq default-directory "~/.emacs.d/"))

(defun je/reset-gc-threshold-percentage ()
  "Reset `gc-cons-threshold' and `gc-cons-percentage' \
to sane runtime defaults."
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1))

(defun je/load-config ()
  "Load init.el."
  (interactive)
  (save-some-buffers)
  (load-file "~/.emacs.d/init.el")
  (revert-buffer t t))

(defun je/open-init-config ()
  "Open init.el."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun je/open-variables-config ()
  "Open variables.el."
  (interactive)
  (find-file "~/.emacs.d/lisp/variables.el"))

(defun je/open-functions-config ()
  "Open functions.el."
  (interactive)
  (find-file "~/.emacs.d/lisp/functions.el"))

(defun je/open-leader-config ()
  "Open leader-config.el."
  (interactive)
  (find-file "~/.emacs.d/lisp/leader-config.el"))

(defun je/open-global-config ()
  "Open global.el."
  (interactive)
  (find-file "~/.emacs.d/lisp/global.el"))

(defun je/open-evil-config ()
  "Open evil-config.el."
  (interactive)
  (find-file "~/.emacs.d/lisp/evil-config.el"))

(defun je/open-packages-config ()
  "Open packages.el."
  (interactive)
  (find-file "~/.emacs.d/lisp/packages.el"))

(defun je/open-language-config ()
  "Open languages/packages-lang.el."
  (interactive)
  (find-file "~/.emacs.d/lisp/languages/packages-lang.el"))

(defun je/open-custom-config ()
  "Open or create `(eval je/custom-file-location)` from current custom file."
  (interactive)
  (if (not (string-equal custom-file je/custom-file-location))
      (progn
        (copy-file custom-file je/custom-file-location t)
        (setq custom-file je/custom-file-location)))
  (find-file je/custom-file-location))

(defun je/minibuffer-fringe-setup ()
  "Decouples the minibuffer's fringe from that of the main buffer."
  ;; (set-window-fringes (minibuffer-window) 0 0 nil)
  (add-hook 'minibuffer-setup-hook #'je/disable-minibuffer-fringe)
  (add-hook 'minibuffer-exit-hook #'je/disable-minibuffer-fringe))

(defun je/disable-minibuffer-fringe ()
  "Disable minibuffer fringe."
  (set-window-fringes (minibuffer-window) 0 0 nil))

(defun je/force-buffer-backup ()
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

(defun je/untabify-except-makefiles ()
  "Replace tabs with spaces except in makefiles."
  (unless (derived-mode-p 'makefile-mode)
    (untabify (point-min) (point-max))))

(defun je/switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

(defun je/cleanup-file ()
  "Remove tabs and trailing whitespace from buffer."
  (interactive)
  (je/untabify-except-makefiles)
  (delete-trailing-whitespace))

(defun je/run-grep ()
  "jegrep function. Will try `je/counsel-rg-region', \
then `je/grep-region' in order."
  (interactive)
  (if (and (executable-find "rg")
           (not (eval 'je/force-basic-grep)))
      (call-interactively 'je/counsel-rg-region)
    (if (projectile-project-p)
        (let ((default-directory (projectile-project-p)))
          (funcall 'je/grep-region))
      (funcall 'je/grep-region))))

(defun je/grep (&optional initial)
  "Gmacs grep wrapper to take optional `INITIAL' input or \
prompt for grep command."
  (if initial
      (je/build-grep-command-with-region)
    (je/build-grep-command-with-input)))

(defun je/build-grep-command-with-region ()
  "Use currently selected region to build grep command."
  (je/grep-concat-command
   (lambda ()
     (string-utils/add-quotes
      (string-utils/escape-str-for-command initial)))))

(defun je/build-grep-command-with-input ()
  "Use `read-string' to build grep command."
  (je/grep-concat-command
   (lambda ()
     (string-utils/add-quotes
      (read-string "grep regexp: ")))))

(defun je/grep-concat-command (func)
  "Constuct grep command with `FUNC' and truncate with cut."
  (grep (concat (eval grep-command) (funcall func) " | cut -c -1500")))

(defun je/counsel-rg-region ()
  "Optionally run ripgrep on region."
  (interactive)
  (je/opt-region-helper 'counsel-rg))

(defun je/counsel-git-grep-region ()
  "Optionally run `counsel-git-grep' on region."
  (interactive)
  (je/opt-region-helper
   '(lambda (&optional initial)
      (counsel-git-grep nil initial))))

(defun je/grep-region ()
  "Optionally run `grep' on region."
  (interactive)
  (je/opt-region-helper
   '(lambda (&optional initial)
      (je/grep initial))))

(defun je/counsel-projectile-find-file-region ()
  "Optionally run `counsel-git' on region."
  (interactive)
  (je/opt-region-helper 'je/counsel-git))

(defun je/counsel-projectile-find-dir-region ()
  "Optionally run `counsel-projectile-find-dir' on region."
  (interactive)
  (je/opt-region-helper 'je/counsel-projectile-find-dir))

(defun je/opt-region-helper (func)
  "Add region to kill ring and run `FUNC' with optional region arg."
  (if (use-region-p)
      (let ((string (buffer-substring-no-properties
                     (region-beginning) (region-end))))
        (progn (kill-new string)
               (deactivate-mark)
               (funcall-interactively func string)))
    (funcall-interactively func)))

(defun je/swiper-region-thing ()
  "Call `swiper' on the selected region or thing under cursor."
  (interactive)
  (if (use-region-p)
      (progn (deactivate-mark)
             (swiper (buffer-substring-no-properties
                      (region-beginning) (region-end))))
    (if (word-at-point) (swiper (word-at-point))
      (error "No region or thing selected"))))

(defun je/xref-find-definitions-symbol ()
  "`xref-find-definitions' that doesn't fall back."
  (interactive)
  (if (symbol-at-point)
      (xref-find-definitions (symbol-name (symbol-at-point)))
    (message "No symbol selected")))

(defun je/xref-find-apropos-symbol ()
  "`xref-find-apropos' that doesn't fall back."
  (interactive)
  (if (symbol-at-point)
      (xref-find-apropos (symbol-name (symbol-at-point)))
    (message "No symbol selected")))

(defun je/org-link-follow ()
  "Push marker stack and follow org link."
  (interactive)
  (if (thing-at-point-url-at-point)
      (browse-url-at-point)
    (lexical-let ((org-link-frame-setup '((file . (lambda (args) (progn (find-file args)))))))
      (call-interactively #'org-open-at-point))))

(defun je/counsel-git (&optional initial-input)
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

(defun je/counsel-projectile-find-dir (&optional initial-input)
  "Jump to a directory in the current project with \
initial input `INITIAL-INPUT'."
  (interactive)
  (counsel-require-program (car (split-string counsel-git-cmd)))
  (let* ((default-directory (expand-file-name (counsel-locate-git-root)))
         (cands (flatmap
                 'last
                 (seq-filter
                  #'je/directory-ls-tree-entry-p
                  (mapcar
                   'split-string
                   (cdr
                    (split-string
                     (shell-command-to-string je/git-ls-tree-head-cmd)
                     "\n"
                     t)))))))
    (ivy-read (projectile-prepend-project-name "Find dir: ") cands
              :initial-input initial-input
              :action #'counsel-projectile-find-dir-action
              :caller 'counsel-projectile-find-dir)))

(defun je/magit-status ()
  "Wrap `magit-status' with `projectile-project-p'."
  (interactive)
  (if (not (projectile-project-p))
      (error "Not in a git repository")
    (magit-status)))

(defun je/toggle-spelling ()
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

(defun je/unhighlight-all ()
  "Unhighlight all currently highlighted symbols and \
disable command `hi-lock-mode'."
  (interactive)
  (unhighlight-regexp t)
  (hi-lock-mode 0))

(defun je/switch-to-scratch-buffer ()
  "Switch to initial scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun je/switch-to-messages-buffer ()
  "Switch to Messages buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun je/eshell-send-eof ()
  "Send EOF to Eshell with newline."
  (interactive)
  (call-interactively 'newline)
  (call-interactively 'eshell-send-eof-to-process))

(defun je/projectile-root-dir ()
  "Jump to the root directory of the current project."
  (interactive)
  (if (not (projectile-project-p))
      (error "Not in a git repository")
    (dired (projectile-project-root))))

(defun je/open-home-dir ()
  "Open ~."
  (interactive)
  (dired "~"))

(defun je/create-visit-dir (dir)
  "Open/create `DIR'."
  (if (file-directory-p dir)
      (dired dir)
    (progn
      (make-directory dir t)
      (dired dir))))

(defun je/open-org-dir ()
  "Open ~/org."
  (interactive)
  (je/create-visit-dir "~/org"))

(defun je/open-downloads-dir ()
  "Open ~/Downloads."
  (interactive)
  (je/create-visit-dir "~/Downloads"))

(defun je/open-code-dir ()
  "Open ~/code."
  (interactive)
  (je/create-visit-dir "~/code"))

(defun je/company-cancel-complete-prev ()
  "Exit company mode and use evil complete to autocomplete upwards."
  (interactive)
  (company-abort)
  (evil-complete-previous))

(defun je/company-cancel-complete-next ()
  "Exit company mode and use evil complete to autocomplete downwards."
  (interactive)
  (company-abort)
  (evil-complete-next))

(defun je/counsel-func-eshell-history (msg func)
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

(defun je/counsel-insert-eshell-history ()
  "Insert at point from eshell history."
  (interactive)
  (je/counsel-func-eshell-history
   "insert eshell history: "
   (lambda (v) (insert v))))

(defun je/counsel-yank-eshell-history ()
  "Yank from eshell history."
  (interactive)
  (je/counsel-func-eshell-history
   "yank eshell history: "
   (lambda (v)
     (progn
       (kill-new v)
       (message "%s yanked" v)))))

(defun je/eshell-clear ()
  "Clear terminal."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq-local je/eshell-message (string-trim je/eshell-message))
    (eshell-banner-initialize)
    (eshell-send-input)))

(defun je/eshell-top-prompt-function ()
  "Builds the top line of the jeemacs eshell prompt."
  (concat "[" (abbreviate-file-name (eshell/pwd)) "]"))

(defun je/eshell-bottom-prompt-function ()
  "Builds the bottom line of the jeemacs eshell prompt."
  (if (= (user-uid) 0) "# " "$ "))

(defun je/eshell-prompt-function ()
  "Builds the Eshell prompt string. Make sure to \
update `je/eshell-prompt-regexp' so that it will \
match your prompt."
  (concat "\n" (je/eshell-top-prompt-function) " \n"
          (je/eshell-bottom-prompt-function)))

(defun je/mc-evil-emacs-state ()
  "When using multiple-cursors, switch to Emacs state."
  (if (region-active-p)
      (delete-selection-mode 1))
  (evil-emacs-state 1))

(defun je/mc-evil-normal-state ()
  "When done using multiple-cursors, switch to normal mode."
  (delete-selection-mode 0)
  (evil-normal-state 1))

(defun je/shrink-window-horizontally ()
  "Shrink the active window horizontally."
  (interactive)
  (let ((current-prefix-arg `(4)))
    (call-interactively 'shrink-window-horizontally)))

(defun je/enlarge-window-horizontally ()
  "Enlarge the active window horizontally."
  (interactive)
  (let ((current-prefix-arg `(4)))
    (call-interactively 'enlarge-window-horizontally)))

(defun je/enlarge-window ()
  "Enlarge the active window vertically."
  (interactive)
  (let ((current-prefix-arg `(4)))
    (call-interactively 'enlarge-window)))

(defun je/shrink-window ()
  "Shrink the active window vertically."
  (interactive)
  (let ((current-prefix-arg `(4)))
    (call-interactively 'shrink-window)))

(defun je/dont-kill-scratch-or-dired ()
  "Don't kill but bury *scratch* and \"dired:\" buffers."
  (if (or (string-match-p "dired:" (buffer-name))
          (equal (buffer-name (current-buffer)) "*scratch*"))
      (progn (bury-buffer) nil)
    t))

(defun je/emacs-lisp-setup ()
  "Setup Emacs Lisp Mode."
  (add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
  (add-hook 'lisp-interaction-mode-hook 'prettify-symbols-mode)
  (push '(company-capf company-yasnippet) company-backends))

(defun je/evil-eshell-mode-setup ()
  "Setup Jeeshell."
  (setq-local inhibit-read-only t)
  (je/eshell-def-evil-eshell-delete)
  (je/jeeshell-evil-mode-keys-setup))

(defun je/jeeshell-evil-mode-keys-setup ()
  "Setup Jeeshell evil-mode keys."
  (evil-define-key 'normal eshell-mode-map (kbd "d") 'evil-eshell-delete)
  (define-key evil-normal-state-local-map (kbd "M-r") 'je/counsel-yank-eshell-history)
  (define-key evil-insert-state-local-map (kbd "M-r") 'je/counsel-insert-eshell-history)
  (define-key evil-normal-state-local-map (kbd "C-l") 'je/eshell-clear)
  (define-key evil-insert-state-local-map (kbd "C-l") 'je/eshell-clear)
  (define-key evil-insert-state-local-map (kbd "C-d") 'je/eshell-send-eof-kill-on-empty-prompt)
  (define-key evil-insert-state-local-map (kbd "C-i") 'eshell-pcomplete)
  (define-key evil-insert-state-local-map (kbd "C-c C-d") 'je/eshell-send-eof)
  (define-key evil-normal-state-local-map (kbd "C-c C-d") 'je/eshell-send-eof)
  (define-key evil-insert-state-local-map (kbd "C-k") 'eshell-life-is-too-much)
  (define-key evil-normal-state-local-map (kbd "C-k") 'eshell-life-is-too-much)
  (define-key evil-normal-state-local-map (kbd "RET") 'eshell-send-input)
  (define-key evil-normal-state-local-map (kbd "C-j") 'eshell-send-input)
  (define-key evil-normal-state-local-map (kbd "C-m") 'eshell-send-input)
  (define-key evil-insert-state-local-map (kbd "RET") 'eshell-send-input)
  (define-key evil-insert-state-local-map (kbd "C-j") 'eshell-send-input)
  (define-key evil-insert-state-local-map (kbd "C-m") 'eshell-send-input))

(defun je/eshell-def-evil-eshell-delete ()
  "Define custom delete operation for Eshell when on the line with the prompt."
  (evil-define-operator evil-eshell-delete (beg end type register yank-handler)
    "Like evil-delete, but inhibit read only and when the eshell prompt is \
involved re-emit it."
    (interactive "<R><x><y>")
    (let ((inhibit-read-only t)
          (total-prompt-length (length (je/eshell-prompt-function)))
          (bottom-prompt-length (length (je/eshell-bottom-prompt-function))))
      (cond
       ((je/looking-at-eshell-prompt-regexp-p beg)
        (progn
          (evil-delete
           (+ beg bottom-prompt-length)
           end type register yank-handler)
          (delete-region
           (- (+ beg bottom-prompt-length) total-prompt-length)
           (+ beg bottom-prompt-length))
          (eshell-emit-prompt)))
       ((je/looking-at-eshell-top-prompt-regexp-p beg) (void))
       (t (evil-delete beg end type register yank-handler))))))

(defun je/looking-at-eshell-prompt-regexp-p (loc)
  "Truthy value for evil-eshell-delete which determines if \
the `LOC' is `looking-at-p' `je/eshell-prompt-regexp'."
  (save-excursion
    (goto-char loc)
    (looking-at-p je/eshell-prompt-regexp)))

(defun je/looking-at-empty-eshell-prompt-regexp-p (loc)
  "Truthy value for evil-eshell-delete which determines if \
the `LOC' is `looking-at-p' `je/eshell-prompt-regexp' \
followed by nothing."
  (save-excursion
    (goto-char loc)
    (looking-at-p (concat je/eshell-prompt-regexp "$"))))

(defun je/eshell-send-eof-kill-on-empty-prompt ()
  "Send eshell-life-is-too-much if there is no pending \
eshell command string, and EOF if there is a pending command string."
  (interactive)
  (if (je/looking-at-empty-eshell-prompt-p)
      (eshell-life-is-too-much)
    (je/eshell-send-eof)))

(defun je/looking-at-empty-eshell-prompt-p ()
  "Test if looking an empty eshell prompt."
  (let ((beginning-of-line-pos
         (progn
           (save-excursion
             (call-interactively 'move-beginning-of-line)
             (point)))))
    (je/looking-at-empty-eshell-prompt-regexp-p beginning-of-line-pos)))

(defun je/looking-at-eshell-top-prompt-regexp-p (loc)
  "Truthy value for evil-eshell-delete which determines if \
the `LOC' is `looking-at-p' `je/eshell-top-prompt-regexp'."
  (save-excursion
    (goto-char loc)
    (looking-at-p je/eshell-top-prompt-regexp)))

(defun je/evil-minibuffer-setup ()
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

(defun je/evil-org-mode-setup ()
  "Setup org mode."
  (define-key evil-normal-state-local-map (kbd "M-i") 'org-cycle)
  (define-key evil-normal-state-local-map (kbd "C-M-l") 'je/org-cycle-list-bullet-forward)
  (define-key evil-normal-state-local-map (kbd "C-M-h") 'je/org-cycle-list-bullet-backward)
  (define-key evil-insert-state-local-map (kbd "C-d") 'evil-shift-left-line))

(defun je/org-cycle-list-bullet-forward ()
  "Cycle org list bullet type forward."
  (interactive)
  (funcall-interactively 'org-cycle-list-bullet 'nil))

(defun je/org-cycle-list-bullet-backward ()
  "Cycle org list bullet type backward."
  (interactive)
  (funcall-interactively 'org-cycle-list-bullet 'previous))

(defun je/evil-c-common-mode-setup ()
  "Setup C mode and it's derivatives."
  (define-key evil-normal-state-local-map (kbd "M-j") 'c-indent-new-comment-line)
  (define-key evil-insert-state-local-map (kbd "M-j") 'c-indent-new-comment-line)
  (define-key evil-normal-state-local-map (kbd "M-m") 'c-indent-new-comment-line)
  (define-key evil-insert-state-local-map (kbd "M-m") 'c-indent-new-comment-line))

(defun je/evil-emmet-mode-setup ()
  "Setup emmet-mode for evil."
  (define-key evil-normal-state-local-map (kbd "M-j") 'emmet-expand-line)
  (define-key evil-insert-state-local-map (kbd "M-j") 'emmet-expand-line)
  (define-key evil-normal-state-local-map (kbd "M-l") 'emmet-next-edit-point)
  (define-key evil-insert-state-local-map (kbd "M-l") 'emmet-next-edit-point)
  (define-key evil-normal-state-local-map (kbd "M-h") 'emmet-prev-edit-point)
  (define-key evil-insert-state-local-map (kbd "M-h") 'emmet-prev-edit-point))

(defun je/evil-company-abort-on-insert-leave ()
  "If company mode is currently enabled, run company abort. \
Note: effective as an evil-insert-state-exit-hook."
  (if (bound-and-true-p company-mode)
      (company-abort)))

(defun je/python-mode-hook ()
  "Python mode setup."
  (prettify-symbols-mode 1)
  (message nil))

(defun je/prompt-maybe-run (confirmed-var question enabled-var init-func)
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

(defun je/toggle-truncate-lines-mode-no-message (arg)
  "Run `toggle-truncate-lines' with `ARG' and swallow the message."
  (progn (toggle-truncate-lines arg)
         (message nil)))

(defun je/disable-truncate-lines-no-message ()
  "Disable `toggle-truncate-lines' and swallow the message."
  (je/toggle-truncate-lines-mode-no-message 0))

(defun je/enable-truncate-lines-no-message ()
  "Enable `toggle-truncate-lines' and swallow the message."
  (je/toggle-truncate-lines-mode-no-message 1))

(defun je/add-xref-js2-xref-backend ()
  "Add `js2-xref-backend' to `xref-backend-functions'."
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))

(defun je/enable-company-mode ()
  "Enable Company Mode."
  (make-variable-buffer-local 'company-backends)
  (company-mode))

(defun je/evil-command-window-setup ()
  "Setup Evil command window."
  (define-key evil-normal-state-local-map (kbd "M-:") 'evil-window-delete))

(defun je/rename-dired-buffer ()
  "Rename Dired buffers so that they can be referred to in the Evil Jumplist."
  (interactive)
  (unless (string-match-p (concat "dired" uniquify-separator) (buffer-name))
    (rename-buffer (concat "dired" uniquify-separator (generate-new-buffer-name dired-directory)))))

(defun je/directory-ls-tree-entry-p (entry)
  "Function to `mapcar' to filter `ENTRY' in 'git ls-tree' for directories."
  (string= (nth 1 entry) "tree"))

(defun je/shell-kill-buffer-on-exit-sentinel ()
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

(defun je/web-mode-setup ()
  "Configure `web-mode'."
  (sp-local-pair 'web-mode "<" "")
  (emmet-mode 1))

(defun je/auto-revert-mode-setup ()
  "Configure `auto-revert-mode' setup."
  (diminish 'auto-revert-mode))

(defun je/evil-jumplist-setup ()
  "Configure evil-jumplist."
  (setq evil--jumps-buffer-targets "\\(\\*\\(\\new\\|scratch\\)\\*\\|dired:.+\\)")
  (evil-add-command-properties #'dired-find-file :jump t))

(defun je/org-indent-setup ()
  "Configure `org-intent-mode'."
  (diminish 'org-indent-mode "in"))

(defun je/cider-repl-mode-setup ()
  "Configure `cider-repl-mode'"
  (define-key evil-normal-state-local-map (kbd "C-m") 'cider-repl-return)
  (define-key evil-insert-state-local-map (kbd "C-m") 'cider-repl-return)
  (define-key evil-normal-state-local-map (kbd "M-m") 'cider-repl-return)
  (define-key evil-insert-state-local-map (kbd "M-m") 'cider-repl-return))

(defun je/cider-deps-p ()
  (and (executable-find "clj") (executable-find "lein")))

(defun je/cider-mode-enabled-p ()
  (bound-and-true-p cider-mode))

(defun je/should-not-be-in-mode-line-p (e)
  (not (member e je/modeline-blacklist)))

(defun je/create-mode-line-format ()
  (seq-filter 'je/should-not-be-in-mode-line-p mode-line-format))

(defun je/set-window-dimensions (x-pos y-pos width height)
  (when window-system
    (progn
      (set-frame-position (selected-frame) x-pos y-pos)
      (set-frame-size (selected-frame) width height))))

(provide 'functions)
;;; functions.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars cl-functions make-local unresolved)
;; End:
