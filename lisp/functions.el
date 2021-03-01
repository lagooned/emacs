;;; functions.el --- custom jeemacs functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Jared M. Engler

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

;; Je/Emacs custom function definitions.

;;; Code:

(require 'seq)
(require 'functional)
(require 'cl-seq)
(require 'string-utils)
(require 'subr-x)

(defun void ()
  "Interactive No-op."
  (interactive))

(defun je/ensure-use-package ()
  "Install `use-package' if not installed."
  (if (not (package-installed-p 'use-package))
      (progn (package-refresh-contents)
             (package-install 'use-package))))

(defun je/reset-gc-threshold-percentage ()
  "Reset `gc-cons-threshold' and `gc-cons-percentage' \
to sane runtime defaults."
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1))

(defun je/set-emacs-d-default-directory ()
  "Set ~/.emacs.d to default directory."
  (setq default-directory "~/.emacs.d/"))

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

(defun je/open-global-config ()
  "Open global.el."
  (interactive)
  (find-file "~/.emacs.d/lisp/global.el"))

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

(defun je/cleanup-whitespace ()
  "Remove tabs and trailing whitespace from buffer."
  (interactive)
  (je/untabify-except-makefiles)
  (delete-trailing-whitespace)
  (message "file untabified and trailing whitespace removed"))

(defun je/cleanup-indent ()
  "Properly indent buffer."
  (interactive)
  (indent-region (point-min) (point-max))
  (message "file properly indented"))

(defun je/run-grep ()
  "Start grepping."
  (interactive)
  (if (projectile-project-p)
      (let ((default-directory (projectile-project-p)))
        (funcall 'je/grep-region))
    (funcall 'je/grep-region)))

(defun je/grep (&optional initial)
  "Je/Emacs grep wrapper to take optional `INITIAL' input or \
prompt for grep command."
  (if initial
      (je/build-grep-command-with-func
       (string-utils/escape-str-for-command initial))
    (je/build-grep-command-with-func
     (read-string "grep regexp: "))))

(defun je/build-grep-command-with-func (func)
  "Use `FUNC' to build grep command."
  (je/grep-concat-command (lambda () (string-utils/add-quotes func))))

(defun je/grep-concat-command (func)
  "Constuct grep command with `FUNC' and truncate with cut."
  (grep (concat (eval grep-command) (funcall func) " | cut -c -1500")))

(defun je/counsel-git-grep-region ()
  "Optionally run `counsel-git-grep' on region."
  (interactive)
  (je/opt-region-helper
   '(lambda (&optional initial)
      (counsel-git-grep initial))))

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

(defun je/org-link-follow ()
  "Push marker stack and follow org link."
  (interactive)
  (if (thing-at-point-url-at-point)
      (browse-url-at-point)
    (let ((org-link-frame-setup '((file . (lambda (args) (progn (find-file args)))))))
      (call-interactively #'org-open-at-point))))

(defun je/counsel-git (&optional initial-input)
  "Find file in the current Git repository. `INITIAL-INPUT' \
can be given as the initial minibuffer input."
  (interactive)
  (counsel-require-program (car (split-string counsel-git-cmd)))
  (let* ((default-directory (expand-file-name (counsel-locate-git-root)))
         (cands (split-string
                 (shell-command-to-string "git ls-files --full-name -- ")
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
         (cands (--je/unwrap-inner-lists-on
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

(defsubst --je/unwrap-inner-lists (list)
  (mapcan (lambda (x) (if (listp x) x nil)) list))

(defun --je/unwrap-inner-lists-on (func list)
  (--je/unwrap-inner-lists (mapcar func list)))

(defun je/projectile-vc ()
  "Wrap `projectile-vc' with `projectile-project-p'."
  (interactive)
  (if (not (projectile-project-p))
      (error "Not in a vc repository")
    (projectile-vc)))

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

(defun je/switch-to-dashboard-buffer ()
  "Switch to dashboard buffer."
  (interactive)
  (switch-to-buffer "*dashboard*"))

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
    (eshell-banner-initialize)
    (eshell-send-input)))

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

(defun je/dont-kill-scratch ()
  "Don't kill but bury *scratch* and \"dired:\" buffers."
  (if (equal (buffer-name (current-buffer)) "*scratch*")
      (progn (bury-buffer) nil)
    t))

(defun je/dont-kill-dired ()
  "Don't kill but bury \"dired:\" buffers."
  (if (string-match-p "dired:" (buffer-name))
      (progn (bury-buffer) nil)
    t))

(defun je/dont-kill-dashboard ()
  "Don't kill but bury *dashboard* buffer.."
  (if (equal (buffer-name (current-buffer)) "*dashboard*")
      (progn (bury-buffer) nil)
    t))

(defun je/evil-eshell-mode-setup ()
  "Setup Je/eshell."
  (evil-set-initial-state 'eshell-mode 'emacs)
  (je/eshell-evil-mode-keys-setup))

(defun je/eshell-evil-mode-keys-setup ()
  "Setup Je/eshell evil-mode keys."
  (define-key evil-emacs-state-local-map (kbd "C-c C-r") 'je/counsel-insert-eshell-history)
  (define-key evil-emacs-state-local-map (kbd "C-c C-l") 'je/eshell-clear)
  (define-key evil-emacs-state-local-map (kbd "C-c C-d") 'je/eshell-send-eof-kill-on-empty-prompt)
  (define-key evil-emacs-state-local-map (kbd "RET") 'eshell-send-input)
  (define-key evil-emacs-state-local-map (kbd "C-m") 'eshell-send-input))

(defun je/eshell-send-eof-kill-on-empty-prompt ()
  "Send eshell-life-is-too-much if there is no pending \
eshell command string, and EOF if there is a pending command string."
  (interactive)
  (if (je/on-empty-eshell-prompt-line-p)
      (eshell-life-is-too-much)
    (je/eshell-send-eof)))

(defun je/on-empty-eshell-prompt-line-p ()
  (string-match (string-trim-right eshell-prompt-regexp)
                (string-trim (thing-at-point 'line t))))

(defun je/evil-minibuffer-setup ()
  "Setup the minibuffer."
  (evil-emacs-state)
  (define-key evil-emacs-state-local-map (kbd "M-m") 'void)
  (define-key evil-emacs-state-local-map (kbd "M-j") 'void)
  (define-key evil-emacs-state-local-map (kbd "C-s") 'void)
  (define-key evil-emacs-state-local-map (kbd "C-j") 'ivy-alt-done)
  (define-key evil-emacs-state-local-map (kbd "M-o") 'ivy-dispatching-done)
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
  (define-key evil-emacs-state-local-map (kbd "C-M-h") 'ivy-help)
  (define-key evil-emacs-state-local-map (kbd "C-u") 'ivy-scroll-down-command)
  (define-key evil-emacs-state-local-map (kbd "C-d") 'ivy-scroll-up-command))

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

(defun je/evil-jumplist-setup ()
  "Configure evil-jumplist."
  (setq evil--jumps-buffer-targets "\\(\\*\\(\\new\\|scratch\\)\\*\\|dired:.+\\)")
  (evil-add-command-properties #'dired-find-file :jump t))

(defun je/org-indent-setup ()
  "Configure `org-intent-mode'."
  (diminish 'org-indent-mode "in"))

(defun je/cider-repl-mode-setup ()
  "Configure `cider-repl-mode'."
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

(defun je/setup-elscreen ()
  "Setup elscreen."
  (evil-leader/set-key
    "1"   'elscreen-jump
    "2"   'elscreen-jump
    "3"   'elscreen-jump
    "4"   'elscreen-jump
    "5"   'elscreen-jump
    "6"   'elscreen-jump
    "7"   'elscreen-jump
    "8"   'elscreen-jump
    "9"   'elscreen-jump
    "0"   'elscreen-jump
    "z z" 'elscreen-toggle
    "z l" 'elscreen-display-screen-name-list
    "z c" 'elscreen-create
    "z C" 'elscreen-clone
    "z k" 'elscreen-kill
    "z r" 'elscreen-screen-nickname
    "z K" 'elscreen-kill-others
    "z n" 'elscreen-next
    "z p" 'elscreen-previous
    "z ;" 'elscreen-execute-extended-command
    "z d" 'elscreen-dired
    "z s" 'elscreen-swap)
  (which-key-add-key-based-replacements
    "SPC z" "screen"))

(defun je/configure-elisp-company-backends ()
  (setq company-backends `(company-capf company-yasnippet)))

(defun je/java-lsp-deps-p ()
  (and (not (or (eq system-type 'windows-nt)
                (eq system-type 'cygwin)))
       (executable-find "java")))

(defun je/configure-evil-collection-mode-list ()
  (setq evil-collection-mode-list (remove 'company evil-collection-mode-list))
  (setq evil-collection-mode-list (remove 'eshell evil-collection-mode-list))
  (setq evil-collection-mode-list (remove `(term term ansi-term multi-term) evil-collection-mode-list)))

(defun je/print-to-file (filename data)
  (with-temp-file filename
    (insert data)))

(defun je/unbindall ()
  (global-unset-key (kbd "C-k"))
  (global-unset-key (kbd "C-l"))
  (global-unset-key (kbd "C-w"))
  (global-unset-key (kbd "C-s"))
  (global-unset-key (kbd "C-y"))
  (global-unset-key (kbd "C-b"))
  (global-unset-key (kbd "C-f"))
  (global-unset-key (kbd "C-/"))
  (global-unset-key (kbd "C-_"))
  (global-unset-key (kbd "M-j"))
  (global-unset-key (kbd "M-a"))
  (global-unset-key (kbd "M-b"))
  (global-unset-key (kbd "M-e"))
  (global-unset-key (kbd "M-w"))
  (global-unset-key (kbd "M-d"))
  (global-unset-key (kbd "M-m"))
  (global-unset-key (kbd "M-k"))
  (global-unset-key (kbd "M-u"))
  (global-unset-key (kbd "M-v"))
  (global-unset-key (kbd "M-l"))
  (global-unset-key (kbd "M-i"))
  (global-unset-key (kbd "M-c"))
  (global-unset-key (kbd "M-t"))
  (global-unset-key (kbd "M-h"))
  (global-unset-key (kbd "M-r"))
  (global-unset-key (kbd "M-m"))
  (global-unset-key (kbd "M-."))
  (global-unset-key (kbd "M-^"))
  (global-unset-key (kbd "M-,"))
  (global-unset-key (kbd "M-;"))
  (global-unset-key (kbd "M-?"))
  (global-unset-key (kbd "M-z"))
  (global-unset-key (kbd "M-!"))
  (global-unset-key (kbd "M-@"))
  (global-unset-key (kbd "M-&"))
  (global-unset-key (kbd "M->"))
  (global-unset-key (kbd "M-<"))
  (global-unset-key (kbd "C-x C-u"))
  (global-unset-key (kbd "C-x C-l"))
  (global-unset-key (kbd "C-M-j"))
  (global-unset-key (kbd "C-M-k"))
  (global-unset-key (kbd "C-M-v"))
  (global-unset-key (kbd "C-M-h"))
  (global-unset-key (kbd "C-M-u"))
  (global-unset-key (kbd "C-M-d"))
  (global-unset-key (kbd "C-M-f"))
  (global-unset-key (kbd "C-M-e"))
  (global-unset-key (kbd "C-M-b"))
  (global-unset-key (kbd "C-M-a"))
  (global-unset-key (kbd "C-M-t"))
  (global-unset-key (kbd "C-M-l"))
  (global-unset-key (kbd "C-M-n"))
  (global-unset-key (kbd "C-M-p"))
  (global-unset-key (kbd "C-M-/"))
  (global-unset-key (kbd "C-M-."))
  (global-unset-key (kbd "M-o"))
  (global-unset-key (kbd "C-M-o")))

(defun je/evil-insert-binds ()
  (define-key evil-insert-state-map (kbd "C-j") 'newline)
  (define-key evil-insert-state-map (kbd "C-m") 'newline-and-indent)
  (define-key evil-insert-state-map (kbd "M-j") 'indent-new-comment-line)
  (define-key evil-insert-state-map (kbd "M-m") 'indent-new-comment-line)
  (define-key evil-insert-state-map (kbd "M-y") 'counsel-yank-pop)
  (define-key evil-insert-state-map (kbd "RET") 'newline-and-indent)
  (define-key evil-insert-state-map (kbd "C-f") 'evil-forward-char)
  (define-key evil-insert-state-map (kbd "C-b") 'evil-backward-char)
  (define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop))

(defun je/evil-motion-binds ()
  (define-key evil-motion-state-map (kbd "C-=") 'er/expand-region)
  (define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up))

(defun je/evil-normal-binds ()
  (define-key evil-normal-state-map (kbd "-") 'dired-jump)
  (define-key evil-normal-state-map (kbd "C-/") 'comment-line)
  (define-key evil-normal-state-map (kbd "C-_") 'comment-line)
  (define-key evil-normal-state-map (kbd "gcc") 'comment-line)
  (define-key evil-normal-state-map (kbd "C-=") 'er/expand-region)
  (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-b") 'evil-scroll-page-up)
  (define-key evil-normal-state-map (kbd "C-j") 'newline)
  (define-key evil-normal-state-map (kbd "C-m") 'newline-and-indent)
  (define-key evil-normal-state-map (kbd "C-s") 'swiper)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "G") 'end-of-buffer)
  (define-key evil-normal-state-map (kbd "M-j") 'indent-new-comment-line)
  (define-key evil-normal-state-map (kbd "M-m") 'indent-new-comment-line)
  (define-key evil-normal-state-map (kbd "RET") 'newline-and-indent)
  (define-key evil-normal-state-map (kbd "U") 'undo-tree-visualize)
  (define-key evil-normal-state-map (kbd "gg") 'beginning-of-buffer)
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "gr") 'revert-buffer))

(defun je/evil-visual-binds ()
  (define-key evil-visual-state-map (kbd "$") 'evil-end-of-visual-line)
  (define-key evil-visual-state-map (kbd "C-/") 'comment-dwim)
  (define-key evil-visual-state-map (kbd "C-_") 'comment-dwim)
  (define-key evil-visual-state-map (kbd "gcc") 'comment-dwim)
  (define-key evil-visual-state-map (kbd "C-=") 'er/expand-region)
  (define-key evil-visual-state-map (kbd "C-j") 'void)
  (define-key evil-visual-state-map (kbd "G") 'end-of-buffer)
  (define-key evil-visual-state-map (kbd "RET") 'void)
  (define-key evil-visual-state-map (kbd "gg") 'beginning-of-buffer)
  (define-key evil-visual-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-visual-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-visual-state-map (kbd "M-j") 'void)
  (define-key evil-visual-state-map (kbd "M-m") 'void))

(defun je/evil-emacs-binds ()
  (define-key evil-emacs-state-map (kbd "C-k") 'kill-line)
  (define-key evil-emacs-state-map (kbd "C-s") 'isearch-forward)
  (define-key evil-emacs-state-map (kbd "C-w") 'kill-region)
  (define-key evil-emacs-state-map (kbd "C-y") 'yank)
  (define-key evil-emacs-state-map (kbd "C-f") 'forward-char)
  (define-key evil-emacs-state-map (kbd "C-b") 'backward-char)
  (define-key evil-emacs-state-map (kbd "M-r") 'move-to-window-line-top-bottom)
  (define-key evil-emacs-state-map (kbd "M-a") 'backward-sentence)
  (define-key evil-emacs-state-map (kbd "M-b") 'backward-word)
  (define-key evil-emacs-state-map (kbd "M-e") 'forward-sentence)
  (define-key evil-emacs-state-map (kbd "M-w") 'kill-ring-save)
  (define-key evil-emacs-state-map (kbd "M-d") 'kill-word)
  (define-key evil-emacs-state-map (kbd "M-v") 'scroll-down-command)
  (define-key evil-emacs-state-map (kbd "M-m") 'back-to-indentation)
  (define-key evil-emacs-state-map (kbd "M-j") 'back-to-indentation)
  (define-key evil-emacs-state-map (kbd "M-k") 'kill-sentence)
  (define-key evil-emacs-state-map (kbd "M-u") 'fix-word-upcase)
  (define-key evil-emacs-state-map (kbd "M-l") 'fix-word-downcase)
  (define-key evil-emacs-state-map (kbd "M-c") 'fix-word-capitalize)
  (define-key evil-emacs-state-map (kbd "M-z") 'zop-to-char)
  (define-key evil-emacs-state-map (kbd "M-.") 'xref-find-definitions)
  (define-key evil-emacs-state-map (kbd "M-&") 'async-shell-command)
  (define-key evil-emacs-state-map (kbd "M-^") 'delete-indentation)
  (define-key evil-emacs-state-map (kbd "M-,") 'xref-pop-marker-stack)
  (define-key evil-emacs-state-map (kbd "M-?") 'xref-find-references)
  (define-key evil-emacs-state-map (kbd "M-@") 'mark-word)
  (define-key evil-emacs-state-map (kbd "M-!") 'shell-command)
  (define-key evil-emacs-state-map (kbd "M-<") 'beginning-of-buffer)
  (define-key evil-emacs-state-map (kbd "M->") 'end-of-buffer)
  (define-key evil-emacs-state-map (kbd "M-o b") 'facemenu-set-bold)
  (define-key evil-emacs-state-map (kbd "M-o d") 'facemenu-set-default)
  (define-key evil-emacs-state-map (kbd "M-o i") 'facemenu-set-italic)
  (define-key evil-emacs-state-map (kbd "M-o l") 'facemenu-set-bold-italic)
  (define-key evil-emacs-state-map (kbd "M-o o") 'facemenu-set-face)
  (define-key evil-emacs-state-map (kbd "M-o u") 'facemenu-set-underline)
  (define-key evil-emacs-state-map (kbd "M-o M-o") 'font-lock-fontify-block)
  (define-key evil-emacs-state-map (kbd "M-o M-s") 'center-line)
  (define-key evil-emacs-state-map (kbd "M-o M-S") 'center-paragraph)
  (define-key evil-emacs-state-map (kbd "C-x C-u") 'upcase-region)
  (define-key evil-emacs-state-map (kbd "C-x C-l") 'downcase-region)
  (define-key evil-emacs-state-map (kbd "C-M-h") 'mark-defun)
  (define-key evil-emacs-state-map (kbd "C-M-d") 'down-list)
  (define-key evil-emacs-state-map (kbd "C-M-u") 'backward-up-list)
  (define-key evil-emacs-state-map (kbd "C-M-n") 'forward-list)
  (define-key evil-emacs-state-map (kbd "C-M-p") 'backward-list)
  (define-key evil-emacs-state-map (kbd "C-M-f") 'forward-sexp)
  (define-key evil-emacs-state-map (kbd "C-M-b") 'backward-sexp)
  (define-key evil-emacs-state-map (kbd "C-M-k") 'kill-sexp)
  (define-key evil-emacs-state-map (kbd "C-M-v") 'scroll-other-window)
  (define-key evil-emacs-state-map (kbd "C-M-e") 'end-of-defun)
  (define-key evil-emacs-state-map (kbd "C-M-a") 'beginning-of-defun)
  (define-key evil-emacs-state-map (kbd "C-M-t") 'transpose-sexps)
  (define-key evil-emacs-state-map (kbd "C-M-l") 'reposition-window)
  (define-key evil-emacs-state-map (kbd "C-=") 'er/expand-region)
  (define-key evil-emacs-state-map (kbd "M-;") 'comment-dwim)
  (define-key evil-emacs-state-map (kbd "M-i") 'tab-to-tab-stop))

(defun je/evil-ex-binds ()
  (define-key evil-ex-completion-map (kbd "C-b") 'backward-char)
  (define-key evil-ex-completion-map (kbd "C-d") 'delete-char)
  (define-key evil-ex-completion-map (kbd "C-a") 'move-beginning-of-line))

(provide 'functions)
;;; functions.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars cl-functions make-local unresolved)
;; End:
