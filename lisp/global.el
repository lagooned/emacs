;;;;;;;;;;;;;;;;;;
;; GLOBAL SETUP ;;
;;;;;;;;;;;;;;;;;;

;; set default warning level
(setq warning-minimum-level :emergency)

;; setup user
(setq user-full-name "Jared M. Engler"
      user-mail-address "jared.lite@gmail.com"
      calendar-latitude 40.52
      calendar-longitude -88.99
      calendar-location-name "Normal, IL")

;; frame title format
(setq frame-title-format
  '("Emacs - " (buffer-file-name "%f"
    (dired-directory dired-directory "%b"))))

;; backup files to temp
(setq backup-directory-alist `((".*" . , temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" , temporary-file-directory t)))

;; unique buffer names
(require 'uniquify)
(setq 
  uniquify-buffer-name-style 'reverse
  uniquify-after-kill-buffer-p t
  uniquify-separator ":"
  uniquify-ignore-buffers-re "^\\*")

;; bury scratch on kill
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
	(if (equal buffer-to-kill "*scratch*")
		(bury-buffer)
	  ad-do-it)))

;; winner mode
(when (fboundp 'winner-mode)
    (winner-mode 1))

;; no tool bar
(tool-bar-mode -1)

;; no scroll bar
(scroll-bar-mode -1) 

;; symbols
(global-prettify-symbols-mode t)

;; show parens
(show-paren-mode 1)
(setq show-paren-delay 0)

;; set default tab display width to 4 spaces
(setq-default tab-width 4) 
(setq-default c-basic-offset 4)

;; electric indent
(electric-indent-mode 1)

;; electric pair
(electric-pair-mode 1)

;; reload init 
(global-set-key (kbd "C-c i") (lambda() (interactive)(load-file "~/.emacs.d/init.el")))

;; make prompts easier
(defalias 'yes-or-no-p 'y-or-n-p)

;; highlight line
(when window-system
  (global-hl-line-mode))

;; random binds
(global-set-key (kbd "C-;") 'comment-line)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c C-k") 'compile)

;; platform specific options
(when (eq system-type 'darwin)
    ;; railwaycat/homebrew-emacsmacport
    (set-face-attribute 'default nil :family "Source Code Pro")
    (set-face-attribute 'default nil :height 130)

    ;; disable the menu bar => no full screen
    ;; (menu-bar-mode -1)
) 

(when (eq system-type 'gnu/linux)
    (set-face-attribute 'default nil :family "Ubuntu Mono")
    (set-face-attribute 'default nil :height 110)
    (menu-bar-mode -1)
) 

(when (eq system-type 'windows-nt)
    (set-face-attribute 'default nil :family "Consolas")
    (set-face-attribute 'default nil :height 130)
    (menu-bar-mode -1)
) 
