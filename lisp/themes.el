;;;;;;;;;;;;
;; THEMES ;;
;;;;;;;;;;;;

(use-package remember-theme
  :init
  (add-hook 'kill-emacs-hook 'remember-theme-save)
  (remember-theme-load)
  (setq remember-theme-file "~/.emacs.d/.last-theme"))

(use-package solarized-theme
  :init
  (setq solarized-use-variable-pitch nil)
  (setq solarized-high-contrast-mode-line t)
  (setq solarized-use-less-bold t)
  (setq solarized-emphasize-indicators nil)
  (setq solarized-scale-org-headlines nil)
  (setq solarized-height-minus-1 1)
  (setq solarized-height-plus-1 1)
  (setq solarized-height-plus-2 1)
  (setq solarized-height-plus-3 1)
  (setq solarized-height-plus-4 1))

(use-package monokai-theme
  :init
  (setq monokai-height-plus-2 1.0
		monokai-height-plus-2 1.0
		monokai-height-plus-3 1.0
		monokai-height-plus-4 1.0
		monokai-height-minus-1 1.0)
  (setq monokai-use-variable-pitch nil))
