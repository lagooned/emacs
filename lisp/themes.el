;;;;;;;;;;;;
;; THEMES ;;
;;;;;;;;;;;;

(use-package remember-theme
  :init
  (add-hook 'kill-emacs-hook 'remember-theme-save)
  :config
  (remember-theme-load))

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
