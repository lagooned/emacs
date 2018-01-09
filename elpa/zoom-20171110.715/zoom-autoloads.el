;;; zoom-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "zoom" "zoom.el" (23124 32325 0 0))
;;; Generated autoloads from zoom.el

(defvar zoom-mode nil "\
Non-nil if Zoom mode is enabled.
See the `zoom-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `zoom-mode'.")

(custom-autoload 'zoom-mode "zoom" nil)

(autoload 'zoom-mode "zoom" "\
Perform `zoom' automatically as the selected window changes.

\(fn &optional ARG)" t nil)

(autoload 'zoom "zoom" "\
Zoom the current window and balance the others according to `zoom-size'.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; zoom-autoloads.el ends here
