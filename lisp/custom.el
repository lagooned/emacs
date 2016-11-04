;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM FUNCTIONS ;;
;;;;;;;;;;;;;;;;;;;;;;

;; deunicode file
(defun jme/de-unicode ()
  "Tidy up a buffer by replacing all special Unicode characters
     (smart quotes, etc.) with their more sane cousins"
  (interactive)
  (let ((unicode-map '(("[\u2018\'\u2019\'\u201A\'\uFFFD]" . "'")
                       ("[\u201c\'\u201d\'\u201e]" . "\"")
                       ("\u2013" . "--")
                       ("\u2014" . "---")
                       ("\u2026" . "...")
                       ("\u00A9" . "(c)")
                       ("\u00AE" . "(r)")
                       ("\u2122" . "TM")
                       ("[\u02DC\'\u00A0]" . " "))))
    (save-excursion
      (loop for (key . value) in unicode-map
            do
            (goto-char (point-min))
            (replace-regexp key value)))))

;; sudo find-file
(defun jme/find-file-as-sudo ()
  (interactive)
  (let ((file-name (buffer-file-name)))
    (when file-name
      (find-alternate-file (concat "/sudo::" file-name)))))

;; indent on paste
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
		   (and (not current-prefix-arg)
				(member major-mode '(emacs-lisp-mode lisp-mode
													 clojure-mode    scheme-mode
													 haskell-mode    ruby-mode
													 rspec-mode      python-mode
													 c-mode          c++-mode
													 objc-mode       latex-mode
													 plain-tex-mode))
				(let ((mark-even-if-inactive transient-mark-mode))
				  (indent-region (region-beginning) (region-end) nil))))))
