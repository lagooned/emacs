;;; variables.el --- jeemacs vars  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Jared M. Engler

;; Author: Jared M. Engler <jared.lite@gmail.com>
;; Keywords: jeemacs, config, var

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

;; Je/Emacs custom variable definitions.

;;; Code:

(defvar je/large-file-size 2
  "Size (in MB) above which the user will be prompted to open the file literally \
to avoid performance issues. Opening literally means that no major or minor \
modes are active and the buffer is read-only.")

(defvar je/eshell-message "Je/eshell \\(^.^\\)\n"
  "Message shown when starting Eshell.")

(defvar je/eshell-top-prompt-regexp "^\\[.*\\].*"
  "A regexp which matches the top line in the eshell prompt.")

(defvar je/eshell-prompt-regexp "^[#$] "
  "A regexp which fully matches your eshell prompt.

This setting is important, since it affects how eshell will interpret \
the lines that are passed to it. \
If this variable is changed, all Eshell buffers must be exited and \
re-entered for it to take effect.")

(defvar je/mc-evil-prev-state nil
  "Saves the previous evil state as a string.")

(defvar je/mc-evil-mark-was-active nil
  "Saves the previous state of the mark.")

(defvar je/js2-xref-accept-ag nil
  "Determines whether or not to show the xref-ag confirmation.")

(defvar je/force-basic-grep nil
  "Determines whether or not to use basic grep.")

(defvar je/git-ls-tree-head-cmd "git ls-tree -rt HEAD"
  "Git command for showing ls-tree")

(defvar je/current-user
  (getenv (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(defvar je/modeline-blacklist
  '((vc-mode vc-mode)
    mode-line-mule-info
    mode-line-client
    mode-line-remote))

(defvar je/custom-file-location "~/.emacs.d/.custom.el"
  "Location for Emacs custom config file.")

(defvar je/scratch-message
  "
;;
;;                                              88888
;;           :::::::::::               ::::::: 88888 ::::::::
;;           :888888888:               :88888 88888 88888888:
;;           :888888888:               :8888 88888 888888888:
;;           ::8888888::                ::: 88888 ::::::8888:
;;             :88888:   :::::::::::       88888 :     ::::::  :::::::    :::::::    :::::::::::::     :::::::::::::    ::::::::::
;;             :88888: ::88888888888::    88888 8:           ::8888888:  :8888888::  :888888888888:  ::8888888888888: ::8888888888:
;;             :88888::88888:::::88888:: 88888 888::::::::  :8888888888::8888888888: :::::::::88888::888888888888888::8888888888888:
;;             :88888:88888:     :88888 88888 88888888888:  :8888888888888888888888:          :8888:888888::::::8888:888888::::88888:
;;             :88888:888888:::::88888 88888 888888888888:  :88888:::888888:::88888:   :::::::88888:88888:     :::::: :88888:  ::::::
;; :::::::     :88888:888888888888888 88888 88888:::::::::  :8888:   :8888:   :8888: ::888888888888:8888:               :888888:
;; :88888:     :88888:888::::::::::: 88888 88888:           :8888:   :8888:   :8888::8888::::888888:8888:                  :888888:
;; :888888:   :888888:888:          88888 :88888:      ::::::8888:   :8888:   :8888:8888:    :88888:88888:     :::::::::::   :88888:
;; ::888888:::888888::888:         88888 :888888:::::::88888:8888:   :8888:   :8888:8888:    :88888:888888::::::8888:88888::::888888:
;;  ::8888888888888:: :888::::::: 88888 88888888888888888888:8888:   :8888:   :8888:88888::::888888::888888888888888:88888888888888:
;;    ::888888888::    ::8888888 88888 :88888888888888888888:8888:   :8888:   :8888::888888888::888: ::8888888888888::88888888888::
;;      :::::::::        :::::: 88888  :::::::::::::::::::::::::::   ::::::   :::::: :::::::::::::::  :::::::::::::  :::::::::::
;;                             88888
;;
;;  %s, %s.
;;

;; To begin exploring, press %s.

"
  "Initial scratch buffer message format.")

(defvar je/leader-key "SPC"
  "Key used by evil-leader to define the primary leader key.")

(provide 'variables)
;;; variables.el ends here
