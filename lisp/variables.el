;;; variables.el --- jeemacs vars  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Jared M. Engler

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

(defvar je/git-ls-tree-head-cmd "git ls-tree -rt HEAD"
  "Git command for showing ls-tree")

(defvar je/current-user
  (getenv (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(defvar je/modeline-blacklist
  '((vc-mode vc-mode)
    mode-line-mule-info
    mode-line-client
    mode-line-remote
    "  "))

(defvar je/custom-file-location "~/.emacs.d/.custom.el"
  "Location for Emacs custom config file.")

(defvar je/scratch-message
  "
;;
;;  hey %s, this buffer can be used to evaluate emacs lisp
;;

"
  "Initial scratch buffer message format.")

(defvar je/leader-key "SPC"
  "Key used by evil-leader to define the primary leader key.")

(defvar je/logo
  "
                                             88888
          :::::::::::               ::::::: 88888 ::::::::
          :888888888:               :88888 88888 88888888:
          :888888888:               :8888 88888 888888888:
          ::8888888::                ::: 88888 ::::::8888:
            :88888:   :::::::::::       88888 :     ::::::  :::::::    :::::::    :::::::::::::     :::::::::::::    ::::::::::
            :88888: ::88888888888::    88888 8:           ::8888888:  :8888888::  :888888888888:  ::8888888888888: ::8888888888:
            :88888::88888:::::88888:: 88888 888::::::::  :8888888888::8888888888: :::::::::88888::888888888888888::8888888888888:
            :88888:88888:     :88888 88888 88888888888:  :8888888888888888888888:          :8888:888888::::::8888:888888::::88888:
            :88888:888888:::::88888 88888 888888888888:  :88888:::888888:::88888:   :::::::88888:88888:     :::::: :88888:  ::::::
:::::::     :88888:888888888888888 88888 88888:::::::::  :8888:   :8888:   :8888: ::888888888888:8888:               :888888:
:88888:     :88888:888::::::::::: 88888 88888:           :8888:   :8888:   :8888::8888::::888888:8888:                  :888888:
:888888:   :888888:888:          88888 :88888:      ::::::8888:   :8888:   :8888:8888:    :88888:88888:     :::::::::::   :88888:
::888888:::888888::888:         88888 :888888:::::::88888:8888:   :8888:   :8888:8888:    :88888:888888::::::8888:88888::::888888:
 ::8888888888888:: :888::::::: 88888 88888888888888888888:8888:   :8888:   :8888:88888::::888888::888888888888888:88888888888888:
   ::888888888::    ::8888888 88888 :88888888888888888888:8888:   :8888:   :8888::888888888::888: ::8888888888888::88888888888::
     :::::::::        :::::: 88888  :::::::::::::::::::::::::::   ::::::   :::::: :::::::::::::::  :::::::::::::  :::::::::::
                            88888
"
  "Je/Emacs logo.")

(provide 'variables)
;;; variables.el ends here
