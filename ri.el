;;; ri.el --- Ruby Documentation Lookup

;; Copyright (C) 2008 Phil Hagelberg

;; Author: Phil Hagelberg
;; Version: 0.1
;; Keywords: tools, documentation

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;; Commentary:

;; ri.el provides an Emacs frontend to Ruby's `ri' documentation tool.

;;; Code:

(defvar ri-mode-hook nil
  "Hooks to run when invoking ri-mode.")

(defvar ri-history nil
  "History of previously invoked lookups")

(defun ri (ri-documented)
  "Look up Ruby documentation."
  (interactive (ri-read-string))
  (let ((ri-buffer (get-buffer-create (format "*ri `%s'*" ri-documented)))
        (ri-content (ri-lookup ri-documented)))
    (display-buffer ri-buffer)
    (with-current-buffer ri-buffer
      (buffer-disable-undo)
      (erase-buffer)
      (insert ri-content)
      (goto-char (point-min))
      (ri-mode))))

(defun ri-read-string ()
  (completing-read "Documentation for: " 'ri-complete nil t 'ri-history))

(defun ri-complete (string predicate true) )

(defun ri-start-process ()
  (start-process "ri" " *ri-output*" "ri_repl"))

(defun ri-mode ()
  "Mode for viewing Ruby documentation."
  (kill-all-local-variables)
  (local-set-key (kbd "q") 'quit-window)
  (local-set-key (kbd "RET") 'ri-follow)
  (setq mode-name "RI")
  (setq major-mode 'ri-mode)
  (setq buffer-read-only t)
  (run-hooks 'ri-mode-hook))

(provide 'ri)
;;; ri.el ends here