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

;;; TODO:

;; * Hook up ri-read-string to ri_repl's disambiguate function.
;; * Flex matching?

;;; Code:

(defvar ri-mode-hook nil
  "Hooks to run when invoking ri-mode.")

(defvar ri-separator "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
  "String used by ri_repl to indicate completion.")

(defun ri (&optional ri-documented)
  "Look up Ruby documentation."
  (interactive)
  (setq ri-documented (or ri-documented (ri-read-string)))
  (let ((ri-buffer (get-buffer-create (format "*ri `%s'*" ri-documented)))
        (ri-content (ri-get ri-documented)))
    (display-buffer ri-buffer)
    (with-current-buffer ri-buffer
      (buffer-disable-undo)
      (erase-buffer)
      (insert ri-content)
      (goto-char (point-min))
      (ri-mode))))

(defun ri-read-string ()
  (read-from-minibuffer "Look up: "))
;; (completing-read "Look up: " 'ri-complete)) ;; nil t))

(defun ri-complete (string predicate true) )

(defun ri-get-process ()
  "Return the subprocess, starting it if necessary."
  (or (get-process "ri-repl")
      (start-process "ri-repl" " *ri-output*" "ri_repl")))

(defun ri-get (ri-documented)
  "Returns the documentation for the class/module/method given."
  (with-current-buffer (process-buffer (ri-get-process))
    (kill-region (point-min) (point-max))

    (process-send-string (ri-get-process) (concat ri-documented "\n"))
    (accept-process-output (ri-get-process) 3)

    ;; Clean up
    (goto-char (point-min))
    (search-forward ri-separator)
    (replace-match "")

    (buffer-string)))

(defun ri-mode ()
  "Mode for viewing Ruby documentation."
  (kill-all-local-variables)
  (local-set-key (kbd "q") 'quit-window)
  (local-set-key (kbd "RET") 'ri-follow)
  (setq mode-name "RI")
  (setq major-mode 'ri-mode)
  (setq buffer-read-only t)
  (run-hooks 'ri-mode-hook))

(defun ri-buffer-contains-p (string)
  (save-excursion
    (goto-char (point-min))
    (search-forward string nil t)))

(provide 'ri)
;;; ri.el ends here

(process-send-string (ri-get-process) "\n")
