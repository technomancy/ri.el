;;; ri.el --- Ruby Documentation Lookup

;; Copyright (C) 2008 Phil Hagelberg

;; Author: Phil Hagelberg
;; Version: 0.2
;; Keywords: tools, documentation
;; Created: 2008-09-19
;; URL: http://www.emacswiki.org/cgi-bin/wiki/RiEl
;; EmacsWiki: RiEl

;; This file is NOT part of GNU Emacs.

;;; License:

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

;; ri.el provides an Emacs frontend to Ruby's `ri' documentation
;; tool. It offers lookup and completion.

;; It relies on the ruby script `ri_repl' being on the path, which
;; should be found at http://p.hagelb.org/ri_repl

;;; TODO:

;; * Class methods only work with Class::method syntax, not Class.method
;; * Keep input history
;; * Documentation for completing-read is out of date, ask for
;;   clarification on emacs-devel re: (boundaries . "")
;; * Can we bundle the Ruby script *inside* the elisp and run it with
;;   "ruby -e"? Is that even *sane*?
;; * Flex matching?

;;; Code:

(defvar ri-mode-hook nil
  "Hooks to run when invoking ri-mode.")

(defun ri (&optional ri-documented)
  "Look up Ruby documentation."
  (interactive)
  (setq ri-documented (or ri-documented (ri-read-string)))
  (let ((ri-buffer (get-buffer-create (format "*ri %s*" ri-documented)))
        (ri-content (ri-get ri-documented)))
    (display-buffer ri-buffer)
    (with-current-buffer ri-buffer
      (erase-buffer)
      (insert ri-content)
      (goto-char (point-min))
      (ri-mode))))

(defun ri-read-string ()
  (setq ri-catcher nil) ;; for debugging completion
  (completing-read "Look up: " 'ri-complete nil t))

(defun ri-complete (string predicate bounds)
  (add-to-list 'ri-catcher (list string predicate bounds))
  (if (eq t bounds)
      (read (ri-get (concat "Complete: " string)))))

(defun ri-get-process ()
  "Return the subprocess, starting it if necessary."
  (or (get-process "ri-repl")
      (start-process "ri-repl" " *ri-output*" "ri_repl")))

(defun ri-get (ri-documented)
  "Returns the documentation for the class/module/method given."
  (with-current-buffer (process-buffer (ri-get-process))
    (erase-buffer)

    (process-send-string (ri-get-process) (concat ri-documented "\n"))
    (accept-process-output (ri-get-process) 3 0 t)
    (buffer-string)))

(defun ri-mode ()
  "Mode for viewing Ruby documentation."
  (buffer-disable-undo)
  (kill-all-local-variables)
  (local-set-key (kbd "q") 'quit-window)
  (local-set-key (kbd "RET") 'ri-follow)
  (setq mode-name "RI")
  (setq major-mode 'ri-mode)
  (setq buffer-read-only t)
  (run-hooks 'ri-mode-hook))

(provide 'ri)
;;; ri.el ends here