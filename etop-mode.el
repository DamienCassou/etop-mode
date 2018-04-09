;;; etop-mode.el --- run "top" -*- lexical-binding: t -*-

;;; Author: Case Mattingly
;;; Version: 0.1
;;; URL: https://github.com/akacase/etop

;; Author: Benjamin Rutt
;; Created: Jul 18, 2004
;; Keywords: extensions, processes

;; etop-mode.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; etop-mode.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This code runs top from within emacs (using top's batch mode), and
;; provides functionality to operate on processes.

;; In order to run it, just execute M-x top.  Unlike real top, the
;; resulting buffer doesn't refresh automatically (yet, it's a feature
;; I'd like to add someday).  You can refresh the buffer manually by
;; pressing 'g'.  If you'd like to mark processes for later actions,
;; use 'm' to mark or 'u' to unmark.  If no processes are marked, the
;; default action will apply to the process at the current line.  At
;; the time of this writing, the valid actions are:
;;
;;     -strace a process
;;     -kill processes
;;     -renice processes

;; You can also toggle showing processes of a specific user by
;; pressing 'U'.
;;
;; NOTE: tested only on GNU/Linux. 

;;; Code:
(defgroup etop nil
  "Emacs frontend to the top command, which monitors system processes."
  :group 'processes)

(defcustom etop-mode-generate-top-command-function
  'etop-mode-generate-top-command-default
  "*Which function to be called to produce the command line for
running top on your machine.

The function will be called with one argument, USER, which will
either be a string specifying that the processes owned by USER
should be shown, or nil, meaning that all processes should be
shown."
  :type 'function
  :group 'etop-mode)

(defcustom etop-mode-column-header-regexp "^\\s-+PID\\s-+USER.*COMMAND\\s-*$"
  "*regexp to match the column header line, which helps this
package to identify where the list of processes begins."
  :type 'regexp
  :group 'etop-mode)

(defcustom etop-mode-mark-face 'highlight
  "*face with which to mark lines."
  :type 'face
  :group 'etop-mode)

(defcustom etop-mode-strace-command "strace"
  "*system call tracer (probably set this to \"truss\" on Solaris, etc)."
  :type 'string
  :group 'etop-mode)

(defcustom etop-refresh-rate 1
  "*set refresh rate that top will be called at."
  :type 'number
  :group 'etop-mode)

;; internals
(defvar etop-mode-hook nil)
(defvar etop-timer nil)
(defvar etop-mode-specific-user nil)
(defvar etop-mode-overlay-list nil)
(defvar etop-mode-generate-top-command-default-user-arg 'unknown)

(defun etop-mode-generate-top-command-default (user)
  (if (not user)
      "top -b -n 1"
    ;; try "-u" argument, and set cache variable based on result
    (if (eq etop-mode-generate-top-command-default-user-arg
	    'unknown)
	(let ((result (shell-command
		       (format "top -b -n 1 -u %s >/dev/null"
			       user-login-name))))
	  (if (= result 0)
	      (setq etop-mode-generate-top-command-default-user-arg 'yes)
	    (setq etop-mode-generate-top-command-default-user-arg 'no))))
    (cond
     ((eq etop-mode-generate-top-command-default-user-arg 'yes)
      (format "top -b -n 1 -u %s" user))
     ;; fall back on "awk"ward manual removal of commands not owned by
     ;; the user
     ((eq etop-mode-generate-top-command-default-user-arg 'no)
      (format "top -b -n 1 | awk 'BEGIN { seenColumnLine=0; } { if (seenColumnLine==0) { print } else if ($2 == \"%s\") { print }; if ($0 ~ /PID.*USER.*COMMAND/) { seenColumnLine=1; } }'" user)))))

(defvar etop-mode-map nil    ; Create a mode-specific keymap.
  "keymap for top mode.")

(if etop-mode-map nil
  (setq etop-mode-map (make-sparse-keymap))
  (define-key etop-mode-map "n" 'etop-mode-next-line)
  (define-key etop-mode-map "p" 'etop-mode-previous-line)
  (define-key etop-mode-map "g" 'beginning-of-buffer)
  (define-key etop-mode-map "q" 'quit-window)
  (define-key etop-mode-map "k" 'etop-mode-kill)
  (define-key etop-mode-map "K" 'etop-mode-kill-noconfirm)
  (define-key etop-mode-map "s" 'etop-mode-strace)
  (define-key etop-mode-map "S" 'etop-mode-strace-noconfirm)
  (define-key etop-mode-map "r" 'etop-mode-renice)
  (define-key etop-mode-map "R" 'etop-mode-renice-noconfirm)
  (define-key etop-mode-map "p" 'etop-pause)
  (define-key etop-mode-map "P" 'etop-resume)
  (define-key etop-mode-map "m" 'etop-mode-mark)
  (define-key etop-mode-map "u" 'etop-mode-unmark)
  (define-key etop-mode-map "U" 'etop-mode-show-specific-user))

(defun etop-mode ()
  "major mode for running top and interacting with processes."
  (interactive)
  (kill-all-local-variables)
  (use-local-map etop-mode-map)
  (setq mode-name "etop")
  (setq major-mode 'etop-mode)
  (run-hooks 'etop-mode-hook)
  (when (not window-system)
    (setq truncate-lines t)))

(defun etop-mode-revert-buffer-function (&optional noconfirm)
  (when (or noconfirm
	    (y-or-n-p "revert *etop* buffer? "))
    (etop)))

(defun etop-mode-next-line ()
  "move to next line in etop-mode"
  (interactive)
  (forward-line 1))

(defun etop-mode-previous-line ()
  "move to previous line in etop-mode"
  (interactive)
  (forward-line -1))

(defun etop-mode-fill-buffer (goto-first-process)
  (get-buffer-create "*etop*")
  (with-current-buffer "*etop*"
    (setq buffer-read-only nil)
    (erase-buffer)
    (let ((old-term-env (getenv "TERM"))
	  (output nil))
      (unwind-protect
	  (progn
	    (setenv "TERM" "dumb")
	    (setq output
		  (shell-command-to-string
		   (funcall etop-mode-generate-top-command-function
			    etop-mode-specific-user))))
	(setenv "TERM" old-term-env))
      (if (not output)
	  (kill-buffer (get-buffer-create "*etop*"))
	(insert output)
	(when goto-first-process
	  (goto-char (point-min))
	  (re-search-forward etop-mode-column-header-regexp nil t)
	  (forward-line 1)
	  (etop-mode-goto-pid))
	(setq buffer-read-only t)
	(etop-mode)))))

;;;###autoload
(defun etop ()
  "runs 'etop' in an emacs buffer."
  (interactive)
  (let* ((already-in-top (or (equal major-mode 'etop-mode) (get-buffer "*etop*")))
	 (preserved-line (if already-in-top (string-to-number (format-mode-line "%l")) nil))
	 (preserved-col (if already-in-top (current-column) nil))
	 (preserved-window-start (window-start)))
    (if already-in-top
	(progn
	  (cancel-timer etop-timer)
	  (if (not (equal (current-buffer) (get-buffer "*etop*")))
	      (switch-to-buffer-other-window "*etop*"))
	  (setq etop-timer (run-at-time (concat (number-to-string etop-refresh-rate) " sec") 1 #'etop-mode-fill-buffer t))
	  (set-window-start (selected-window) preserved-window-start)
	  (forward-line preserved-line)
	  (move-to-column preserved-col))
      (setq etop-timer (run-at-time (concat (number-to-string etop-refresh-rate) " sec") 1 #'etop-mode-fill-buffer t))
      (switch-to-buffer "*etop*"))))

(defsubst etop-mode-string-trim (string)
  "lose leading and trailing whitespace. also remove all
properties from string."
  (if (string-match "\\`[ \t\n]+" string)
      (setq string (substring string (match-end 0))))
  (if (string-match "[ \t\n]+\\'" string)
      (setq string (substring string 0 (match-beginning 0))))
  (set-text-properties 0 (length string) nil string)
  string)

(defun etop-mode-on-pid-line ()
  (when (save-excursion
	  (beginning-of-line)
	  (let ((orig-line (string-to-number (format-mode-line "%l"))))
	    (while (and (equal (string-to-number (format-mode-line "%l")) orig-line)
			(looking-at "\\s-"))
	      (forward-char 1))
	    (and (equal (string-to-number (format-mode-line "%l")) orig-line)
		 (looking-at "[0-9]+\\s-"))))
    (let ((after-pid-line-column-header t)
	  (done nil))
      (save-excursion
	(while (not done)
	  (forward-line 1)
	  (if (= (point) (point-max))
	      (setq done t)
	    (if (looking-at etop-mode-column-header-regexp)
		(progn
		  (setq done t)
		  (setq after-pid-line-column-header nil)))))
	after-pid-line-column-header))))

(defun etop-mode-goto-pid ()
  (interactive)
  (when (etop-mode-on-pid-line)
    (beginning-of-line)
    (while (looking-at "\\s-")
      (forward-char 1))
    (while (looking-at "[0-9]")
      (forward-char 1))
    (forward-char -1)))

(defun etop-mode-get-pid-from-line ()
  (save-excursion
    (beginning-of-line)
    (re-search-forward "\\s-*\\([0-9]+\\)\\s-+" (line-end-position))
    (string-to-number (match-string 1))))

(defun etop-mode-show-specific-user ()
  (interactive)
  (let ((response (read-from-minibuffer "which user (blank for all): ") ))
    (if (string= response "")
	(setq etop-mode-specific-user nil)
      (setq etop-mode-specific-user response))
    (etop)))

(defun etop-mode-get-target-pids ()
  (or
   (sort
    (delq
     nil
     (mapcar
      (lambda (ov)
	(let ((os (overlay-start ov))
	      (oe (overlay-end ov))
	      (str nil))
	  (when (and os oe)
	    (setq str (buffer-substring (overlay-start ov) (overlay-end ov)))
	    (string-match "^\\s-*\\([0-9]+\\)" str)
	    (string-to-number (etop-mode-string-trim
			       (substring str 0 (match-end 0)))))))
      etop-mode-overlay-list))
    '<)
   (list (etop-mode-get-pid-from-line))))

(defun etop-mode-member-at-least-one (ls1 ls2)
  (if (or (null ls1) (null ls2))
      nil
    (or (member (car ls1) ls2)
	(etop-mode-member-at-least-one (cdr ls1) ls2))))

(defun etop-mode-unmark ()
  (interactive)
  (if (not (etop-mode-on-pid-line))
      (message "not on a process line")
    (let (existing-overlay)
      (mapc
       (lambda (ov)
	 (if (member ov etop-mode-overlay-list)
	     (setq existing-overlay ov)))
       (overlays-at (point)))
      (when existing-overlay
	(setq etop-mode-overlay-list
	      (delq existing-overlay etop-mode-overlay-list))
	(delete-overlay existing-overlay))
      (forward-line 1))))

(defun etop-mode-mark ()
  (interactive)
  (if (not (etop-mode-on-pid-line))
      (message "not on a process line")
    (when (not (etop-mode-member-at-least-one
		(overlays-at (point))
		etop-mode-overlay-list))
      (let (o)
	(setq o (make-overlay (line-beginning-position)
			      (line-end-position) nil nil t))
	(overlay-put o (quote face) etop-mode-mark-face)
	(overlay-put o (quote evaporate) t)
	(setq etop-mode-overlay-list (cons o etop-mode-overlay-list))))
    (forward-line 1)))

(defun etop-mode-confirm-action (action-name pids)
  (y-or-n-p
   (format "really %s pids %s? " action-name
	   (mapconcat (lambda (num) (format "%d" num)) pids " "))))

(defun etop-mode-renice (&optional noconfirm)
  (interactive)
  (if (not (etop-mode-on-pid-line))
      (message "not on a process line")
    (let ((pids (etop-mode-get-target-pids)))
      (when (or noconfirm
		(etop-mode-confirm-action "renice" pids))
	(shell-command
	 (format "renice +10 %s"
		 (mapconcat (lambda (num) (format "%d" num)) pids " ")))
	(etop)))))

(defun etop-mode-renice-noconfirm ()
  (interactive)
  (etop-mode-renice t))

(defun etop-mode-strace (&optional noconfirm)
  (interactive)
  (if (not (etop-mode-on-pid-line))
      (message "not on a process line")
    (let ((pids (etop-mode-get-target-pids)))
      (if (> (length pids) 1)
	  (message "cannot strace more than 1 process")
	(when (or noconfirm
		  (etop-mode-confirm-action etop-mode-strace-command pids))
	  (shell-command
	   (format "%s -p %d &" etop-mode-strace-command (car pids))))))))

(defun etop-mode-strace-noconfirm ()
  (interactive)
  (etop-mode-strace t))

(defun etop-mode-kill (&optional noconfirm)
  (interactive)
  (if (not (etop-mode-on-pid-line))
      (message "not on a process line")
    (let ((pids (etop-mode-get-target-pids)))
      (when (or noconfirm
		(etop-mode-confirm-action "kill" pids))
	(shell-command
	 (format "kill -9 %s"
		 (mapconcat (lambda (num) (format "%d" num)) pids " ")))
	(etop)))))

(defun etop-pause ()
  (interactive)
  (cancel-timer etop-timer))

(defun etop-resume ()
  (interactive)
  (etop))

(defun etop-mode-kill-noconfirm ()
  (interactive)
  (etop-mode-kill t))

(provide 'etop-mode)

;;; etop-mode.el ends here
