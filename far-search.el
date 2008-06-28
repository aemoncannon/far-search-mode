;;; far-search.el --- search incrementally in many buffers at once

;; Copyright (c) 2008 Aemon Cannon, aemoncannon -at- gmail -dot- com

;; Author: Aemon Cannon
;; Keywords: matching, lisp, tools

;; This file is part of far-search.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Basic usage:

;;  (add-to-list 'load-path "~/path/to/far-search/")
;;  (require 'far-search)
;;  M-x far-search

;;; Code:

(eval-when-compile (require 'cl))

(defcustom far-search-mode-hook nil
  "*Hooks to run on entering far-search-mode."
  :group 'far-search
  :type 'hook)

;; Internal variables below
(defvar far-search-mode nil
  "Enables the far-search minor mode.")

(defvar far-search-buffer-name "*far-search*"
  "Buffer to use for far-search.")

(defvar far-search-target-buffer-name "*far-search-results*"
  "Buffer name for target-buffer.")

(defvar far-search-target-buffer nil
  "Buffer to which the far-search is applied to.")

(defvar far-search-target-window nil
  "Window to which the far-search is applied to.")

(defvar far-search-window-config nil
  "Old window configuration.")

(defvar far-search-mode-string ""
  "String in mode line for additional info.")

(defvar far-search-current-results '()
  "The most recent far-search result list.")

(defvar far-search-current-selected-result '()
  "The currently selected far-search result.")

;; Define the local "\C-c" keymap
(defvar far-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-q" 'far-search-quit)
    (define-key map "\C-n" 'far-search-next-match)
    (define-key map "\C-p" 'far-search-prev-match)
    (define-key map [(return)] 'far-search-i-feel-lucky)
    map)
  "Keymap used by far-search.")

(defstruct far-search-result 
  "A far-search search result." 
  (link-text nil)
  (match-file-name nil) 
  (match-start nil)
  (match-end nil)
  (text-link-offset 0)
  (text-link-length 0)
  (link-offset 0)
  )

(defun far-search-mode ()
  "Major mode for interactively building Regular Expressions.
\\{reb-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'far-search-mode
        mode-name "far-search-mode")
  (use-local-map far-search-mode-map)
  (far-search-mode-common)
  (run-mode-hooks 'far-search-mode-hook))

;;;###autoload
(defun far-search ()
  "Initiate an incremental search of live buffers."
  (interactive)
  (if (and (string= (buffer-name) far-search-buffer-name)
	   (memq major-mode '(far-search-mode)))
      (message "Already in far-search buffer")

    (setq far-search-target-buffer (switch-to-buffer (get-buffer-create far-search-target-buffer-name)))
    (setq far-search-target-window (selected-window))
    (setq far-search-window-config (current-window-configuration))

    (select-window (split-window (selected-window) (- (window-height) 4)))
    (switch-to-buffer (get-buffer-create far-search-buffer-name))
    (far-search-initialize-buffer)))

(defun far-search-quit ()
  "Quit the far-search mode."
  (interactive)
  (kill-buffer far-search-buffer-name)
  (set-window-configuration far-search-window-config))

(defun far-search-i-feel-lucky ()
  "Jump to the target of the currently selected far-search-result."
  (interactive)
  (if (and (far-search-result-p far-search-current-selected-result)
	   (get-buffer far-search-buffer-name))
      (progn
	(switch-to-buffer far-search-buffer-name)
	(kill-buffer-and-window)
	(let* ((r far-search-current-selected-result)
	       (file-name (far-search-result-match-file-name r))
	       (offset (far-search-result-match-start r)))
	  (find-file file-name)
	  (goto-char offset)))))


(defun far-search-next-match ()
  "Go to next match in the far-search target window."
  (interactive)
  (if (and far-search-current-results
	   far-search-current-selected-result)
      (let* ((i (position far-search-current-selected-result far-search-current-results))
	     (len (length far-search-current-results))
	     (next (if (< (+ i 1) len) 
		       (nth (+ i 1) far-search-current-results)
		     (nth 0 far-search-current-results))))
	(setq far-search-current-selected-result next)
	(far-search-update-result-selection)
	)))


(defun far-search-prev-match ()
  "Go to previous match in the far-search target window."
  (interactive)
  (if (and far-search-current-results
	   far-search-current-selected-result)
      (let* ((i (position far-search-current-selected-result far-search-current-results))
	     (len (length far-search-current-results))
	     (next (if (> i 0)
		       (nth (- i 1) far-search-current-results)
		     (nth (- len 1) far-search-current-results))))
	(setq far-search-current-selected-result next)
	(far-search-update-result-selection)
	)))


;;
;; Non-interactive functions below
;;

(defun far-search-mode-common ()
  "Setup functions common to function `far-search-mode'."
  (setq	far-search-mode-string  ""
	far-search-mode-valid-string ""
	mode-line-buffer-identification
	'(25 . ("%b" far-search-mode-string far-search-valid-string)))
  (far-search-update-modestring)
  (make-local-variable 'after-change-functions)
  (add-hook 'after-change-functions
	    'far-search-auto-update)
  (make-local-variable 'far-search-kill-buffer)
  (add-hook 'kill-buffer-hook 'far-search-kill-buffer)
  )


(defun far-search-initialize-buffer ()
  "Initialize the current buffer as a far-search buffer."
  (erase-buffer)
  (far-search-mode)
  )


(defun far-search-update-result-selection ()
  "Move cursor to current result selection in target buffer."
  (if (far-search-result-p far-search-current-selected-result)
      (with-current-buffer far-search-target-buffer
	(let ((target-point (far-search-result-link-offset 
			     far-search-current-selected-result)))
	  (set-window-point far-search-target-window target-point)
	  ))))


(defun far-search-update-regexp ()
  "Update the regexp for the target buffer."
  (let* ((re-src (far-search-read-regexp)))
    (with-current-buffer far-search-target-buffer
      (if re-src
	  (setq far-search-regexp re-src))
      far-search-regexp
      )))

(defun far-search-do-update (&optional subexp)
  "Update matches in the far-search target window."
  (far-search-assert-buffer-in-window)
  (far-search-update-regexp))

(defun far-search-auto-update (beg end lenold &optional force)
  "Called from `after-update-functions' to update the display.
BEG, END and LENOLD are passed in from the hook.
An actual update is only done if the regexp has changed or if the
optional fourth argument FORCE is non-nil."
  (progn
    (if (or (far-search-update-regexp) force)
	(progn
	  (far-search-assert-buffer-in-window)
	  (far-search-do-update)
	  (far-search-update-target-buffer)
	  ))
    (force-mode-line-update)))

(defun far-search-assert-buffer-in-window ()
  "Assert that `far-search-target-buffer' is displayed in `far-search-target-window'."
  (if (not (eq far-search-target-buffer (window-buffer far-search-target-window)))
      (set-window-buffer far-search-target-window far-search-target-buffer)))

(defun far-search-update-modestring ()
  "Update the variable `far-search-mode-string' displayed in the mode line."
  (force-mode-line-update))

(defun far-search-kill-buffer ()
  "When the far-search buffer is killed, kill the target buffer."
  (remove-hook 'kill-buffer-hook 'far-search-kill-buffer)
  (if (buffer-live-p far-search-target-buffer)
      (kill-buffer far-search-target-buffer)))

;; The next functions are the interface between the regexp and
;; its textual representation in the far-search buffer.
;; They are the only functions concerned with the actual syntax
;; being used.
(defun far-search-read-regexp ()
  "Read current RE."
  (buffer-string))

(defun far-search-buffers-to-search ()
  "Return the list of buffers that are suitable for searching."
  (let ((all-buffers (buffer-list)))
    (remove-if
     (lambda (b)
       (let ((b-name (buffer-name b)))
	 (or (null (buffer-file-name b))
	     (equal b-name far-search-target-buffer-name)
	     (equal b-name far-search-buffer-name)
	     (equal b-name "*Messages*"))))
     all-buffers)))

(defun far-search-make-text-link (start end file-path offset)
  "Make an emacs button, from start to end in current buffer, linking to file-path and offset."
  (make-button start end
	       'face font-lock-constant-face
	       'action `(lambda (x)
			  (far-search-quit)
			  (find-file ,file-path)
			  (goto-char ,offset)
			  )))

(defun far-search-update-target-buffer ()
  "This is where the magic happens. Update the result list."
  (save-excursion
    (set-buffer far-search-target-buffer)
    (setq buffer-read-only nil)
    (goto-char (point-min))
    (erase-buffer)
    (setq far-search-current-results '())

    (let ((buffers (far-search-buffers-to-search)))

      (mapc 
       (lambda (b)
	 (with-current-buffer b
	   (goto-char (point-min))
	   (let ((search-result (re-search-forward far-search-regexp nil t)))
	     (if search-result
		 (let ((text (buffer-substring-no-properties 
			      (point-at-bol)
			      (min (point-max)
				   (+ (match-end 0) 20))
			      )))
		   (push (make-far-search-result 
			  :link-text text
			  :match-file-name (buffer-file-name)
			  :match-start (match-beginning 0)
			  :match-end (match-end 0)
			  :text-link-offset (- (match-beginning 0) (point-at-bol))
			  :text-link-length (length (match-string 0))) far-search-current-results)
		   )))
	   )) buffers)

      (if far-search-current-results 
	  (setq far-search-current-selected-result (first far-search-current-results)))

      (mapc
       (lambda (r)
	 (let ((start-point (point)))
	   (setf (far-search-result-link-offset r) start-point)
	   (insert (format "%s....  \n[%s]" 
			   (far-search-result-link-text r) 
			   (far-search-result-match-file-name r)))
	   (far-search-make-text-link (+ start-point (far-search-result-text-link-offset r))
				      (+ start-point (far-search-result-text-link-offset r) (far-search-result-text-link-length r))
				      (far-search-result-match-file-name r)
				      (far-search-result-match-start r))
	   (insert "\n\n--------------------------\n\n")
	   ))
       far-search-current-results)

      (setq buffer-read-only t)
      )))



(provide 'far-search)



