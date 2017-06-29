;;; save-cwd.el --- Save the Current Working Directory to a file

;; Copyright (C) 2017 Wes Hardaker
;; Please see the distributed LICENSE file for licencing details

;; Version: 20170628.01
;; Author: Wes Hardaker <opensource@hardakers.net>
;; Maintaner: Wes Hardaker
;; Package-Requires: ()
;; Keywords: convenience
;; URL: https://github.com/hardaker/elisp-save-cwd
;; License: GNU General Public License >= 2
;; Distribution: This file is not part of Emacs

;;; Code:

(defcustom save-cwd-location "~/.emacs_cwd"
  "Location to save the current buffer directory in."
  :type 'file :group 'editing)

(defcustom save-cwd-timer-period 5
  "Wait this many seconds of idle time before saving CWD.

  The saving process is generally very cheap, so short values (even 1 second) sholud be fine."
  :type 'integer :group 'editing)

(defvar save-cwd-timer-object nil)
(defvar save-cwd-current-directory nil)

(defun save-cwd-buffer-directory ()
  "Determine the current directory."
  (when buffer-file-name
    (file-name-directory buffer-file-name)))

(defun save-cwd-save-directory (dirname)
  "Save the current working directory (DIRNAME) to the file from ‘save-cwd-location’."
  (with-temp-file save-cwd-location (insert dirname))
  (setq save-cwd-current-directory save-cwd-cwd))

(defun save-cwd-timer-function ()
  "On idle timer, save the cwd."
  (let ((save-cwd-cwd (save-cwd-buffer-directory)))
    (when (and save-cwd-cwd (not (equal save-cwd-current-directory save-cwd-cwd)))
      (save-cwd-save-directory save-cwd-cwd))))

(defvar save-cwd-mode-p nil)

(define-minor-mode save-cwd-mode
  "A mode to save Emacs' Current Working Directory."
  :lighter " CWD"
  :global t
  :variable save-cwd-mode-p
  (if save-cwd-mode-p
      (progn
        (when (not save-cwd-timer-object)
          (setq save-cwd-timer-object
                (run-with-idle-timer save-cwd-timer-period t #'save-cwd-timer-function))))
    (progn
      (when save-cwd-timer-object
        (cancel-timer save-cwd-timer-object)
        (setq save-cwd-timer-object nil)
        (setq save-cwd-current-directory nil)))))

(provide 'save-cwd)

;;; save-cwd.el ends here
