(defvar save-cwd-location "~/.emacs_cwd"
  "Location to save the current buffer directory in ")

(defvar save-cwd-timer-period 5
  "Number of seconds to wait after being idle to save the current working directory.

  The saving process is generally very cheap, so short values (even 1 second) sholud be fine.
" 
)

(defvar *save-cwd-timer-object* nil)
(defvar *save-cwd-current-directory* nil)

(defun save-cwd-buffer-directory ()
  (when buffer-file-name
    (file-name-directory buffer-file-name)))

(defun save-cwd-save-directory (dirname)
  "Save the current working directory to the file from save-cwd-location"
  (with-temp-file save-cwd-location (insert dirname))
  (setq *save-cwd-current-directory* save-cwd-cwd))

(defun save-cwd-timer ()
  "The meat of the save-cwd effort"

  (let ((save-cwd-cwd (save-cwd-buffer-directory)))
    (when (and save-cwd-cwd (not (equal *save-cwd-current-directory* save-cwd-cwd)))
      (save-cwd-save-directory save-cwd-cwd))))

(defvar save-cwd-mode-p nil)

(define-minor-mode save-cwd-mode
  "A mode to save Emacs' Current Working Directory."
  :lighter " CWD"
  :global t
  :variable save-cwd-mode-p
  (if save-cwd-mode-p
      (progn
        (when (not *save-cwd-timer-object*)
          (setq *save-cwd-timer-object*
                (run-with-idle-timer save-cwd-timer-period t 'save-cwd-timer))))
    (progn
      (when *save-cwd-timer-object*
        (cancel-timer *save-cwd-timer-object*)
        (setq *save-cwd-timer-object* nil)
        (setq *save-cwd-current-directory* nil)))))

(provide 'save-cwd)
