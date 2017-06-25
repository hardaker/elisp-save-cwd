(defvar save-cwd-location "~/.emacs_cwd"
  "Location to save the current buffer directory in ")

(defvar save-cwd-timer-period 5
  "Number of seconds to wait after being idle to save the current working directory.

  The saving process is generally very cheap, so short values (even 1 second) sholud be fine.
" 
)

(defvar save-cwd-timer-object nil)
(defvar save-cwd-current-directory nil)

(defun save-cwd-buffer-directory ()
  (when buffer-file-name
    (file-name-directory buffer-file-name)))

(defun save-cwd-save-directory (dirname)
  "Save the current working directory to the file from save-cwd-location"
  (with-temp-file save-cwd-location (insert dirname))
  (setq save-cwd-current-directory save-cwd-cwd)
)

(defun save-cwd-timer ()
  "The meat of the save-cwd effort"

  (let ((save-cwd-cwd (save-cwd-buffer-directory)))
    (when (and save-cwd-cwd (not (equal save-cwd-current-directory save-cwd-cwd)))
	(save-cwd-save-directory save-cwd-cwd)
	)))

(defun save-cwd-start ()
  "Enables saving the current working directory to a file"
  (interactive)
  (when (not save-cwd-timer-object)
    (setq save-cwd-timer-object
	  (run-with-idle-timer save-cwd-timer-period t 'save-cwd-timer))))

(defun save-cwd-stop ()
  "Stops the process of saving the current working directory to a file"
  (when save-cwd-timer-object
    (cancel-timer save-cwd-timer-object)
    (setq save-cwd-timer-object nil)
    (setq save-cwd-save-directory nil)
    ))

(save-cwd-start)
(save-cwd-stop)
