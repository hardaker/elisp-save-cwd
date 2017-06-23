save-cwd: save the current working directory to a file

# About

How many times have you had to type in a long path in a shell to get
to the directory you were just editing a file while inside emacs?

Sure, you can do `M-x shell` and get a new shell there, but that's
simply inefficient.

# Usage in emacs

	(require 'save-cwd)
	(setq save-cwd-location "~/.emacs_cwd") ;; the default
	(setq save-cwd-timer-period 5)  ;; can be 1 and still be efficient, FYI
	(save-cwd-start)

# Usage in your shell:

    # cd `cat ~/.emacs_cwd`

or put it in an alias:

    # cde() {  cd `cat ~/.emacs_cwd` ; echo "changed to `cat ~/.emacs_cwd`" ; }

