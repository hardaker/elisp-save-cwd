# save-cwd: save your current working directory to a file

# About

How many times have you had to type in a long path in a shell to get
to the directory you were just editing a file while inside emacs?

## Usage in emacs

	(require 'save-cwd)
	(setq save-cwd-location "~/.emacs_cwd") ;; the default
	(setq save-cwd-timer-period 5)  ;; can be 1 and still be efficient, FYI
	(save-cwd) ; turns on the save-cwd minor mode

## Usage in your shell:

    # cd `cat ~/.emacs_cwd`

or put it in an alias:

    # cde() {  cd `cat ~/.emacs_cwd` ; echo "changed to `cat ~/.emacs_cwd`" ; }

# Caveats

Will do funky things if invoked in two different emacs processes.  *(But
why would you use two in the first place???)*

# Other options

**M-x shell:** Sure, you can do `M-x shell` and get a new shell there, but that's
simply inefficient when you have a bazillion shells, one per
directory.

**emacsclient:** Three is probably a way to pass a command to
`emacsclient -e`, but I didn't go that route.  Would probably work
fine too though.

**Bind to key:** I'll eventually add *(interactive)* to the function
to optionally "save now" rather than via a timer.  But that would
require moving the mouse *back* to emacs when you forget to run it
before switching to a terminal window.

# What I'll take flack for

Leaving emacs in the first place.

# With Thanks To ...

Thanks a bunch to the following people that help contribute code
and/or ideas to this project:

  - Samuel W. Flint <swflint@flintfam.org>


