clean:
	@rm -f *.elc readme.el
compile: init.el readme.org clean
	@emacs -Q --batch -l 'lisp/compile.el'
