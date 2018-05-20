clean:
	@rm -f init.elc nemacs.el nemacs.elc

compile: init.el bmacs.org clean
	@emacs -Q --batch -l 'lisp/compile.el'
