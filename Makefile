.PHONY : test

test:
	emacs -Q -batch -L . -l test-ltsv.el -f ert-run-tests-batch-and-exit
