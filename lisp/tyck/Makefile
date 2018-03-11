EMACS := emacs --quick --batch

.PHONY: compile
compile: tyck.elc

.PHONY: test
test: tyck.el tyck-test.el
	$(EMACS) --directory . --load tyck-test.el --funcall ert-run-tests-batch-and-exit

%.elc: %.el
	$(EMACS) --funcall batch-byte-compile $<
