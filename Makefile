EMACS ?= emacs

SRCS = sgn-db.el sgn-rpc.el sgn-contacts.el sgn-media.el sgn-format.el \
       sgn-chat.el sgn-actions.el sgn-notify.el sgn-search.el \
       sgn-dashboard.el sgn.el
ELCS = $(SRCS:.el=.elc)

.PHONY: test compile clean

test:
	$(EMACS) -Q --batch \
	  -L . -L test \
	  $(addprefix -l ,$(SRCS)) \
	  -l sgn-test.el \
	  -f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -Q --batch \
	  -L . \
	  --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(SRCS)

clean:
	rm -f *.elc
