
TEMPDIR := $(shell mktemp -ud --tmpdir fp-typecheck-XXXX)

COMMAND = runghc $(TYPECHECK)

default: mangle-test

mangle-test:
	make --no-print-directory mangle
	make --no-print-directory test

mangle:
	python3 premark.py

test:
	runhaskell TestBench

.PHONY: default mangle-test mangle test
