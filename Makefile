publishLocal:
	mill -k __.publishLocal
	mill soundness.__.publishLocal

build:
	mill soundness.all

dev:
	mill -w soundness.all

.PHONY: publishLocal build dev
