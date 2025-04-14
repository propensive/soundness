publishLocal:
	mill -k __.publishLocal
	mill soundness.__.publishLocal

test:
	mill test.assembly
	java -cp out/test/assembly.dest/out.jar soundness.Tests

build:
	mill soundness.all

dev:
	mill -w soundness.all

ci:
	./mill test.assembly
	java -cp out/test/assembly.dest/out.jar soundness.Tests

.PHONY: publishLocal build dev
