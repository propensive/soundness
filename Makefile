SOURCES := $(wildcard src/java/**/*.java)

compile: $(SOURCES)
	javac -d res -Xlint:deprecation src/java/burdock/*.java

.PHONY: compile
