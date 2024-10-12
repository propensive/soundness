SOURCES := $(wildcard src/java/**/*.java)

compile: .wrath/bin $(SOURCES)
	javac -d .wrath/bin -Xlint:deprecation src/java/burdock/*.java

.wrath/bin:
	mkdir -p .wrath/bin

.PHONY: compile
