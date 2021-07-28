
pub: scala/bin/scalac $(wildcard mod/*/)
	echo $<
	mkdir -p pub
	etc/build.sh

bin:
	mkdir -p bin

scala/bin/scalac:
	git submodule update --init scala

mod/%/:
	git submodule update --init mod/$*
