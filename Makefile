
SCALAC := scala/bin/scalac -language:experimental.saferExceptions -new-syntax -Ysafe-init -feature -Xcheck-macros -Ycheck-all-patmat -Yexplicit-nulls -d out

pub: out $(wildcard mod/*/src)
	mkdir -p pub
	etc/build.sh

bin:
	mkdir -p bin

scala/src:
	git submodule update --init scala

mod/%/src:
	git submodule update --init mod/$@

