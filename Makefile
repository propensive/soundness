
bin:
	mkdir -p bin

bin/cs: bin
	curl -fLo bin/cs https://git.io/coursier-cli-linux
	chmod +x bin/cs

bin/sbt: bin/cs
	bin/cs install sbt
	ln /home/runner/.local/share/coursier/bin/sbt bin/sbt

bin/scalac: bin/sbt scala
	cd scala && bin/sbt dist/packArchive
	cp scala/bin/scalac bin/scalac

out:
	mkdir -p out

scala:
	git submodule update --init scala

mod/rudiments:
	git submodule update --init mod/rudiments

out/rudiments: out mod/rudiments bin/scalac
	bin/scalac -d out rudiments/src/core/*.scala
