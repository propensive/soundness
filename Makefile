build: ire tmp/.publish
	./ire

ire:
	curl -Lo ire https://github.com/propensive/ire/releases/download/v0.1.0/ire
	chmod +x ire

tmp:
	mkdir -p tmp

tmp/sbt.tgz: tmp
	wget -nc -q -O tmp/sbt.tgz https://github.com/sbt/sbt/releases/download/v1.5.5/sbt-1.5.5.tgz || true

tmp/scala-cli.gz: tmp
	wget -nc -q -O tmp/scala-cli.gz https://github.com/VirtusLab/scala-cli/releases/download/v0.0.9/scala-cli-x86_64-pc-linux-static.gz || true

scala-cli: tmp/scala-cli.gz
	gunzip -k tmp/scala-cli.gz
	mv tmp/scala-cli scala-cli
	chmod +x scala-cli

sbt/bin/sbt: tmp/sbt.tgz
	tar xvf tmp/sbt.tgz

scala:
	git clone https://github.com/dotty-staging/dotty --branch=fix-13691 scala

tmp/.publish: sbt/bin/sbt scala
	cd scala && ../sbt/bin/sbt publishLocal
	touch tmp/.publish
