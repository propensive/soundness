compile:
	mill soundnessweb.web.assembly

build: compile
	rm -rf tmp
	mkdir -p tmp
	unzip -d tmp -o out/soundnessweb/web/assembly.dest/out.jar || echo ignoring error
	cd tmp && zip -r -q -o ../web/WEB-INF/lib/out.jar .

run: build
	java_dev_appserver.sh web

deploy: build
	gcloud app deploy --project=propensive-soundness web/

.PHONY: compile build run deploy
