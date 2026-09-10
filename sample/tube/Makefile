build:
	@sh -c "jps | grep tube | cut -d' ' -f1 | xargs -r kill"
	rm -f tube
	xeq build --jar target/scala-3.5.1/tube.jar --out tube
	cp tube ~/.local/bin/tube
