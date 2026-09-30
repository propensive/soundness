build:
	@sh -c "jps | grep tube | cut -d' ' -f1 | xargs -r kill"
	rm -f tube
	xek target/scala-3.5.1/tube.jar tube
	cp tube ~/.local/bin/tube
