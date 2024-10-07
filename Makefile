build:
	@sh -c "jps | grep tube | cut -d' ' -f1 | xargs -r kill"
	rm -f tube
	java -Dbuild.executable=tube -jar target/scala-3.5.1/tube.jar
