There are two common methods of passing textual key/value data into a JVM when
it starts: environment variables and system properties. Environment variables
are taken from the shell environment within which the JVM is started, and
usually have uppercase names like `PATH` or `XDG_CONFIG_DIR`, while system
properties are specified as parameters to the `java` command, and have
lowercase or camel-case, period-separated names like `default.log.directory`.
The format of the values of each is dependent on the variable or property, but
a program running on the JVM will have expectations about the format of the
variables and properties it accesses, and it is desirable to parse them into
structured types as early as possible. This is the role of Ambience.

