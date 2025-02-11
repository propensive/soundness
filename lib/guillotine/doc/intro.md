Guillotine makes it easy to work with shell processes in Scala, with simple interpolation-based
definitions of commands, and type-based interpretation of their output. For example, a directory
listing may be obtained with `sh"ls $path".exec[List[String]]()`.

