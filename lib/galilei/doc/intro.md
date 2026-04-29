__Galilei__ is a library for performing safe disk I/O with Scala. It uses dependent types to provide
precise static representations of paths, files, directories and other filesystem objects, enforcing
filesystem-aware constraints on filenames and metadata. Galilei has a particular focus on precise error
handling and, when enabled, each filesystem operation which might fail must be handled for each possible
failure type; but its innovation is in minimizing that to a set of failure types which depends not
just on the operation, but also the filesystem and in-scope options.

