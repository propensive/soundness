_Spectral_ provides the capability to turn an existing Scala command-line application into a daemon for fast startup
(like [Nailgun](https://github.com/facebookarchive/nailgun)). This is particularly useful when combined with
[Exoskeleton](https://github.com/propensive/exoskeleton) for tab completions.

Spectral's launcher script is written in around 100 lines of zero-dependency Bash, and can be used to transform a
Scala application into a self-contained, portable executable.
