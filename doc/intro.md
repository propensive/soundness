Exoskeleton is a Scala library to make it easy to write command-line applications, in particular those which
run as a daemon for fast startup (like [Nailgun](https://github.com/facebookarchive/nailgun)) or which require
argument parsing and programmatic tab-completions.

Exoskeleton's launcher script is written in under 100 lines of Bash, and can be used to transform any executable
JAR file whose main entry point is provided by Exoskeleton into a self-contained, portable executable.

A novel API for tab-completions is provided, making it trivial to write interactive command-line applications in
Scala. Tab completions are automatically provided for [`bash`](https://www.gnu.org/software/bash/),
[`zsh`](http://zsh.sourceforge.net/) and [`fish`](https://fishshell.com/).