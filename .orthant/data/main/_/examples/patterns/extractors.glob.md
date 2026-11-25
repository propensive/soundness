# A path taken apart by a glob

A glob is the wildcard syntax of the command line, and `g"…"` makes one a pattern. A `$name`
in it binds a segment of the path, `*` stands for a segment which is not wanted, and `**` for
any number of them.

```scala fuel
compile
frame
  label One segment, bound
  highlight $play  binds one segment
  code
      def play(path: Text): Text = path match
        case g"/plays/$play/act1/scene2" => play
        case _                           => t"unknown"
frame
  label A segment which may be anything
  highlight *  one segment, not bound
  code
      def play(path: Text): Text = path match
        case g"/plays/$play/*/scene2" => play
        case _                        => t"unknown"
frame
  label Any number of segments
  highlight **  the rest of the path
  code
      def play(path: Text): Text = path match
        case g"/plays/$play/**" => play
        case _                  => t"unknown"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(play(t"/plays/hamlet/act3/scene1"))
```

[Globs](../../soundness/modules/patterns.md#globs) has the full syntax, and its translation to
a regular expression.
