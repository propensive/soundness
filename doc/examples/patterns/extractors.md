# Groups which bind variables

A name written before a group in a pattern binds what the group matched. The pattern and the
variables it extracts are one expression, so they cannot drift apart.

```scala fuel
compile
frame
  label A pattern which matches
  code
      def surname(name: Text): Text = name match
        case r"[A-Z][a-z]+ [A-Z][a-z]+" => name
        case _                          => t"unknown"
frame
  label A group
  focus ([A-Z][a-z]+)"
  code
      def surname(name: Text): Text = name match
        case r"[A-Z][a-z]+ ([A-Z][a-z]+)" => name
        case _                            => t"unknown"
frame
  label A name for the group
  highlight $last  binds the group which follows it
  code
      def surname(name: Text): Text = name match
        case r"[A-Z][a-z]+ $last([A-Z][a-z]+)" => last
        case _                                 => t"unknown"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(surname(t"Nick Bottom"))
```

[Matching](../../soundness/modules/patterns.md#matching) has more on captures.
