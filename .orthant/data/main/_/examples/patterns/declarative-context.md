# An engine which cannot backtrack

The engine which runs a pattern is chosen by an import. `re2` is an automaton, which takes
time linear in its input and cannot express a backreference, and a literal pattern which needs
one is rejected as the code compiles.

```scala fuel
compile
frame
  label The default engine, and a backreference
  highlight \1  the first group, again
  code
      def doubled(text: Text): Boolean = text match
        case r"([a-z]+) \1" => true
        case _              => false
frame
  label An automaton, and a backreference
  errors show
  code
      import regexBackends.re2

      def doubled(text: Text): Boolean = text match
        case r"([a-z]+) \1" => true
        case _              => false
frame
  label An automaton, and a pattern it can run
  highlight regexBackends.re2  linear time, by construction
  code
      import regexBackends.re2

      def doubled(text: Text): Boolean = text match
        case r"([a-z]+) [a-z]+" => true
        case _                  => false
##
```

[Choosing an engine](../../soundness/modules/patterns.md#choosing-an-engine) compares the two.
