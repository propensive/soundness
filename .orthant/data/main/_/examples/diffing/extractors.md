# The edits of a difference, taken apart

A `Diff` is a sequence of edits, and each edit is an insertion, a deletion or an element kept.
A pattern match takes them apart, with the positions and the value of each bound by the
`case` which matches it.

```scala fuel
compile
frame
  label A difference
  code
      import Diff.{Del, Ins, Par}

      val cast = diff(Sequence(t"Lear", t"Kent"), Sequence(t"Lear", t"Fool", t"Kent"))
frame
  label Each kind of edit, by its shape
  highlight Ins(right..name)  the position and the element, bound
  code
      import Diff.{Del, Ins, Par}

      val cast = diff(Sequence(t"Lear", t"Kent"), Sequence(t"Lear", t"Fool", t"Kent"))

      def describe(edit: Diff.Edit[Text]): Text = edit match
        case Ins(right, name) => t"$name enters at position $right"
        case Del(left, _)     => t"an exit from position $left"
        case Par(_, _, _)     => t"no change"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

cast.edits.each(edit => Out.println(describe(edit)))
```

[Computing a diff](../../soundness/modules/diffing.md#computing-a-diff) explains each kind of
edit.
