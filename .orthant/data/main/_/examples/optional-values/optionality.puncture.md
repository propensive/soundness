# An empty answer is no answer

Data from outside often marks absence with a value of its own: an empty string, a zero, a
`-1`. `puncture` turns that one value into `Unset` where the data arrives, and from there on
the compiler sees that it is handled.

```scala fuel
compile
frame
  label A sentinel which looks like a value
  code
      val answer: Text = t""
      val greeting: Text = t"Good morrow, $answer"
frame
  label The sentinel, made absent
  highlight puncture(t"")  empty text becomes Unset
  code
      val answer: Text = t""
      val name: Optional[Text] = answer.puncture(t"")
      val greeting: Text = t"Good morrow, ${name.or(t"stranger")}"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(greeting)
```

[Building an optional](../../soundness/modules/optional-values.md#building-an-optional) has
`unless` and `only`, which take a condition.
