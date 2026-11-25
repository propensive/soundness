# One method for a function of either kind

An `Option` needs `map` for a function which gives a value, and `flatMap` for one which gives
an `Option`. Optionality is flat, so `let` takes both, and the result is an optional value
either way.

```scala fuel
compile
frame
  label A function which always gives a value
  highlight let(_.upper)  as map would
  code
      val name: Optional[Text] = t"Hamlet"
      val shout: Optional[Text] = name.let(_.upper)
frame
  label A function which may give none
  highlight let(initial)  as flatMap would, with no second layer
  code
      def initial(text: Text): Optional[Char] = text.chars.head

      val name: Optional[Text] = t"Hamlet"
      val shout: Optional[Text] = name.let(_.upper)
      val letter: Optional[Char] = name.let(initial)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(shout.inspect)
Out.println(letter.inspect)
```

[Transforming](../../soundness/modules/optional-values.md#transforming) has `lay`, which
handles both cases at once.
