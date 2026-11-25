# The present values of a list

Optional values and collections meet without ceremony. A list of optional values is an
ordinary list, and `compact` reduces it to the values which are present, in a list whose type
no longer mentions absence.

```scala fuel
compile
frame
  label A cast with gaps
  highlight List[Optional[Text]]  some parts are not yet cast
  code
      val cast: List[Optional[Text]] = List(t"Viola", Unset, t"Feste", Unset)
frame
  label Only the parts which are filled
  highlight List[Text]  nothing optional remains
    line 2
  code
      val cast: List[Optional[Text]] = List(t"Viola", Unset, t"Feste", Unset)
      val players: List[Text] = cast.compact
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(players.inspect)
```

[Absence from collections](../../soundness/modules/optional-values.md#absence-from-collections)
has the other direction, in which a collection gives an optional value.
