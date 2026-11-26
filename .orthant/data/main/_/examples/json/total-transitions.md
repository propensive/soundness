# A step to a field which is not there

Stepping into a document is defined for every key, present or not. A missing field gives an
absent value, which may be stepped into again, and nothing fails until the code asks for a
type. Asked for an optional type, absence is an answer.

```scala fuel
compile
frame
  label Two steps into a document
  highlight (t"patron")(t"title")  each step is safe
  code
      import strategies.throwUnsafely

      val feste: Json = j"""{"name": "Feste", "patron": {"title": "Countess"}}"""
      val title: Optional[Text] = feste(t"patron")(t"title").as[Optional[Text]]
frame
  label The same steps, where nothing is
  highlight (t"master")(t"title")  absent, and still safe
  code
      import strategies.throwUnsafely

      val feste: Json = j"""{"name": "Feste", "patron": {"title": "Countess"}}"""
      val title: Optional[Text] = feste(t"patron")(t"title").as[Optional[Text]]
      val rank: Optional[Text] = feste(t"master")(t"title").as[Optional[Text]]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(title.inspect)
Out.println(rank.inspect)
```

[Navigating](../../soundness/modules/json.md#navigating) shows the error which a required type
gives, and [total transitions](../../philosophy/total-transitions.md) the principle.
