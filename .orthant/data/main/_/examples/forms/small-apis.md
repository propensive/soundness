# A submission, decoded like any text

A form's submission is read with `as`, the method which reads a number or a date: once to take
the posted text apart as a `Query`, and once to decode the query as the type the form was
derived from.

```scala fuel
compile
frame
  label The posted text, as a query
  highlight as[Query]  keys and values
  code
      import strategies.throwUnsafely

      case class Troupe(name: Text, players: Int)

      val query = t"name=Mechanicals&players=6".as[Query]
frame
  label The query, as the type
  highlight as[Troupe]  the fields, at their types
  code
      import strategies.throwUnsafely

      case class Troupe(name: Text, players: Int)

      val query = t"name=Mechanicals&players=6".as[Query]
      val troupe = query.as[Troupe]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(troupe.inspect)
```

[Submissions](../../soundness/modules/forms.md#submissions) covers nested fields, which are
dotted in the query.
