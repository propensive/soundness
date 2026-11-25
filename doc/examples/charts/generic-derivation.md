# The cases of an enumeration as categories

An axis of categories needs no list of its labels. An enumeration is categorical because the
compiler knows its cases, so a series keyed by one is drawn as bars, and a value which is not
a case cannot be plotted.

```scala fuel
compile
frame
  label Categories as text
  code
      val plays = Series(t"plays")((t"Comedy", 14.0), (t"History", 10.0), (t"Tragedy", 12.0))

      val chart = plays.chart(Bars())
frame
  label Categories as an enumeration
  highlight Genre.Comedy  a case, which cannot be misspelled
  code
      enum Genre:
        case Comedy, History, Tragedy

      val plays = Series(t"plays")
        ( (Genre.Comedy, 14.0), (Genre.History, 10.0), (Genre.Tragedy, 12.0) )

      val chart = plays.chart(Bars())
##
```

[Series](../../soundness/modules/charts.md#series) says which types are categorical and which
are continuous.
