# Absence, dealt with where it matters

An optional value cannot be used as a plain one until the absent case has been answered.
`let` works on a value which is present, and `or` says what stands in for one which is not.

```scala fuel
compile
frame
  label An optional value used as a plain one
  errors show
  code
      val title: Optional[Text] = Unset
      val heading: Text = title
frame
  label A fallback
  highlight or  the value to use in its absence
    line 2
  code
      val title: Optional[Text] = Unset
      val heading: Text = title.or(t"untitled")
frame
  label A change to a present value, first
  highlight let(_.upper)  applies only to a present value
  code
      val title: Optional[Text] = Unset
      val heading: Text = title.let(_.upper).or(t"untitled")
frame
  label Both at once
  highlight lay(t"untitled")(_.upper)  the fallback, then the function
  code
      val title: Optional[Text] = Unset
      val heading: Text = title.lay(t"untitled")(_.upper)
##
```

[Supplying a fallback](../../soundness/modules/optional-values.md#supplying-a-fallback) and
[Transforming](../../soundness/modules/optional-values.md#transforming) describe each, and
[optionality](../../philosophy/optionality.md) the reasons.
