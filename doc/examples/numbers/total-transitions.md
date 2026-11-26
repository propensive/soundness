# An index which has been proved

Reading a position of an array may find nothing there, so it gives an optional value. A
position handed out by the array's own `extent` has been proved to lie within it, and reading
with it gives the element itself, with no check.

```scala fuel
compile
frame
  label Any position may be out of range
  highlight Optional[Int]  the third element, if there is one
  code
      val lines = Array(1422, 3924, 2477)
      val third: Optional[Int] = lines(Ter)
frame
  label Treating it as present
  errors show
  code
      val lines = Array(1422, 3924, 2477)
      val third: Int = lines(Ter)
frame
  label A position from the array's own extent
  highlight lines(ordinal)  an Int, not an optional one
  code
      val lines = Array(1422, 3924, 2477)
      var total: Int = 0
      lines.extent.each { ordinal => total += lines(ordinal) }
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(total.show)
```

[Ordinals](../../soundness/modules/numbers.md#ordinals) describes intervals and `extent`, and
[total transitions](../../philosophy/total-transitions.md) why an index needs a proof.
