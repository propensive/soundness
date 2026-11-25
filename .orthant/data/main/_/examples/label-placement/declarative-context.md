# Another algorithm, by an import

The algorithm which arranges the labels is the `Arranger` in scope. The code which describes
the labels never names it, so a crowded picture is given a more thorough one by an import,
and its body is not touched.

```scala fuel
compile
frame
  label The default places each label in turn
  code
      val cast = arrange:
        val viola = position(60.0, 12.0, 100.0, 100.0, reach = 20.0)
        val feste = position(60.0, 12.0, 102.0, 101.0, reach = 20.0)
        val maria = position(60.0, 12.0, 98.0, 99.0, reach = 20.0)
        List(viola.attachment, feste.attachment, maria.attachment)
frame
  label Simulated annealing considers them together
  highlight arrangers.annealingArranger  the only change
  code
      import arrangers.annealingArranger

      val cast = arrange:
        val viola = position(60.0, 12.0, 100.0, 100.0, reach = 20.0)
        val feste = position(60.0, 12.0, 102.0, 101.0, reach = 20.0)
        val maria = position(60.0, 12.0, 98.0, 99.0, reach = 20.0)
        List(viola.attachment, feste.attachment, maria.attachment)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(cast.inspect)
```

[Choosing an arranger](../../soundness/modules/label-placement.md#choosing-an-arranger) has
the parameters of both, and [declarative context](../../philosophy/declarative-context.md)
the reason an import is enough.
