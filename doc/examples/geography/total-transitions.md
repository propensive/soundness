# An angle which turns past north

Adding two angles is defined for every pair of them, since the sum of two headings is a
heading. Arithmetic wraps at a full turn, and `canonical` gives the same angle as a turn to
the left or the right of north.

```scala fuel
compile
frame
  label Twenty degrees past 350
  highlight +  wraps at a full turn
  code
      val heading: Angle = 350.0.deg + 20.0.deg
frame
  label Thirty degrees back again
  highlight -  wraps below zero
  code
      val heading: Angle = 350.0.deg + 20.0.deg
      val turned: Angle = heading - 30.0.deg
frame
  label The same angle, to the left of north
  highlight canonical  between −180° and 180°
  code
      val heading: Angle = 350.0.deg + 20.0.deg
      val turned: Angle = heading - 30.0.deg
      val signed: Angle = turned.canonical
##
```

```scala fuel
run
##
import soundness.angleShowable
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(heading.show)
Out.println(turned.show)
Out.println(signed.show)
```

[Angles](../../soundness/modules/geography.md#angles) has both normal forms, and
[total transitions](../../philosophy/total-transitions.md) the principle.
