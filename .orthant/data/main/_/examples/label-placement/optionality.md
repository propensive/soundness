# A leader which may not be there

A label which had to be moved far from its point has a leader, a line back to what it names,
and a label which stayed has none. The leader is therefore an optional value, and cannot be
drawn before the code has said what its absence means.

```scala fuel
compile
frame
  label A leader, used as though it were always there
  errors show
  code
      val label = arrange:
        avoid(Obstacle.Box(0.0, 0.0, 200.0, 110.0))
        position(60.0, 12.0, 100.0, 100.0, List(Caption.Attachment.South), reach = 40.0)

      val length = label.leader.y1 - label.leader.y2
frame
  label A length where there is a leader, and zero where there is none
  highlight lay(0.0)  the value which stands for no leader
  code
      val label = arrange:
        avoid(Obstacle.Box(0.0, 0.0, 200.0, 110.0))
        position(60.0, 12.0, 100.0, 100.0, List(Caption.Attachment.South), reach = 40.0)

      val length = label.leader.lay(0.0): leader =>
        leader.y1 - leader.y2
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(label.leader.present.inspect)
Out.println(length.inspect)
```

[Displacement and leaders](../../soundness/modules/label-placement.md#displacement-and-leaders)
says when a label gets one, and [optionality](../../philosophy/optionality.md) describes `lay`
and its relatives.
