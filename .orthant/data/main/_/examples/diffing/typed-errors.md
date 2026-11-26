# An edit which may not apply

A redraft names the lines to remove and to add, and finds for itself where they belong. A
line which is not in the source cannot be found, so `patch` declares a `Redraft.Error`, and
code which applies an edit must say what happens when it does not fit.

```scala fuel
compile
frame
  label An edit, applied on trust
  errors show
  code
      val scene = Sequence(t"Enter Lear", t"Enter Kent", t"Exeunt")

      def revise(edit: Chain[Text]): Chain[Text] =
        Redraft.parse(edit).patch(scene)
frame
  label The failure, in the return type
  highlight raises..Redraft.Error  the caller must answer for it
  code
      val scene = Sequence(t"Enter Lear", t"Enter Kent", t"Exeunt")

      def revise(edit: Chain[Text]): Chain[Text] raises Redraft.Error =
        Redraft.parse(edit).patch(scene)
frame
  label Or the scene as it was
  highlight recover  an edit which does not fit is not applied
  code
      val scene = Sequence(t"Enter Lear", t"Enter Kent", t"Exeunt")

      def revise(edit: Chain[Text]): Chain[Text] =
        recover:
          case Redraft.Error(_, _, _) => scene.to[Chain]
        . protect(Redraft.parse(edit).patch(scene))
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

revise(Chain(t"- Enter Kent", t"+ Enter Fool")).each(Out.println(_))
revise(Chain(t"- Enter Cordelia")).each(Out.println(_))
```

The second edit names a line which the scene does not have, and the scene is printed
unchanged. [Redrafts](../../soundness/modules/diffing.md#redrafts) describes the format.
