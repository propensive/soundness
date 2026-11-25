# A style of one's own

A tree's style is four short pieces of text: a space, a last branch, a branch, and a line
carried down. A new style is a value made from the four, and a `given` puts it in the context
where an import would have put one of the standard styles.

```scala fuel
compile
frame
  label A standard style
  highlight squareTreeStyle  chosen by import
  code
      import treeStyles.squareTreeStyle

      case class Scene(name: Text, parts: List[Scene] = Nil)

      val play = Scene
        ( t"Macbeth",
          List(Scene(t"Act I", List(Scene(t"A heath"))), Scene(t"Act II")) )
      val lines = TreeDiagram.by[Scene](_.parts)(play).render(_.name)
frame
  label A style made here
  highlight TreeDiagram.TextualStyle[Text]  a space, a last branch, a branch, an extender
  code
      given style: TreeDiagram.TextualStyle[Text] =
        TreeDiagram.TextualStyle[Text](t"   ", t" `-", t" |-", t" | ")

      case class Scene(name: Text, parts: List[Scene] = Nil)

      val play = Scene
        ( t"Macbeth",
          List(Scene(t"Act I", List(Scene(t"A heath"))), Scene(t"Act II")) )
      val lines = TreeDiagram.by[Scene](_.parts)(play).render(_.name)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.basicTermcap

lines.each(Out.println(_))
```

[Structure and style apart](../../soundness/modules/trees.md#structure-and-style-apart) has the
four tiles, and [declarative context](../../philosophy/declarative-context.md) the reason a
style is not an argument.
