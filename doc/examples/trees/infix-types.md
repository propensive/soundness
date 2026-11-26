# A type which knows its children

A tree is drawn from its roots and a way to find the children of a node. The way can be given
at each drawing, or said once about the type, in a sentence: `Scene is Expandable`.

```scala fuel
compile
frame
  label The children, given where the tree is drawn
  highlight by[Scene](_.parts)  said at each drawing
  code
      import treeStyles.squareTreeStyle

      case class Scene(name: Text, parts: List[Scene] = Nil)

      val play = Scene
        ( t"Macbeth",
          List(Scene(t"Act I", List(Scene(t"A heath"))), Scene(t"Act II")) )
      val diagram = TreeDiagram.by[Scene](_.parts)(play)
frame
  label The children, as a fact about the type
  highlight given..Expandable  said once, about the type
  code
      import treeStyles.squareTreeStyle

      case class Scene(name: Text, parts: List[Scene] = Nil)

      given expandable: Scene is Expandable = _.parts

      val play = Scene
        ( t"Macbeth",
          List(Scene(t"Act I", List(Scene(t"A heath"))), Scene(t"Act II")) )
      val diagram = TreeDiagram(play)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.basicTermcap

diagram.render(_.name).each(Out.println(_))
```

[Supplying the tree](../../soundness/modules/trees.md#supplying-the-tree) compares the two, and
[infix types](../../philosophy/infix-types.md) explains how such a type is read.
