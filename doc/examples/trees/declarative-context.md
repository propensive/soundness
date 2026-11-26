# The same tree, in another style

A `TreeDiagram` knows which lines branch and which continue, and not what a branch looks like.
The characters come from a style, which an import chooses, and the code which draws the tree
does not change.

```scala fuel
compile
alternative Square
  highlight squareTreeStyle  square corners
  code
      import treeStyles.squareTreeStyle

      case class Scene(name: Text, parts: List[Scene] = Nil)

      val play = Scene
        ( t"Macbeth",
          List(Scene(t"Act I", List(Scene(t"A heath"))), Scene(t"Act II")) )
      val lines = TreeDiagram.by[Scene](_.parts)(play).render(_.name)
alternative Rounded
  highlight roundedTreeStyle  curved corners
  code
      import treeStyles.roundedTreeStyle

      case class Scene(name: Text, parts: List[Scene] = Nil)

      val play = Scene
        ( t"Macbeth",
          List(Scene(t"Act I", List(Scene(t"A heath"))), Scene(t"Act II")) )
      val lines = TreeDiagram.by[Scene](_.parts)(play).render(_.name)
alternative ASCII
  highlight asciiTreeStyle  nothing beyond ASCII
  code
      import treeStyles.asciiTreeStyle

      case class Scene(name: Text, parts: List[Scene] = Nil)

      val play = Scene
        ( t"Macbeth",
          List(Scene(t"Act I", List(Scene(t"A heath"))), Scene(t"Act II")) )
      val lines = TreeDiagram.by[Scene](_.parts)(play).render(_.name)
##
```

[Trees](../../soundness/modules/trees.md#trees) prints each of the three, and
[declarative context](../../philosophy/declarative-context.md) explains why the style is not
an argument.
