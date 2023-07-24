package dendrology

import probably.*
import rudiments.*
import gossamer.*
import turbulence.*, basicIo.jvm

import unsafeExceptions.canThrowAny

object Tests extends Suite(t"Dendrology tests"):

  case class Tree(value: Text, children: List[Tree] = Nil)

  val life = List(
    Tree(t"Plantae", List()),
    Tree(t"Fungi", List(
      Tree(t"Rozellomyceta", List()),
      Tree(t"Aphelidiomyceta", List()),
      Tree(t"Eumycota", List()),
    )),
    Tree(t"Protozoa", List()),
    Tree(t"Bacteria", List()),
    Tree(t"Animalia", List(
      Tree(t"Chordata", List(
        Tree(t"Mammalia", List(
          Tree(t"Carnivora", List(
            Tree(t"Feliadae", List()),
            Tree(t"Canidae", List(
              Tree(t"Canis", List()),
            )),
            Tree(t"Ursidae", List())
          ))
        ))
      ))
    ))
  )

  def run(): Unit =
    import treeStyles.default
    def line(tiles: List[TreeTile], tree: Tree): Text = t"${tiles.map(_.text).join}● ${tree.value}"
    drawTree[Tree, Text](_.children, line)(life).foreach(Io.println(_))