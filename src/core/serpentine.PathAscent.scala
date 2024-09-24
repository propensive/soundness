package serpentine

import rudiments.*
import prepositional.*
import symbolism.*

object PathAscent:
  given [ElementType](using Navigable by ElementType)
      => PathAscent is Divisible by ElementType into (Relative by ElementType) =
    new Divisible:
      type Self = PathAscent
      type Result = Relative by ElementType
      type Operand = ElementType

      def divide(path: PathAscent, child: ElementType): Relative by ElementType =
        Relative(path.ascent, List(child))

case class PathAscent(ascent: Int) extends Relative:
  type Operand = Nothing
  val descent: List[Operand] = Nil

  @targetName("parent")
  infix def / (parent: ^.type): PathAscent = PathAscent(ascent + 1)

  override def parent(using navigable: Navigable by Nothing): PathAscent =
    PathAscent(ascent + 1)