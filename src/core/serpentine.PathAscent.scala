package serpentine

import rudiments.*
import prepositional.*
import anticipation.*
import gossamer.*
import spectacular.*
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

  given [ElementType, RootType: Navigable by ElementType] => Encoder[PathAscent] =
    pathAscent =>
      if pathAscent.textDescent.isEmpty
      then
        if pathAscent.ascent == 0 then RootType.selfText
        else List.fill(pathAscent.ascent)(RootType.parentElement).join(RootType.separator)
      else pathAscent
       .textDescent
       .reverse
       .join(RootType.ascent*pathAscent.ascent, RootType.separator, t"")

case class PathAscent(ascent: Int) extends Relative:
  type Operand = Nothing
  val textDescent: List[Operand] = Nil

  @targetName("parent")
  infix def / (parent: ^.type): PathAscent = PathAscent(ascent + 1)

  override def parent: PathAscent = PathAscent(ascent + 1)