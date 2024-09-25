package galilei

import anticipation.*
import denominative.*
import prepositional.*
import symbolism.*
import rudiments.*
import contingency.*
import serpentine.*
import gossamer.*

object UnixRoot:
  given [ElementType](using navigable: UnixRoot is Navigable by ElementType)
      => UnixRoot is Divisible by ElementType into (Path on UnixRoot by ElementType) =
    new Divisible:
      type Self = UnixRoot
      type Result = Path on UnixRoot by ElementType
      type Operand = ElementType

      def divide(root: UnixRoot, child: ElementType): Path on UnixRoot by ElementType =
        new Path:
          type Platform = UnixRoot
          type Operand = ElementType
          val root: UnixRoot = %
          val descent: List[Operand] = List(child)

  given (using Tactic[PathError]) => UnixRoot is Navigable by Text as navigable =
    new Navigable:
      type Self = UnixRoot
      type Operand = Text

      val separator: Text = t"/"
      val parentElement: Text = t".."
      val selfText: Text = t"."

      def element(element: Text): Text = element
      def prefixLength(path: Text): Int = 1
      def elementText(element: Text): Text = element
      def rootText(root: UnixRoot): Text = t"/"
    
      def prefix(path: Text): UnixRoot =
        if path.at(Prim) == '/' then %
        else raise(PathError(PathError.Reason.InvalidRoot, path)) yet %

class UnixRoot() extends Path:
  type Platform = UnixRoot
  type Operand = Text
  def descent: List[Nothing] = Nil
  def root: UnixRoot = this

  override def precedes(path: Path on UnixRoot by Text)(using UnixRoot is Navigable by Text)
          : Boolean =
    true