package galilei

import anticipation.*
import denominative.*
import nomenclature.*
import prepositional.*
import symbolism.*
import rudiments.*
import contingency.*
import serpentine.*
import gossamer.*

object UnixRoot:
  // given [ElementType](using navigable: UnixRoot is Navigable by ElementType)
  //     => UnixRoot is Divisible by ElementType into (Path on UnixRoot by ElementType) =
  //   new Divisible:
  //     type Self = UnixRoot
  //     type Result = Path on UnixRoot by ElementType
  //     type Operand = ElementType

  //     def divide(root: UnixRoot, child: ElementType): Path on UnixRoot by ElementType =
  //       new Path:
  //         type Platform = UnixRoot
  //         type Operand = ElementType
  //         val root: UnixRoot = %
  //         val descent: List[Operand] = List(child)

  given (using Tactic[PathError], Tactic[NameError]) => Unix is Navigable by
      Name[Unix] under MustNotContain["/"] & MustNotEqual["."] & MustNotEqual[".."] as navigable =
    new Navigable:
      type Self = Unix
      type Operand = Name[Unix]
      type Constraint = MustNotContain["/"] & MustNotEqual["."] & MustNotEqual[".."]

      val separator: Text = t"/"
      val parentElement: Text = t".."
      val selfText: Text = t"."

      def element(element: Text): Name[Unix] = Name(element)
      def prefixLength(path: Text): Int = 1
      def elementText(element: Name[Unix]): Text = element.text
      def rootText(root: Root on Unix): Text = t"/"
    
      def prefix(path: Text): Root on Unix =
        if path.at(Prim) == '/' then %
        else raise(PathError(PathError.Reason.InvalidRoot, path)) yet %

class UnixRoot() extends Root:
  type Operand = Name[Unix]
  type Platform = Unix
  def root = this

  // override def precedes(path: Path on UnixRoot by Name[Unix])
  //     (using UnixRoot is Navigable by Name[Unix])
  //         : Boolean =
  //   true