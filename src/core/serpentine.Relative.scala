package serpentine

import prepositional.*
import gossamer.*
import symbolism.*
import spectacular.*
import rudiments.*

object Relative:
  given [ElementType, RootType: Navigable by ElementType] => Encoder[Relative by ElementType] =
    relative =>
      if relative.descent.isEmpty
      then
        if relative.ascent == 0 then RootType.selfText
        else List.fill(relative.ascent)(RootType.parentElement).join(RootType.separator)
      else relative
       .descent
       .map(RootType.elementText)
       .reverse
       .join(RootType.ascent*relative.ascent, RootType.separator, t"")

  def apply[ElementType](using navigable: Navigable by ElementType)
      (ascent0: Int, descent0: List[ElementType])
          : Relative by ElementType =
    new Relative:
      type Operand = navigable.Operand
      val ascent: Int = ascent0
      val descent: List[Operand] = descent0

  given [ElementType](using Navigable by ElementType)
      => (Relative by ElementType) is Addable by (Relative by ElementType) into
          (Relative by ElementType) =
    (left, right) =>
      def recur(ascent: Int, descent: List[ElementType], ascent2: Int): Relative by ElementType =
        if ascent2 > 0 then
          if descent.isEmpty then recur(ascent + 1, Nil, ascent - 1)
          else recur(ascent, descent.tail, ascent - 1)
        else Relative(ascent, right.descent ++ descent)

      recur(left.ascent, left.descent, right.ascent)
        
abstract class Relative extends Pathlike:
  override def toString(): String = descent.reverse.mkString("../"*ascent, "/", "")

  type Operand
  val ascent: Int
  val descent: List[Operand]

  def delta: Int = descent.length - ascent

  def parent(using Navigable by Operand): Relative =
    if descent.isEmpty then Relative(ascent + 1, Nil) else Relative(ascent, descent.tail)

  protected def pathRoot: java.lang.Integer = ascent
  protected def pathDescent: List[Operand] = descent

  @targetName("child")
  infix def / (element: Operand)(using navigable: Navigable by Operand)
          : Relative by Operand =
    Relative(ascent, element :: descent)
