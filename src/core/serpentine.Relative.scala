package serpentine

import anticipation.*
import prepositional.*
import gossamer.*
import symbolism.*
import spectacular.*
import rudiments.*
import vacuous.*

import scala.compiletime.*

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

  def parse[ElementType](using navigable: Navigable by ElementType)(text: Text)
          : Relative by ElementType =
    def recur(start: Int, ascent: Int, elements: List[ElementType]): Relative by ElementType =
      if start >= text.length then Relative(ascent, elements)
      else
        val end = text.s.indexOf(navigable.separator.s, start).puncture(-1).or(text.length)
        val element = text.s.substring(start, end).nn.tt
        val start2 = end + navigable.separator.length

        if element == navigable.parentElement then
          if elements.isEmpty then recur(start2, ascent + 1, Nil)
          else recur(start2, ascent, elements.tail)
        else recur(start2, ascent, navigable.element(element) :: elements)
    
    if text == navigable.selfText then Relative(0, Nil) else recur(0, 0, Nil)

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
  type Operand
  val ascent: Int
  val descent: List[Operand]

  def delta: Int = descent.length - ascent

  def parent(using Navigable by Operand): Relative =
    if descent.isEmpty then Relative(ascent + 1, Nil) else Relative(ascent, descent.tail)

  override def equals(that: Any): Boolean = that.asMatchable match
    case that: Relative => that.ascent == ascent && that.descent == descent
    case _              => false
  
  override def hashCode: Int = ascent*31 + descent.hashCode

  @targetName("child")
  infix def / (element: Operand)(using navigable: Navigable by Operand)
          : Relative by Operand =
    Relative(ascent, element :: descent)
