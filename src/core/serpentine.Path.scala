package serpentine

import anticipation.*
import gossamer.*
import prepositional.*
import contingency.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

object Path:
  given [ElementType, RootType <: AnyRef: Navigable by ElementType]
      => Encoder[Path on RootType by ElementType] as encoder = path =>
    path
     .descent
     .reverse
     .map(RootType.elementText(_))
     .join(RootType.rootText(path.root), RootType.separator, t"")
    
  given [ElementType, RootType <: AnyRef](using RootType is Navigable by ElementType, Tactic[PathError])
      => (Path on RootType by ElementType) is Addable by (Relative by ElementType) into
          (Path on RootType by ElementType) =
    (left, right) =>
      def recur(descent: List[ElementType], ascent: Int): Path on RootType by ElementType =
        if ascent > 0 then
          if descent.isEmpty
          then
            raise(PathError(PathError.Reason.RootParent))
            Path(left.root)(Nil)
          else recur(descent.tail, ascent - 1)
        else Path(left.root)(right.descent ++ descent)

      recur(left.descent, right.ascent)
        
  def apply[RootType <: AnyRef, ElementType](root0: RootType)(elements: List[ElementType])
      (using navigable: RootType is Navigable by ElementType)
          : Path on RootType by ElementType =
    new Path:
      type Platform = RootType
      type Operand = ElementType
      val root: Platform = root0
      val descent: List[Operand] = elements

  def parse[RootType <: AnyRef: Navigable](path: Text): Path on RootType =
    val root = RootType.prefix(path)

    val descent = path
     .skip(RootType.prefixLength(path))
     .cut(RootType.separator)
     .filter(_ != t"")
     .reverse
     .map(RootType.element(_))

    Path(root)(descent)

  given [RootType <: AnyRef, ElementType]
      => (Path on RootType by ElementType) is Divisible by ElementType into
          (Path on RootType by ElementType) =
    new Divisible:
      type Self = Path on RootType by ElementType
      type Result = Path on RootType by ElementType
      type Operand = ElementType

      def divide(path: Path on RootType by ElementType, child: ElementType)
              : Path on RootType by ElementType =
        new Path:
          type Platform = path.Platform
          type Operand = path.Operand
          val root: Platform = path.root
          val descent: List[Operand] = child :: path.descent

abstract class Path extends Pathlike:
  path =>

    type Platform <: AnyRef
    type Operand
    
    def root: Platform
    def descent: List[Operand]
    
    protected def pathRoot: AnyRef & Matchable = root
    protected def pathDescent: List[Operand] = descent
    
    def depth: Int = descent.length
    
    def parent(using Platform is Navigable by Operand): Optional[Path on Platform by Operand] =
      if descent == Nil then Unset else Path(root)(descent.tail)

    def conjunction(right: Path on Platform by Operand)(using Platform is Navigable by Operand)
            : Path on Platform by Operand =
      val difference = path.depth - right.depth
      val left0 = path.descent.drop(difference)
      val right0 = right.descent.drop(-difference)

      def recur(left: List[Operand], right: List[Operand], size: Int, count: Int)
              : Path on Platform by Operand =
        if left.isEmpty then Path(root)(left0.drop(size - count))
        else if left.head == right.head then recur(left.tail, right.tail, size + 1, count + 1)
        else recur(left.tail, right.tail, size + 1, 0)
      
      recur(left0, right0, 0, 0)

    def precedes(path: Path on Platform by Operand)(using Platform is Navigable by Operand)
            : Boolean =
      conjunction(path) == path

    def retain(count: Int)(using Platform is Navigable by Operand): Path on Platform by Operand =
      Path(root)(descent.drop(depth - count))

    def relativeTo(right: Path on Platform by Operand)(using Platform is Navigable by Operand)
            : Relative by Operand =
      val common = conjunction(right).depth
      Relative(right.depth - common, descent.dropRight(common))