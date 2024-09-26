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
  given [ElementType, PlatformType: Navigable by ElementType]
      => Encoder[Path on PlatformType by ElementType] as encoder = path =>
    path.descent.reverse
     .map(PlatformType.elementText(_))
     .join(PlatformType.rootText(path.root), PlatformType.separator, t"")
    
  given [ElementType, PlatformType](using PlatformType is Navigable by ElementType, Tactic[PathError])
      => (Path on PlatformType by ElementType) is Addable by (Relative by ElementType) into
          (Path on PlatformType by ElementType) =
    (left, right) =>
      def recur(descent: List[ElementType], ascent: Int): Path on PlatformType by ElementType =
        if ascent > 0 then
          if descent.isEmpty then
            raise(PathError(PathError.Reason.RootParent))
            Path(left.root)(Nil)
          else recur(descent.tail, ascent - 1)
        else Path(left.root)(right.descent ++ descent)

      recur(left.descent, right.ascent)
        
  def apply[PlatformType, ElementType](root0: Root on PlatformType)(elements: List[ElementType])
      (using navigable: PlatformType is Navigable by ElementType)
          : Path on PlatformType by ElementType =
    if elements.isEmpty then root0.asInstanceOf[Path on PlatformType by ElementType] else new Path:
      type Platform = PlatformType
      type Operand = ElementType
      val root: Root on Platform = root0
      val descent: List[Operand] = elements

  def parse[PlatformType: Navigable](path: Text): Path on PlatformType =
    val root = PlatformType.prefix(path)

    val descent = path
     .skip(PlatformType.prefixLength(path))
     .cut(PlatformType.separator)
     .filter(_ != t"")
     .reverse
     .map(PlatformType.element(_))

    Path(root)(descent)

  given [PlatformType, ElementType]
      => (Path on PlatformType by ElementType) is Divisible by ElementType into
          (Path on PlatformType by ElementType) =
    new Divisible:
      type Self = Path on PlatformType by ElementType
      type Result = Path on PlatformType by ElementType
      type Operand = ElementType

      def divide(path: Path on PlatformType by ElementType, child: ElementType)
              : Path on PlatformType by ElementType =
        new Path:
          type Platform = path.Platform
          type Operand = path.Operand
          val root: Root on Platform = path.root
          val descent: List[Operand] = child :: path.descent

abstract class Path extends Pathlike:
  path =>
    type Platform
    type Operand
    
    def root: Root on Platform
    def descent: List[Operand]
    def depth: Int = descent.length

    override def equals(that: Any): Boolean = that match
      case that: Path => (root.toString == that.root.toString) && descent == that.descent

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
      descent == Nil || conjunction(path) == path

    def retain(count: Int)(using Platform is Navigable by Operand): Path on Platform by Operand =
      Path(root)(descent.drop(depth - count))

    def relativeTo(right: Path on Platform by Operand)(using Platform is Navigable by Operand)
            : Relative by Operand =
      val common = conjunction(right).depth
      Relative(right.depth - common, descent.dropRight(common))
      
    infix def / (path: Path on Platform by Operand, child: Operand): Path on Platform by Operand =
      new Path:
        type Platform = path.Platform
        type Operand = path.Operand
        val root: Root on Platform = path.root
        val descent: List[Operand] = child :: path.descent
