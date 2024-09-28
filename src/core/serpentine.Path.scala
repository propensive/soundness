package serpentine

import anticipation.*
import gossamer.*
import prepositional.*
import contingency.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

import scala.compiletime.*

object Path:
  given [PlatformType: Navigable] => Encoder[Path on PlatformType] as encoder = path =>
    path.descent.reverse.join(path.root, PlatformType.separator, t"")
    
  given [PlatformType: Navigable](using Tactic[PathError])
      => (Path on PlatformType) is Addable by (Relative by PlatformType.Operand) into
          (Path on PlatformType) =
    (left, right) =>
      def recur(descent: List[Text], ascent: Int): Path on PlatformType =
        if ascent > 0 then
          if descent.isEmpty then
            raise(PathError(PathError.Reason.RootParent))
            Path[PlatformType](left.root, Nil)
          else recur(descent.tail, ascent - 1)
        else Path[PlatformType](left.root, right.descent ++ descent)

      recur(left.descent, right.ascent)
        
  def apply[ElementType, PlatformType: Navigable by ElementType](root0: Root on PlatformType)
      (elements: List[ElementType])
          : Path on PlatformType =
    if elements.isEmpty then root0
    else Path[PlatformType](PlatformType.rootText(root0), elements.map(PlatformType.elementText(_)))
  
  def apply[PlatformType](root0: Text, elements: List[Text]): Path on PlatformType =
    new Path(root0, elements):
      type Platform = PlatformType

  def parse[PlatformType: Navigable](path: Text): Path on PlatformType =
    val root = PlatformType.root(path)

    val descent = path
     .skip(PlatformType.rootLength(path))
     .cut(PlatformType.separator)
     .filter(_ != t"")
     .reverse
     .map(PlatformType.element(_))

    Path(root)(descent)

  given [PlatformType: Navigable]
      => (Path on PlatformType) is Divisible by PlatformType.Operand into (Path on PlatformType) =
    new Divisible:
      type Operand = PlatformType.Operand
      type Self = Path on PlatformType
      type Result = Path on PlatformType

      def divide(path: Path on PlatformType, child: PlatformType.Operand): Path on PlatformType =
        Path[path.Platform](path.root, PlatformType.elementText(child) :: path.descent)

abstract class Path(val root: Text, val descent: List[Text]) extends Pathlike:
  type Platform
  
  def depth: Int = descent.length

  override def equals(that: Any): Boolean = that.asMatchable match
    case that: Path => (root == that.root) && descent == that.descent
    case _          => false

  override def hashCode: Int = root.toString.hashCode*31 + descent.hashCode

  def parent(using Platform is Navigable): Optional[Path on Platform] =
    if descent == Nil then Unset else Path(root, descent.tail)

  def conjunction(right: Path on Platform)(using Platform is Navigable): Path on Platform =
    val difference = depth - right.depth
    val left0 = descent.drop(difference)
    val right0 = right.descent.drop(-difference)

    def recur(left: List[Text], right: List[Text], size: Int, count: Int)
            : Path on Platform =
      if left.isEmpty then Path(root, left0.drop(size - count))
      else if left.head == right.head then recur(left.tail, right.tail, size + 1, count + 1)
      else recur(left.tail, right.tail, size + 1, 0)
    
    recur(left0, right0, 0, 0)

  def precedes(path: Path on Platform)(using Platform is Navigable): Boolean =
    descent == Nil || conjunction(path) == path

  def retain(count: Int)(using Platform is Navigable): Path on Platform =
    Path(root, descent.drop(depth - count))

  def relativeTo(right: Path on Platform)(using navigable: Platform is Navigable)
          : Relative by navigable.Operand =
    val common = conjunction(right).depth
    Relative(right.depth - common, descent.dropRight(common).map(navigable.element(_)))
