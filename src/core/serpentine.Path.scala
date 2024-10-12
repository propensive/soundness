package serpentine

import anticipation.*
import gossamer.*
import prepositional.*
import contingency.*
import rudiments.*
import spectacular.*
import symbolism.*
import fulminate.*
import vacuous.*

import scala.compiletime.*

object Path:
  given Encoder[Path] as encoder = _.text
  
  given [PlatformType: {Navigable, Radical}] => Decoder[Path on PlatformType] as decoder =
    Path.parse(_)

  given [PlatformType] => (Path on PlatformType) is Showable as showable = _.text
  given [PlatformType] => (Path on PlatformType) is GenericPath as generic = _.text
  
  given [PlatformType: {Navigable, Radical}] => Path on PlatformType is SpecificPath as specific =
    _.decode[Path on PlatformType]
  
  given Path is Communicable as communicable = path =>
    Message(path.textDescent.reverse.join(path.textRoot, path.separator, t""))
    
  given [PlatformType: Navigable](using Tactic[PathError])
      => (Path on PlatformType) is Addable by (Relative by PlatformType.Operand) into
          (Path on PlatformType) as addable =
    (left, right) =>
      def recur(descent: List[Text], ascent: Int): Path on PlatformType =
        if ascent > 0 then
          if descent.isEmpty then
            raise(PathError(PathError.Reason.RootParent))
            
            Path.from[PlatformType]
             (left.textRoot, Nil, left.separator, left.caseSensitivity)
          else recur(descent.tail, ascent - 1)
        else
          Path.from[PlatformType]
           (left.textRoot,
            right.textDescent ++ descent,
            PlatformType.separator,
            PlatformType.caseSensitivity)

      recur(left.textDescent, right.ascent)

  def apply
      [RootType <: Root on PlatformType,
       ElementType,
       PlatformType: {Navigable by ElementType, Radical from RootType}]
      (root0: RootType, elements: List[ElementType])
          : Path on PlatformType =
    if elements.isEmpty then root0 else
      Path.from[PlatformType]
       (PlatformType.rootText(root0),
        elements.map(PlatformType.makeElement(_)),
        PlatformType.separator,
        PlatformType.caseSensitivity)
  
  private def from[PlatformType]
      (root0: Text, elements: List[Text], separator: Text, caseSensitivity: Case)
          : Path on PlatformType =
    new Path(root0, elements, separator, caseSensitivity):
      type Platform = PlatformType
  
  def parse[PlatformType: {Navigable, Radical}](path: Text): Path on PlatformType =
    val root = PlatformType.root(path)

    val descent = path
     .skip(PlatformType.rootLength(path))
     .cut(PlatformType.separator)
     .filter(_ != t"")
     .reverse
     .map(PlatformType.element(_))

    Path(root, descent)

  given [PlatformType: Navigable]
      => (Path on PlatformType) is Divisible by PlatformType.Operand into (Path on PlatformType) =
    new Divisible:
      type Operand = PlatformType.Operand
      type Self = Path on PlatformType
      type Result = Path on PlatformType

      def divide(path: Path on PlatformType, child: PlatformType.Operand): Path on PlatformType =
        Path.from[path.Platform]
         (path.textRoot,
          PlatformType.makeElement(child) :: path.textDescent,
          PlatformType.separator,
          PlatformType.caseSensitivity)

open class Path
    (val textRoot: Text,
     val textDescent: List[Text],
     val separator: Text,
     val caseSensitivity: Case)
extends Pathlike:
  type Platform
  
  def depth: Int = textDescent.length
  def root(using radical: Platform is Radical): radical.Source = radical.root(textRoot)
  def text: Text = textDescent.reverse.join(textRoot, separator, t"")
  
  def name(using navigable: Platform is Navigable): Optional[navigable.Operand] =
    textDescent.prim.let(navigable.element(_))
  
  def peer(using navigable: Platform is Navigable)(name: navigable.Operand)
          : Path on Platform raises PathError =
    parent.let(_ / name).or(abort(PathError(PathError.Reason.RootParent)))

  def descent(using navigable: Platform is Navigable): List[navigable.Operand] =
    textDescent.reverse.map(navigable.element(_))

  def child(filename: Text)(using Unsafe): Path on Platform =
    Path.from(textRoot, filename :: textDescent, separator, caseSensitivity)

  override def toString(): String = text.s
  
  override def equals(that: Any): Boolean = that.asMatchable match
    case that: Path =>
      (textRoot == that.textRoot) && caseSensitivity.equal(textDescent, that.textDescent)
    
    case _ =>
      false

  override def hashCode: Int =
    separator.hashCode + textRoot.toString.hashCode*31 + caseSensitivity.hash(textDescent)

  def parent: Optional[Path on Platform] =
    if textDescent == Nil then Unset
    else Path.from(textRoot, textDescent.tail, separator, caseSensitivity)

  def ancestor(n: Int): Optional[Path on Platform] =
    def recur(n: Int, current: Optional[Path on Platform]): Optional[Path on Platform] =
      current.let: current =>
        if n == 0 then current else recur(n - 1, current.parent)

    recur(n, this)

  def ancestors: List[Path on Platform] = parent.let { parent => parent :: parent.ancestors }.or(Nil)

  transparent inline def on [PlatformType]: Path on PlatformType =
    inline erasedValue[PlatformType & Matchable] match
      case _: Platform => this.asInstanceOf[Path on PlatformType]
      case _ =>
        summonInline[PlatformType is Navigable].give:
          summonInline[PlatformType is Radical].give(Path.parse(text))

  def conjunction(right: Path on Platform): Path on Platform =
    val difference = depth - right.depth
    val left0 = textDescent.drop(difference)
    val right0 = right.textDescent.drop(-difference)

    def recur(left: List[Text], right: List[Text], size: Int, count: Int)
            : Path on Platform =
      if left.isEmpty
      then Path.from(textRoot, left0.drop(size - count), separator, caseSensitivity)
      else if left.head == right.head then recur(left.tail, right.tail, size + 1, count + 1)
      else recur(left.tail, right.tail, size + 1, 0)
    
    recur(left0, right0, 0, 0)

  def precedes(path: Path on Platform): Boolean = textDescent == Nil || conjunction(path) == path
  
  def retain(count: Int): Path on Platform =
    Path.from
     (textRoot, textDescent.drop(depth - count), separator, caseSensitivity)

  def relativeTo(right: Path on Platform)(using navigable: Platform is Navigable)
          : Relative by navigable.Operand raises PathError =
    if textRoot != right.textRoot then raise(PathError(PathError.Reason.DifferentRoots, right.text))
    val common = conjunction(right).depth
    Relative(right.depth - common, textDescent.dropRight(common).map(navigable.element(_)))
  
  def resolve(text: Text)(using Platform is Navigable, Platform is Radical)
          : Path on Platform raises PathError =
    safely(Path.parse(text)).or(safely(this + Relative.parse(text))).or:
      abort(PathError(PathError.Reason.InvalidRoot))

