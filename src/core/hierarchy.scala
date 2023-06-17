package serpentine

import rudiments.*
import digression.*
import spectacular.*
import gossamer.*
import kaleidoscope.*

import scala.compiletime.*
import scala.quoted.*

object PathError:
  enum Reason:
    case InvalidChar(char: Char)
    case InvalidName(name: Text)
    case ParentOfRoot
    case NotRooted

  given Show[Reason] =
    case Reason.InvalidChar(char) => t"the character '$char' cannot appear in a path"
    case Reason.InvalidName(name) => t"the name '$name' is reserved"
    case Reason.ParentOfRoot      => t"the root has no parent"
    case Reason.NotRooted         => t"the path is not rooted"

object SerpentineOpaques:
  opaque type PathName[ForbiddenType <: Label] = Text

  object PathName:
    def apply[ForbiddenType <: Label](text: Text): PathName[ForbiddenType] throws PathError = ???

  extension [ForbiddenType <: Label](pathName: PathName[ForbiddenType])
    def text: Text = pathName

export SerpentineOpaques.*

case class PathError(reason: PathError.Reason)
extends Error(err"the path is invalid because $reason")

@targetName("unixRoot")
def %(using Unix.type): Unix.AbsolutePath = Unix.AbsolutePath(Unix, Nil)

@targetName("relative")
def ?
    [RootType, NameType <: Label]
    (using hierarchy: Hierarchy[RootType, NameType])
    : hierarchy.RelativePath =
  hierarchy.RelativePath(0, Nil)

@targetName("relativeParent")
def ?^
    [RootType, NameType <: Label]
    (using hierarchy: Hierarchy[RootType, NameType])
    : hierarchy.RelativePath =
  hierarchy.RelativePath(1, Nil)

@targetName("relativeParent2")
def ?^^
    [RootType, NameType <: Label]
    (using hierarchy: Hierarchy[RootType, NameType])
    : hierarchy.RelativePath =
  hierarchy.RelativePath(2, Nil)

@targetName("relativeParent3")
def ?^^^
    [RootType, NameType <: Label]
    (using hierarchy: Hierarchy[RootType, NameType])
    : hierarchy.RelativePath =
  hierarchy.RelativePath(3, Nil)

case class System
    [RootType, NameType <: Label]
    (hierarchies: Hierarchy[RootType, NameType]*)

trait Hierarchy[RootType, NameType <: Label]:
  def parse(value: Text): Path = ???
  def rootText(root: RootType): Text
  def parseRoot(text: Text): (RootType, Text) throws PathError
  
  val pathSeparator: Text
  val parentRef: Text
  val selfRef: Text

  sealed trait Path:
    val nameSeq: List[PathName[NameType]]
    lazy val elements: IArray[Text] = IArray.from(nameSeq.map(_.text).reverse)
    def parent: Maybe[Path]

    @targetName("child")
    infix def /(name: PathName[NameType]): Path

  case class RelativePath(ascent: Int, nameSeq: List[PathName[NameType]]) extends Path:
    @targetName("child")
    infix def /(name: PathName[NameType]): RelativePath = RelativePath(ascent, name :: nameSeq)
    
    def parent: RelativePath =
      if nameSeq.isEmpty then RelativePath(ascent + 1, Nil) else RelativePath(ascent, nameSeq.tail)
    
    def text: Text =
      if nameSeq.isEmpty then
        if ascent == 0 then selfRef else t"${t"$parentRef$pathSeparator"*(ascent - 1)}$parentRef"
      else t"${t"$parentRef$pathSeparator"*ascent}${elements.join(pathSeparator)}"
    
    def keep(n: Int): RelativePath = RelativePath(ascent, nameSeq.takeRight(n))

  object AbsolutePath:
    def parse(text: Text): AbsolutePath throws PathError =
      val (root, rest) = parseRoot(text)
      AbsolutePath(root, rest.cut(pathSeparator).map(PathName[NameType](_)))

  case class AbsolutePath(root: RootType, nameSeq: List[PathName[NameType]]) extends Path:
    @targetName("child")
    infix def /(name: PathName[NameType]): AbsolutePath = AbsolutePath(root, name :: nameSeq)
    def text: Text = t"${rootText(root)}${elements.join(pathSeparator)}"
    
    def parent: Maybe[AbsolutePath] = ancestor(1)
    
    def ancestor(n: Int): Maybe[AbsolutePath] =
      if n > nameSeq.length then Unset else AbsolutePath(root, nameSeq.drop(n))
    
    def relative: RelativePath = RelativePath(0, nameSeq)
    
    def depth: Int = nameSeq.length
    def keep(n: Int): AbsolutePath = AbsolutePath(root, nameSeq.takeRight(n))
    
    def conjunction(right: AbsolutePath): AbsolutePath =
      @tailrec
      def count(n: Int): Int =
        if elements.length > n && right.elements.length > n && elements(n) == right.elements(n)
        then count(n + 1)
        else n
      
      keep(count(0))
    
    def relativeTo(path: AbsolutePath): RelativePath =
      val common = conjunction(path).depth
      RelativePath(depth - common, path.nameSeq.dropRight(common))
    
    def precedes(path: AbsolutePath): Boolean = conjunction(path).nameSeq == nameSeq

    @targetName("plus")
    def +(relative: RelativePath): AbsolutePath throws PathError =
      if relative.ascent > depth then throw PathError(PathError.Reason.ParentOfRoot)
      else AbsolutePath(root, relative.nameSeq ::: ancestor(relative.ascent).avow.nameSeq)


type UnixForbidden =
  ".*<.*" | ".*>.*" | ".*:.*" | ".*\".*" | ".*\\\\.*" | ".*\\|.*" | ".*\\?.*" | ".*\\*.*" | ".*/.*"

type WindowsForbidden =
  "con(\\..*)?" | "prn(\\..*)?" | "aux(\\..*)?" | "nul(\\..*)?" | "com1(\\..*)?" | "com2(\\..*)?" |
      "com3(\\..*)?" | "com4(\\..*)?" | "com5(\\..*)?" | "com6(\\..*)?" | "com7(\\..*)?" |
      "com8(\\..*)?" | "com9(\\..*)?" | "lpt1(\\..*)?" | "lpt2(\\..*)?" | "lpt3(\\..*)?" |
      "lpt4(\\..*)?" | "lpt5(\\..*)?" | "lpt6(\\..*)?" | "lpt7(\\..*)?" | "lpt8(\\..*)?" |
      "lpt9(\\..*)?" | ".* " | ".*\\."

object Unix extends Hierarchy[Unix.type, UnixForbidden]:
  def apply(): AbsolutePath = AbsolutePath(Unix, Nil)

  val pathSeparator: Text = t"/"
  val parentRef: Text = t".."
  val selfRef: Text = t"."

  def rootText(root: Unix.type): Text = t"/"
  def parseRoot(text: Text): (Unix.type, Text) throws PathError =
    if text.starts(t"/") then (Unix, text.drop(1)) else throw PathError(PathError.Reason.NotRooted)

export Windows.Drive

object Windows extends Hierarchy[Windows.Drive, WindowsForbidden]:
  val pathSeparator: Text = t"\\"
  val parentRef: Text = t".."
  val selfRef: Text = t"."
  
  def rootText(drive: Drive): Text = t"${drive.letter}:\\"
  
  def parseRoot(text: Text): (Drive, Text) throws PathError = text match
    case r"$letter([A-Za-z]):\\.*" => (Drive(unsafely(letter(0))), text.drop(3))
    case _                         => throw PathError(PathError.Reason.NotRooted)

  case class Drive(letter: Char):
    def apply(): AbsolutePath = AbsolutePath(this, Nil)

    @targetName("child")
    infix def /(name: PathName[WindowsForbidden]): AbsolutePath = AbsolutePath(this, List(name))

object SerpentineMacros:
  def parse
      [ForbiddenType <: Label: Type](context: Expr[StringContext])(using Quotes)
      : Expr[PathName[ForbiddenType]] =
    import quotes.reflect.*
    
    val (element: String, pos: Position) = context match
      case '{StringContext(${Varargs(Seq(str))}*)} => (str.value.get, str.asTerm.pos)
      case _                                       => fail("A StringContext should contain literals")
    
    def checkType(repr: TypeRepr): Unit = repr.dealias.asMatchable match
      case OrType(left, right) =>
        checkType(left)
        checkType(right)
      
      case ConstantType(StringConstant(pattern)) =>
        if element.matches(pattern) then
          Text(pattern) match
            case r"\.\*\\?$char(.)\.\*" =>
              fail(s"a path element may not contain the character '$char'", pos)
            
            case r"$start([a-zA-Z0-9]*)\.\*" =>
              fail(s"a path element may not start with '$start'", pos)
            
            case r"[a-zA-Z0-9]*" =>
              fail(s"a path element may not be '$pattern'", pos)
            
            case other =>
              fail(s"a path element may not match the pattern '$other'")

      case other =>
        fail(s"Unexpectedly found type $other")
    
    checkType(TypeRepr.of[ForbiddenType])

    '{${Expr(element)}.asInstanceOf[PathName[ForbiddenType]]}

extension (inline context: StringContext)
  inline def p[ForbiddenType <: Label](): PathName[ForbiddenType] =
    ${SerpentineMacros.parse[ForbiddenType]('context)}