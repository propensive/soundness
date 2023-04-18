package serpentine2

import rudiments.*
import gossamer.*
import deviation.*

import scala.quoted.*
import scala.compiletime.*

import language.experimental.captureChecking

type ForbiddenSet = Singleton & (Char | String)

object SerpentineOpaques:
  opaque type PathElement[ForbiddenType <: ForbiddenSet] = String

  object PathElement:
    inline def apply[ForbiddenType <: ForbiddenSet](value: Text): PathElement[ForbiddenType] =
      value.s

  given [ForbiddenType <: ForbiddenSet]: Show[PathElement[ForbiddenType]] = Text(_)

export SerpentineOpaques.PathElement

object PathError:
  enum Reason:
    case InvalidChar(char: Char)
    case InvalidName(name: Text)
    case ParentOfRoot

  given Show[Reason] =
    case Reason.InvalidChar(char) => t"the character '$char' cannot appear in a path"
    case Reason.InvalidName(name) => t"the name '$name' is reserved"
    case Reason.ParentOfRoot      => t"the root has no parent"

case class PathError(reason: PathError.Reason) extends Error(err"the path is invalid because $reason")

trait Pathlike[-PathType]:
  type ForbiddenType <: Singleton & (Char | String)
  type RootType
  type ChildType
  def separator: Text
  def prefix(root: RootType): Text
  def root(path: PathType): RootType
  def elements(path: PathType): List[PathElement[ForbiddenType]]
  def child(base: PathType, element: PathElement[ForbiddenType]): ChildType
  def parent(path: PathType): ChildType

object PathBounds:
  given [PathType, RootType](using pathError: CanThrow[PathError]): PathBounds[PathType] = isRoot =>
    if isRoot then throw PathError(PathError.Reason.ParentOfRoot)

@capability
trait PathBounds[PathType]:
  def hasParent(isRoot: Boolean): Unit

extension [PathType](path: PathType)(using pathlike: Pathlike[PathType])
  def elements: List[PathElement[pathlike.ForbiddenType]] = pathlike.elements(path)
  def root: pathlike.RootType = pathlike.root(path)
  
  def parent(using pathBounds: PathBounds[PathType]): {pathBounds} pathlike.ChildType =
    pathBounds.hasParent(path.elements.isEmpty)
    pathlike.parent(path)
  
  def text: Text = pathlike.elements(path).reverse.map(_.show).join(pathlike.prefix(root), pathlike.separator, t"")

  def depth: Int = pathlike.elements(path).length

extension [PathType](path: PathType)(using pathlike: Pathlike[PathType])
  @targetName("child")
  infix def /(element: PathElement[pathlike.ForbiddenType]): pathlike.ChildType = pathlike.child(path, element)

object SerpentineExports:
  @targetName("RelativeRoot")
  object `?` extends Relative(0, Nil)
  
  @targetName("GeneralPathRoot")
  object `^` extends GenericPath(Nil)

export SerpentineExports.{`?`, `^`}

object Relative:
  given [RelativeType <: Relative]: PathBounds[RelativeType] = isRoot => ()
  
  given Show[Relative] = rel =>
    if rel == ? then t"."
    else (List.fill(rel.ascent)(t"..") ::: rel.elements.reverse.map(_.show)).join(t"", t"/", t"")

  given pathlike: Pathlike[Relative] with
    type RootType = `?`.type
    type ChildType = Relative
    type ForbiddenType = '/' | ".." | ""
    
    def separator: Text = t"/"
    def prefix(root: `?`.type): Text = t"./"
    def root(path: Relative): `?`.type = ?
    def elements(path: Relative): List[PathElement[ForbiddenType]] = path.elements
    
    def child(base: Relative, element: PathElement[ForbiddenType]): Relative =
      base.copy(elements = element :: base.elements)
    
    def parent(path: Relative): Relative =
      if path.elements.isEmpty then Relative(path.ascent + 1, Nil)
      else Relative(path.ascent, path.elements.tail)

  def parse(text: Text): Relative throws PathError =
    def recur(text: Text, ascent: Int): Relative =
      if text == t"." then ?
      else if text == t".." then Relative(ascent + 1, Nil)
      else if text.starts(t"../") then recur(text.drop(3), ascent + 1)
      else Relative(ascent, List(text.cut(t"/").filter(_ != t"").map(PathElement(_))*).reverse)
    
    recur(text, 0)

case class Relative(ascent: Int, elements: List[PathElement['/' | ".." | ""]])

object GenericPath:
  given pathlike: Pathlike[GenericPath] with
    type RootType = `^`.type
    type ChildType = GenericPath
    type ForbiddenType = '/' | ".." | ""

    def separator: Text = t"/"
    def prefix(root: `^`.type): Text = t"/"
    def root(path: GenericPath): `^`.type = ^
    def elements(path: GenericPath): List[PathElement['/' | ".." | ""]] = path.elements
    def child(base: GenericPath, child: PathElement['/' | ".." | ""]): GenericPath =
      base.copy(elements = child :: base.elements)
    
    def parent(path: GenericPath): GenericPath = path.copy(elements = path.elements.tail)


case class GenericPath(elements: List[PathElement['/' | ".." | ""]])

object SerpentineMacros:
  def parse
      [ForbiddenType <: ForbiddenSet: Type](ctx: Expr[StringContext])(using Quotes)
      : Expr[PathElement[ForbiddenType]] =
    import quotes.reflect.*
    
    val (element: String, pos: Position) = ctx match
      case '{StringContext(${Varargs(Seq(str))}*)} => (str.value.get, str.asTerm.pos)
      case _                                       => fail("A StringContext should contain literals")
    
    def checkType(repr: TypeRepr): Unit = repr.dealias.asMatchable match
      case OrType(left, right) =>
        checkType(left)
        checkType(right)
      
      case ConstantType(StringConstant(str)) =>
        if element == str then fail(s"'$str' is not a valid name for a path element", pos)

      case ConstantType(CharConstant(char)) =>
        element.indexOf(char) match
          case -1  => ()
          case idx =>
            val pos2 = Position(pos.sourceFile, pos.start + idx, pos.start + idx + 1)
            fail(s"the character '$char' is not permitted in a path element", pos2)
    
      case other =>
        fail(s"Unexpectedly found type $other")
    
    checkType(TypeRepr.of[ForbiddenType])

    '{${Expr(element)}.asInstanceOf[PathElement[ForbiddenType]]}

extension (inline ctx: StringContext)
  inline def p[ForbiddenType <: ForbiddenSet](): PathElement[ForbiddenType] =
    ${SerpentineMacros.parse[ForbiddenType]('ctx)}
