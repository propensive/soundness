/*
    Serpentine, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

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

object Hierarchy:
  given show[PathType](using hierarchy: Hierarchy[PathType]): Show[PathType] = path =>
    t"${hierarchy.prefix(path)}${path.elements.map(_.show).reverse.join(hierarchy.separator)}"

trait Hierarchy[PathType]:

  type ForbiddenType <: Singleton & (Char | String)
  def separator: Text
  def prefix(root: PathType): Text
  def root(path: PathType): PathType
  def elements(path: PathType): List[PathElement[ForbiddenType]]
  def child(base: PathType, element: PathElement[ForbiddenType]): PathType
  def parent(path: PathType): PathType

object PathBounds:
  given [PathType](using pathError: CanThrow[PathError]): PathBounds[PathType] = isRoot =>
    if isRoot then throw PathError(PathError.Reason.ParentOfRoot)

@capability
trait PathBounds[-PathType]:
  def hasParent(isRoot: Boolean): Unit

extension [PathType, PathType2 >: PathType](path: PathType)(using hierarchy: Hierarchy[PathType2])
  def elements: List[PathElement[hierarchy.ForbiddenType]] = hierarchy.elements(path)
  def root: PathType2 = hierarchy.root(path)
  
  def parent(using pathBounds: PathBounds[PathType]): {pathBounds} PathType2 =
    pathBounds.hasParent(path.elements.isEmpty)
    hierarchy.parent(path)
  
  def text: Text =
    hierarchy.elements(path).reverse.map(_.show).join(hierarchy.prefix(root), hierarchy.separator, t"")

  def depth: Int = hierarchy.elements(path).length
  
  @targetName("child")
  infix def /(element: PathElement[hierarchy.ForbiddenType]): PathType2 = hierarchy.child(path, element)
  
  def ancestor(n: Int): PathType2 =
    def recur(path: PathType2, n: Int): PathType2 = if n == 0 then path else recur(hierarchy.parent(path), n - 1)
    if depth > n then recur(path, n) else hierarchy.root(path)
  
  def take(n: Int): PathType2 = ancestor(depth - n)
  
  def conjunction(other: PathType2): PathType2 =
    take(elements.reverse.zip(other.elements.reverse).takeWhile(_ == _).length)

  def relativeTo(other: PathType2): Relative =
    val common: Int = conjunction(other).depth
    Relative(depth - common, other.elements.reverse.drop(common).map(_.show).reverse)
  
  def precedes(other: PathType): Boolean = conjunction(other).elements == elements

  @targetName("add") 
  infix def ++
      (relative: Relative)
      (using pathError: CanThrow[PathError], pathBounds: PathBounds[PathType])
      : {pathBounds, pathError} PathType2 =
    if relative.ascent > depth then throw PathError(PathError.Reason.ParentOfRoot) else
      def recur(path: PathType2, elements: List[Text]): PathType2 = elements match
        case head :: tail => recur(hierarchy.child(path, PathElement(head)), tail)
        case Nil          => path
      
      recur(ancestor(relative.ascent), relative.elements.reverse)

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

  given hierarchy: Hierarchy[Relative] with
    type ForbiddenType = '/' | ".." | ""
    
    def separator: Text = t"/"
    def prefix(root: Relative): Text = t"./"
    def root(path: Relative): Relative = ?
    def elements(path: Relative): List[PathElement[ForbiddenType]] = path.elements.map(PathElement(_))
    
    def child(base: Relative, element: PathElement[ForbiddenType]): Relative =
      base.copy(elements = element.show :: base.elements)
    
    def parent(path: Relative): Relative =
      if path.elements.isEmpty then Relative(path.ascent + 1, Nil)
      else Relative(path.ascent, path.elements.tail)

  def parse(text: Text): Relative throws PathError =
    def recur(text: Text, ascent: Int): Relative =
      if text == t"." then ?
      else if text == t".." then Relative(ascent + 1, Nil)
      else if text.starts(t"../") then recur(text.drop(3), ascent + 1)
      else Relative(ascent, List(text.cut(t"/").filter(_ != t"")*).reverse)
    
    recur(text, 0)

case class Relative(ascent: Int, elements: List[Text])

object GenericPath:
  given Show[GenericPath] = Hierarchy.show[GenericPath]

  given hierarchy: Hierarchy[GenericPath] with
    type ForbiddenType = '/' | ".." | ""

    def separator: Text = t"/"
    def prefix(root: GenericPath): Text = t"/"
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
