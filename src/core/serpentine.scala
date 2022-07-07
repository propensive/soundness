package serpentine

import rudiments.*
import gossamer.*

case class RootParentError(root: Root) extends Error((t"attempted to access parent of root ", root))
case class InvalidPathError(path: Text) extends Error((t"the path ", path, " was not absolute"))

trait Root(val prefix: Text, val separator: Text):
  type PathType <: Absolute[this.type]

  @targetName("child")
  infix def /(element: Text): PathType throws InvalidPathError =
    make(List(PathElement(element).value))
  
  @targetName("safeChild")
  infix def /(element: PathElement): PathType = make(List(element.value))

  def make(elements: List[Text]): PathType
  
object Root:
  @targetName("RelativeRoot")
  object ? extends Relative(0, Nil)
  
  @targetName("GenericRoot")
  object ^ extends Root(t"/", t"/"):
    type PathType = Absolute[^.type]
    def make(elements: List[Text]): PathType = Absolute(^, elements)

export Root.{?, ^}

object Relative:
  object Self extends Relative(0, Nil)
  
  given Show[Relative] =
    case Self                    => t"."
    case Relative(ascent, parts) => parts.join(t"../"*ascent, t"/", t"")

  given anticipation.HtmlAttribute["href", Relative] with
    def name = "href"
    def serialize(value: Relative): String = value.show.s

  def parse(text: Text): Relative =
    def recur(text: Text, ascent: Int): Relative =
      if text == t"." then Self
      else if text == t".." then Relative(ascent + 1, Nil)
      else if text.startsWith(t"../") then recur(text.drop(3), ascent + 1)
      else Relative(ascent, List(text.cut(t"/")*))
    
    recur(text, 0)

case class Relative(ascent: Int, parts: List[Text]):
  def parent: Relative =
    if parts.isEmpty then Relative(ascent + 1, Nil) else Relative(ascent, parts.init)
  
  def ancestor(n: Int): Relative = if n == 0 then this else parent.ancestor(n - 1)
  
  @targetName("child")
  infix def /(filename: Text): Relative = filename match
    case t".." => if parts.isEmpty then Relative(ascent + 1, Nil) else Relative(ascent, parts.init)
    case t"."  => Relative(ascent, parts)
    case _     => Relative(ascent, parts :+ filename)
  
  @targetName("safeChild")
  infix def /(element: PathElement): Relative = Relative(ascent, parts :+ element.value)

  @targetName("add")
  infix def +(relative: Relative): Relative =
    if relative.ascent == 0 then Relative(ascent, parts ++ relative.parts)
    else ancestor(relative.ascent) + Relative(0, relative.parts)

  override def equals(that: Any): Boolean = that.matchable(using Unsafe) match
    case that: Relative => ascent == that.ascent && parts == that.parts
    case _              => false

  override def hashCode: Int = parts.hashCode ^ ascent

object Absolute:
  given [R <: Root & Singleton]: Show[Absolute[R]] = path =>
    path.parts.join(path.root.prefix, path.root.separator, t"")

open class Absolute[+R <: Root](val root: R, val parts: List[Text]):
  def depth: Int = parts.length
  def parent: root.PathType throws RootParentError = ancestor(1)

  def ancestor(ascent: Int): root.PathType throws RootParentError =
    if ascent < 0 then throw RootParentError(root)
    else if ascent == 0 then root.make(parts)
    else if ascent > depth then throw RootParentError(root)
    else root.make(parts.dropRight(ascent))

  def conjunction(other: root.PathType): root.PathType & Absolute[R] =
    root.make(parts.zip(other.parts).takeWhile(_ == _).map(_(0)))
  
  def relativeTo(other: root.PathType): Relative =
    val common = conjunction(other).parts.length
    Relative(other.parts.length - common, parts.drop(common))

  def precedes(other: root.PathType): Boolean =
    conjunction(other).parts == parts

  @targetName("safeChild")
  infix def /(element: PathElement): root.PathType = root.make(parts :+ element.value)

  @targetName("child")
  infix def /(value: Text): root.PathType throws InvalidPathError =
    /(PathElement(value))

  @targetName("add") 
  infix def +(relative: Relative): root.PathType throws RootParentError =
    if relative.ascent > parts.length then throw RootParentError(root)
    else root.make(parts.dropRight(relative.ascent) ++ relative.parts)
  