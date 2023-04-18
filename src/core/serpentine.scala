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

package serpentine

import rudiments.*
import deviation.*
import gossamer.*
import anticipation.*

case class RootParentError(root: Root) extends Error(err"attempted to access the parent of root $root")
case class PathError(path: Text) extends Error(err"the path $path was not valid")

trait Root(val prefix: Text, val separator: Text):
  type PathType <: Absolute

  def apply(path: GenericPath): PathType = make(path.parts)

  @targetName("child")
  infix def /(element: Text): PathType throws PathError =
    make(List(PathElement(element).value))
  
  @targetName("safeChild")
  infix def /(element: PathElement): PathType = make(List(element.value))

  def make(elements: List[Text]): PathType

  def parse(text: Text): PathType throws PathError =
    if !text.starts(prefix) then throw PathError(text)
    else make(text.drop(prefix.length).cut(separator))
  
object Root extends Root(t"/", t"/"):
  type PathType = GenericPath
  
  def make(elements: List[Text]): GenericPath = new GenericPath(elements)
  
  given rootAttributeWriter: GenericHtmlAttribute["href", GenericRoots.^.type] with
    def name: String = "href"
    def serialize(value: GenericRoots.^.type): String = "/"

object GenericRoots:
  @targetName("RelativeRoot")
  object ? extends Relative(0, Nil)
  
  @targetName("GenericRoot")
  object ^ extends Root(t"/", t"/"):
    type PathType = GenericPath
    def make(elements: List[Text]): PathType = new GenericPath(elements)

trait GetRoot[T]:
  def apply(value: Iterable[T]): Root

export GenericRoots.{?, ^}
class GenericPath(parts: List[Text]) extends Absolute(parts):
  type RootType = ^.type
  val root: ^.type = ^

object Relative:
  object Self extends Relative(0, Nil)
  
  given Canonical[Relative] = Canonical(parse(_), _.show)

  given Show[Relative] =
    case Self                    => t"."
    case Relative(ascent, parts) => parts.join(t"../"*ascent, t"/", t"")

  given GenericHtmlAttribute["href", Relative] with
    def name: String = "href"
    def serialize(value: Relative): String = value.show.s
  
  def parse(text: Text): Relative =
    def recur(text: Text, ascent: Int): Relative =
      if text == t"." then Self
      else if text == t".." then Relative(ascent + 1, Nil)
      else if text.starts(t"../") then recur(text.drop(3), ascent + 1)
      else Relative(ascent, List(text.cut(t"/").filter(_ != t"")*))
    
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

object Slash:
  @targetName("Extractor")
  object `/`:
    def unapply(abs: Absolute): Option[(Root | Absolute, Text)] =
      for left <- abs.init.option; right <- abs.last.option
      yield (if left.parts.isEmpty then left.root else left, right)

export Slash.`/`

object Absolute:
  given [R <: Root]: Show[Absolute] = _.text
  given GenericHttpRequestParam["location", GenericPath] = _.show.s

  given GenericHtmlAttribute["href", GenericPath] with
    def name = "href"
    def serialize(value: GenericPath): String = value.show.s

abstract class Absolute(val parts: List[Text]):
  type RootType <: Root
  val root: RootType
  def depth: Int = parts.length
  def init: Maybe[Absolute] = safely(root.make(parts.init))
  def head: Maybe[Absolute] = safely(root.make(List(parts.head)))
  def last: Maybe[Text] = safely(parts.last)
  
  def parent: root.PathType throws RootParentError = ancestor(1)

  def ancestor(ascent: Int): root.PathType throws RootParentError =
    if ascent < 0 then throw RootParentError(root)
    else if ascent == 0 then root.make(parts)
    else if ascent > depth then throw RootParentError(root)
    else root.make(parts.dropRight(ascent))

  def conjunction(other: root.PathType): root.PathType & Absolute =
    root.make(parts.zip(other.parts).takeWhile(_ == _).map(_(0)))
  
  def relativeTo(other: root.PathType): Relative =
    val common = conjunction(other).parts.length
    Relative(other.parts.length - common, parts.drop(common))

  def relative: Relative = Relative(0, parts)

  def precedes(other: root.PathType): Boolean =
    conjunction(other).parts == parts

  def generic: GenericPath = new GenericPath(parts)

  def text: Text = parts.join(root.prefix, root.separator, t"")

  @targetName("safeChild")
  infix def /(element: PathElement): root.PathType = root.make(parts :+ element.value)

  @targetName("child")
  infix def /(value: Text): root.PathType throws PathError =
    /(PathElement(value))

  @targetName("add") 
  infix def +(relative: Relative): root.PathType throws RootParentError =
    if relative.ascent > parts.length then throw RootParentError(root)
    else root.make(parts.dropRight(relative.ascent) ++ relative.parts)
