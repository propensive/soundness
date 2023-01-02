/*
    Serpentine, version 0.4.0. Copyright 2021-23 Jon Pretty, Propensive OÜ.

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
import gossamer.*
import anticipation.*

case class RootParentError(root: Root) extends Error(err"attempted to access parent of root $root")
case class InvalidPathError(path: Text) extends Error(err"the path $path was not absolute")

trait Root(val prefix: Text, val separator: Text):
  type PathType <: Absolute[this.type]

  def apply(path: GenericPath): PathType = make(path.parts)

  @targetName("child")
  infix def /(element: Text): PathType throws InvalidPathError =
    make(List(PathElement(element).value))
  
  @targetName("safeChild")
  infix def /(element: PathElement): PathType = make(List(element.value))

  def make(elements: List[Text]): PathType

  def parse(text: Text): PathType throws InvalidPathError =
    if !text.starts(prefix) then throw InvalidPathError(text)
    else make(text.drop(prefix.length).cut(separator))
  
object Root:
  
  given rootAttributeWriter: GenericHtmlAttribute["href", Root.^.type] with
    def name: String = "href"
    def serialize(value: Root.^.type): String = "/"

  @targetName("RelativeRoot")
  object ? extends Relative(0, Nil)
  
  @targetName("GenericRoot")
  object ^ extends Root(t"/", t"/"):
    type PathType = Absolute[^.type]
    def make(elements: List[Text]): PathType = Absolute(^, elements)

trait GetRoot[T]:
  def apply(value: Iterable[T]): Root

export Root.{?, ^}
val GenericPath: ^.type = ^
type GenericPath = Absolute[^.type]

object Relative:
  object Self extends Relative(0, Nil)
  
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

object Slash:
  @targetName("Extractor")
  object `/`:
    def unapply[R <: Root](abs: Absolute[R]): Option[(R | Absolute[R], Text)] =
      for left <- abs.init.option; right <- abs.last.option
      yield (if left.parts.isEmpty then left.root else left, right)

export Slash.`/`

object Absolute:
  given [R <: Root]: Show[Absolute[R]] = _.text

  given GenericHtmlAttribute["href", GenericPath] with
    def name = "href"
    def serialize(value: GenericPath): String = value.show.s

open class Absolute[+R <: Root](val root: R, val parts: List[Text]):
  def depth: Int = parts.length
  def init: Maybe[Absolute[R]] = safely(root.make(parts.init))
  def head: Maybe[Absolute[R]] = safely(root.make(List(parts.head)))
  def last: Maybe[Text] = safely(parts.last)
  
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

  def generic: GenericPath = Absolute(^, parts)

  def text: Text = parts.join(root.prefix, root.separator, t"")

  @targetName("safeChild")
  infix def /(element: PathElement): root.PathType = root.make(parts :+ element.value)

  @targetName("child")
  infix def /(value: Text): root.PathType throws InvalidPathError =
    /(PathElement(value))

  @targetName("add") 
  infix def +(relative: Relative): root.PathType throws RootParentError =
    if relative.ascent > parts.length then throw RootParentError(root)
    else root.make(parts.dropRight(relative.ascent) ++ relative.parts)
