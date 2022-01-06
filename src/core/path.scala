/*
    Slalom, version 0.6.0. Copyright 2021-21 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apche License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package slalom

import rudiments.*
import gossamer.*

import annotation.targetName

case class RootParentError(root: Root) extends Error:
  def message: Text = txt"""an attempt was made to access the parent of a filesystem root, which (by
                          definition) has no parent"""

object Relative:
  given Show[Relative] = relative => relative.parts.join(t"../"*relative.ascent, t"/", t"")

  def parse(text: Text): Relative =
    def recur(text: Text, ascent: Int): Relative =
      if text.startsWith(t"../") then recur(text.drop(3), ascent + 1)
      else Relative(ascent, List(text.cut(t"/")*))
    
    recur(text, 0)

case class Relative(val ascent: Int, val parts: List[Text]):
  def parent: Relative throws RootParentError =
    if parts.isEmpty then Relative(ascent + 1, List()) else Relative(ascent, parts.init)
  
  def ancestor(n: Int): Relative throws RootParentError =
    if n == 0 then Relative(ascent, parts) else parent.ancestor(n - 1)
  
  // def absolute(pwd: AbsolutePath): AbsolutePath throws RootParentError =
  //   if ascent == 0 then makeAbsolute(pwd.parts ++ parts)
  //   else Relative(ascent - 1, parts).absolute(pwd.parent)
  
  @targetName("access")
  infix def /(filename: Text): Relative throws RootParentError = filename match
    case t".." => if parts.isEmpty then Relative(ascent + 1, List())
                  else Relative(ascent, parts.init)
    case t"."  => Relative(ascent, parts)
    case _     => Relative(ascent, parts :+ filename)
  
  @targetName("addAll")
  infix def ++(relative: Relative): Relative throws RootParentError =
    if relative.ascent == 0 then Relative(ascent, parts ++ relative.parts)
    else ancestor(relative.ascent) ++ Relative(0, relative.parts)

  override def equals(that: Any): Boolean = that.unsafeMatchable match
    case that: Relative => ascent == that.ascent && parts == that.parts
    case _              => false

  override def hashCode: Int = parts.hashCode ^ ascent

trait Root(val separator: Text, val prefix: Text):
  def thisRoot: this.type = this
  type AbsolutePath <: Path.Absolute

  protected def makeAbsolute(parts: List[Text]): AbsolutePath

  trait Path:
    def root: thisRoot.type = thisRoot
    def parent: Path throws RootParentError
    def ancestor(ascent: Int): Path throws RootParentError
    
    @targetName("access")
    infix def /(filename: Text): Path throws RootParentError
    
    @targetName("addAll")
    infix def ++(relative: Relative): Path throws RootParentError

  object Path:
    object Absolute:
      given Show[Absolute] = path => path.parts.join(prefix, separator, t"")

    open class Absolute(val parts: List[Text]) extends Path:
      def parent: AbsolutePath throws RootParentError =
        if parts.isEmpty then throw RootParentError(root) else makeAbsolute(parts.init)

      def ancestor(ascent: Int): AbsolutePath throws RootParentError =
        if ascent == 0 then makeAbsolute(parts)
        else if parts.isEmpty then throw RootParentError(root)
        else parent.ancestor(ascent - 1)
      
      def conjunction(other: AbsolutePath): AbsolutePath =
        makeAbsolute(parts.zip(other.parts).takeWhile(_ == _).map(_(0)))

      def relativeTo(base: AbsolutePath): Relative =
        val common = conjunction(base)
        Relative(base.parts.length - common.parts.length, parts.drop(common.parts.length))


      @targetName("access")
      infix def /(filename: Text): AbsolutePath throws RootParentError = filename match
        case t".." => if parts.isEmpty then throw RootParentError(root) else makeAbsolute(parts.init)
        case t"."  => makeAbsolute(parts)
        case _     => makeAbsolute(parts :+ filename)
      
      @targetName("addAll")
      infix def ++(relative: Relative): AbsolutePath throws RootParentError =
        if relative.ascent > 0 then ancestor(relative.ascent) ++ Relative(0, relative.parts)
        else makeAbsolute(parts ++ relative.parts)
      
      override def equals(that: Any): Boolean = that.unsafeMatchable match
        case that: Absolute => parts == that.parts
        case _              => false

      override def hashCode: Int = parts.hashCode

      def rename(change: Text => Text): AbsolutePath = makeAbsolute(parts.init :+ change(parts.last))

object Base extends Root(t"/", t"/"):
  type AbsolutePath = Path.Absolute
  
  protected def makeAbsolute(parts: List[Text]): AbsolutePath = Path.Absolute(parts)