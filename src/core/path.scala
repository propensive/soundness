/*
    Slalom, version 0.6.0. Copyright 2021-21 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
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

class SlalomException(message: Text) extends Exception(t"slalom: $message".s)

case class RootBoundaryExceeded(root: Root)
extends SlalomException(t"attempted to exceed the root boundary")

transparent trait GenericRelative:
  def ascent: Int
  def path: Vector[Text]

trait Root(val separator: Text, val prefix: Text):
  def thisRoot: this.type = this

  type AbsolutePath <: Path.Absolute
  type RelativePath <: Path.Relative

  protected def makeAbsolute(path: Vector[Text]): AbsolutePath
  protected def makeRelative(ascent: Int, path: Vector[Text]): RelativePath

  trait Path:
    def root: thisRoot.type = thisRoot
    def parent: Path throws RootBoundaryExceeded
    def ancestor(ascent: Int): Path throws RootBoundaryExceeded
    def absolute(pwd: AbsolutePath): AbsolutePath throws RootBoundaryExceeded
    
    @targetName("access")
    infix def /(filename: Text): Path throws RootBoundaryExceeded
    
    @targetName("addAll")
    infix def ++(path: GenericRelative): Path throws RootBoundaryExceeded

  object Path:
    open class Absolute(val path: Vector[Text]) extends Path:
      override def toString(): String = path.join(prefix, separator, t"").s
      
      def parent: AbsolutePath throws RootBoundaryExceeded = path match
        case init :+ last => makeAbsolute(init)
        case _            => throw RootBoundaryExceeded(root)

      def ancestor(ascent: Int): AbsolutePath throws RootBoundaryExceeded =
        if path.length == 0 then makeAbsolute(path) else parent.ancestor(ascent - 1)
      
      def absolute(pwd: AbsolutePath): AbsolutePath throws RootBoundaryExceeded =
        makeAbsolute(path)

      def conjunction(other: AbsolutePath): AbsolutePath =
        makeAbsolute(path.zip(other.path).takeWhile(_ == _).map(_(0)))

      def relativeTo(base: AbsolutePath): RelativePath =
        val common = conjunction(base)
        makeRelative(path.length - common.path.length, base.path.drop(common.path.length))

      @targetName("access")
      infix def /(filename: Text): AbsolutePath throws RootBoundaryExceeded = filename.s match
        case ".."     => path match
          case init :+ last => makeAbsolute(init)
          case _            => throw RootBoundaryExceeded(root)
        case "."      => makeAbsolute(path)
        case fn       => makeAbsolute(path :+ filename)
      
      @targetName("addAll")
      infix def ++(relative: GenericRelative): AbsolutePath throws RootBoundaryExceeded =
        if relative.ascent > 0 then ancestor(relative.ascent) ++ makeRelative(0, relative.path)
        else makeAbsolute(path ++ relative.path)
      
      override def equals(that: Any): Boolean = that.unsafeMatchable match
        case that: Absolute => path == that.path
        case _              => false

      override def hashCode: Int = path.hashCode

    case class Relative(val ascent: Int, val path: Vector[Text]) extends Path, GenericRelative:
      override def toString(): String = path.join(t"../"*ascent, t"/", t"").s

      def parent: Relative throws RootBoundaryExceeded = path match
        case init :+ last => makeRelative(ascent, init)
        case empty        => makeRelative(ascent + 1, Vector())

      def ancestor(ascent: Int): RelativePath throws RootBoundaryExceeded =
        if ascent == 0 then makeRelative(ascent, path) else parent.ancestor(ascent - 1)

      def absolute(pwd: AbsolutePath): AbsolutePath throws RootBoundaryExceeded =
        if ascent == 0 then makeAbsolute(pwd.path ++ path)
        else makeRelative(ascent - 1, path).absolute(pwd.parent)
      
      @targetName("access")
      infix def /(filename: Text): RelativePath throws RootBoundaryExceeded = filename.s match
        case ".."     => path match
          case init :+ last => makeRelative(ascent, init)
          case empty        => makeRelative(ascent + 1, Vector())
        case "."      => makeRelative(ascent, path)
        case fn       => makeRelative(ascent, path :+ filename)
      
      @targetName("addAll")
      infix def ++(relative: GenericRelative): RelativePath throws RootBoundaryExceeded =
        if relative.ascent == 0 then makeRelative(ascent, path ++ relative.path)
        else ancestor(relative.ascent) ++ makeRelative(ascent, relative.path)

      override def equals(that: Any): Boolean = that.unsafeMatchable match
        case that: Relative => ascent == that.ascent && path == that.path
        case _              => false

      override def hashCode: Int = path.hashCode ^ ascent

object Base extends Root(t"/", t"/"):
  type RelativePath = Path.Relative
  type AbsolutePath = Path.Absolute
  
  protected def makeAbsolute(path: Vector[Text]): AbsolutePath = Path.Absolute(path)
  
  protected def makeRelative(ascent: Int, path: Vector[Text]): RelativePath =
    Path.Relative(ascent, path)