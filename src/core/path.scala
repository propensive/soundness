/*
    Slalom, version 0.1.0. Copyright 2020-21 Jon Pretty, Propensive OÜ.

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

class SlalomException(message: String) extends Exception(str"slalom: $message")

case class RootBoundaryExceeded(root: Root)
extends SlalomException(str"attempted to exceed the root boundary")

transparent trait GenericRelative:
  def ascent: Int
  def path: Vector[String]

trait Root(val separator: String, val prefix: String):
  def thisRoot: this.type = this

  type AbsolutePath <: Path.Absolute
  type RelativePath <: Path.Relative

  protected def makeAbsolute(path: Vector[String]): AbsolutePath
  protected def makeRelative(ascent: Int, path: Vector[String]): RelativePath

  trait Path:
    def root: thisRoot.type = thisRoot
    def parent: Path exposes RootBoundaryExceeded
    def ancestor(ascent: Int): Path exposes RootBoundaryExceeded
    def absolute(pwd: AbsolutePath): AbsolutePath exposes RootBoundaryExceeded
    infix def /(filename: String): Path exposes RootBoundaryExceeded
    def ++(path: GenericRelative): Path exposes RootBoundaryExceeded

  object Path:
    open class Absolute(val path: Vector[String]) extends Path:
      override def toString(): String = path.join(prefix, separator, "")
      
      def parent: AbsolutePath exposes RootBoundaryExceeded = path match
        case init :+ last => makeAbsolute(init)
        case _            => throw RootBoundaryExceeded(root)

      def ancestor(ascent: Int): AbsolutePath exposes RootBoundaryExceeded =
        if path.length == 0 then makeAbsolute(path) else parent.ancestor(ascent - 1)
      
      def absolute(pwd: AbsolutePath): AbsolutePath exposes RootBoundaryExceeded =
        makeAbsolute(path)

      def conjunction(other: AbsolutePath): AbsolutePath =
        makeAbsolute(path.zip(other.path).takeWhile(_ == _).map(_(0)))

      def relativeTo(base: AbsolutePath): RelativePath =
        val common = conjunction(base)
        makeRelative(path.length - common.path.length, base.path.drop(common.path.length))

      def /(filename: String): AbsolutePath exposes RootBoundaryExceeded = filename match
        case ".."     => path match
          case init :+ last => makeAbsolute(init)
          case _            => throw RootBoundaryExceeded(root)
        case "."      => makeAbsolute(path)
        case filename => makeAbsolute(path :+ filename)
      
      def ++(relative: GenericRelative): AbsolutePath exposes RootBoundaryExceeded =
        if relative.ascent > 0 then ancestor(relative.ascent) ++ makeRelative(0, relative.path)
        else makeAbsolute(path ++ relative.path)
      
      override def equals(that: Any): Boolean = that.unsafeMatchable match
        case that: Absolute => path == that.path
        case _              => false

      override def hashCode: Int = path.hashCode

    case class Relative(val ascent: Int, val path: Vector[String]) extends Path, GenericRelative:
      override def toString(): String = path.join("../"*ascent, "/", "")

      def parent: Relative exposes RootBoundaryExceeded = path match
        case init :+ last => makeRelative(ascent, init)
        case empty        => makeRelative(ascent + 1, Vector())

      def ancestor(ascent: Int): RelativePath exposes RootBoundaryExceeded =
        if ascent == 0 then makeRelative(ascent, path) else parent.ancestor(ascent - 1)

      def absolute(pwd: AbsolutePath): AbsolutePath exposes RootBoundaryExceeded =
        if ascent == 0 then makeAbsolute(pwd.path ++ path)
        else makeRelative(ascent - 1, path).absolute(pwd.parent)
      
      def /(filename: String): RelativePath exposes RootBoundaryExceeded = filename match
        case ".."     => path match
          case init :+ last => makeRelative(ascent, init)
          case empty        => makeRelative(ascent + 1, Vector())
        case "."      => makeRelative(ascent, path)
        case filename => makeRelative(ascent, path :+ filename)
      
      def ++(relative: GenericRelative): RelativePath exposes RootBoundaryExceeded =
        if relative.ascent == 0 then makeRelative(ascent, path ++ relative.path)
        else ancestor(relative.ascent) ++ makeRelative(ascent, relative.path)

      override def equals(that: Any): Boolean = that.unsafeMatchable match
        case that: Relative => ascent == that.ascent && path == that.path
        case _              => false

      override def hashCode: Int = path.hashCode ^ ascent

object Base extends Root("/", "/"):
  type RelativePath = Path.Relative
  type AbsolutePath = Path.Absolute
  
  protected def makeAbsolute(path: Vector[String]): AbsolutePath = Path.Absolute(path)
  
  protected def makeRelative(ascent: Int, path: Vector[String]): RelativePath =
    Path.Relative(ascent, path)