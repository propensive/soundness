/*

    Slalom, version 0.1.0. Copyright 2020-21 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package slalom

import rudiments.*

class SlalomException(message: String) extends Exception(str"slalom: $message")

case class RootBoundaryExceeded(root: Root)
extends SlalomException(str"attempted to exceed the root boundary")

trait GenericRelative:
  def ascent: Int
  def path: Vector[String]

trait Root(val separator: String, val prefix: String):
  def root: Root = this
  sealed trait Path:
    def parent: Path raises RootBoundaryExceeded
    def ancestor(ascent: Int): Path raises RootBoundaryExceeded
    def absolute(pwd: Path.Absolute): Path.Absolute raises RootBoundaryExceeded
    def /(filename: String): Path raises RootBoundaryExceeded
    def ++(path: GenericRelative): Path

  object Path:
    case class Absolute(path: Vector[String]) extends Path:
      override def toString(): String = path.join(prefix, separator, "")
      
      def parent: Absolute raises RootBoundaryExceeded = path match
        case init :+ last => Absolute(init)
        case _            => throw RootBoundaryExceeded(root)

      def ancestor(ascent: Int): Absolute raises RootBoundaryExceeded =
        if path.length == 0 then this else parent.ancestor(ascent - 1)
      
      def absolute(pwd: Absolute): Absolute raises RootBoundaryExceeded = this
      
      def /(filename: String): Absolute raises RootBoundaryExceeded = filename match
        case ".."     => path match
          case init :+ last => Absolute(init)
          case _            => throw RootBoundaryExceeded(root)
        case "."      => this
        case filename => Absolute(path :+ filename)
      
      def ++(relative: GenericRelative): Absolute raises RootBoundaryExceeded =
        if relative.ascent > 0 then ancestor(relative.ascent) ++ Relative(0, relative.path)
        else Absolute(path ++ relative.path)

    case class Relative(ascent: Int, path: Vector[String]) extends Path, GenericRelative:
      override def toString(): String = path.join("../"*ascent, "/", "")

      def parent: Relative raises RootBoundaryExceeded = path match
        case init :+ last => Relative(ascent, init)
        case empty        => Relative(ascent + 1, Vector())

      def ancestor(ascent: Int): Relative raises RootBoundaryExceeded =
        if ascent == 0 then this else parent.ancestor(ascent - 1)

      def absolute(pwd: Absolute): Absolute raises RootBoundaryExceeded =
        if ascent == 0 then pwd.copy(path = pwd.path ++ path) else Relative(ascent - 1, path).absolute(pwd.parent)
      
      def /(filename: String): Relative raises RootBoundaryExceeded = filename match
        case ".."     => path match
          case init :+ last => Relative(ascent, init)
          case empty        => Relative(ascent + 1, Vector())
        case "."      => this
        case filename => Relative(ascent, path :+ filename)
      
      def ++(relative: GenericRelative): Relative raises RootBoundaryExceeded =
        if relative.ascent == 0 then copy(path = path ++ relative.path)
        else ancestor(relative.ascent) ++ Relative(ascent, relative.path)

object Classpath extends Root("/", "classpath:")
