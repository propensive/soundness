/*

    Fury, version 0.33.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.

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

trait Root(val separator: String, val prefix: String)

object Classpath extends Root("/", "classpath:")

sealed trait Path[+R <: Root]:
  def parent: Path[R] raises RootBoundaryExceeded
  def ancestor(ascent: Int): Path[R] raises RootBoundaryExceeded
  def absolute[R2 >: R <: Root](pwd: Path.Absolute[R2]): Path.Absolute[R2] raises RootBoundaryExceeded
  def /(filename: String): Path[R] raises RootBoundaryExceeded
  def ++(path: Path.Relative): Path[R]

object Path:
  case class Absolute[+R <: Root](root: R, path: Vector[String]) extends Path[R]:
    override def toString(): String = path.join(root.prefix, root.separator, "")
    
    def parent: Absolute[R] raises RootBoundaryExceeded = path match
      case init :+ last => Absolute[R](root, init)
      case _            => throw RootBoundaryExceeded(root)

    def ancestor(ascent: Int): Absolute[R] raises RootBoundaryExceeded =
      if path.length == 0 then this else parent.ancestor(ascent - 1)
    
    def absolute[R2 >: R <: Root](pwd: Absolute[R2]): Absolute[R2] raises RootBoundaryExceeded = this
    
    def /(filename: String): Absolute[R] raises RootBoundaryExceeded = filename match
      case ".."     => path match
        case init :+ last => Absolute(root, init)
        case _            => throw RootBoundaryExceeded(root)
      case "."      => this
      case filename => Absolute(root, path :+ filename)
    
    def ++(relative: Relative): Absolute[R] raises RootBoundaryExceeded =
      if relative.ascent > 0 then ancestor(relative.ascent) ++ relative.copy(ascent = 0)
      else Absolute(root, path ++ relative.path)

  case class Relative(ascent: Int, path: Vector[String]) extends Path[Nothing]:
    override def toString(): String = path.join("../"*ascent, "/", "")

    def parent: Relative raises RootBoundaryExceeded = path match
      case init :+ last => Relative(ascent, init)
      case empty        => Relative(ascent + 1, Vector())

    def ancestor(ascent: Int): Relative raises RootBoundaryExceeded =
      if ascent == 0 then this else parent.ancestor(ascent - 1)

    def absolute[R2 >: Nothing <: Root](pwd: Absolute[R2]): Absolute[R2] raises RootBoundaryExceeded =
      if ascent == 0 then pwd.copy(path = pwd.path ++ path) else Relative(ascent - 1, path).absolute(pwd.parent)
    
    def /(filename: String): Relative raises RootBoundaryExceeded = filename match
      case ".."     => path match
        case init :+ last => Relative(ascent, init)
        case empty        => Relative(ascent + 1, Vector())
      case "."      => this
      case filename => Relative(ascent, path :+ filename)
    
    def ++(relative: Relative): Relative raises RootBoundaryExceeded =
      if relative.ascent == 0 then copy(path = path ++ relative.path)
      else ancestor(relative.ascent) ++ relative.copy(ascent = 0)
