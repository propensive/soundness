/*
    Galilei, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package galilei

import anticipation.*
import contextual.*
import contingency.*
import fulminate.*
import galilei.*
import gossamer.*
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*
import symbolism.*
import vacuous.*

import scala.compiletime.*
import scala.jdk.StreamConverters.*

import java.nio as jn
import java.nio.file as jnf

import language.experimental.pureFunctions

object Directory:
  given Directory is Inspectable = directory => t"directory:${directory.path.render}"

  given GenericWatchService:
    def apply(): java.nio.file.WatchService = jnf.Path.of("/").nn.getFileSystem.nn.newWatchService().nn

  given Directory is GenericDirectory = _.path.fullname

case class Directory(path: Path) extends Unix.Entry, Windows.Entry:
  def children: LazyList[Path] = jnf.Files.list(path.stdlib).nn.toScala(LazyList).map: child =>
    path / Name.unsafe(child.getFileName.nn.toString.nn.tt)

  def descendants(using DereferenceSymlinks, Tactic[IoError], PathResolver[Directory, Path]): LazyList[Path] =
    children #::: children.filter(_.is[Directory]).map(_.as[Directory]).flatMap(_.descendants)

  def size()(using PathResolver[Directory, Path], PathResolver[File, Path]): ByteSize raises IoError =
    import filesystemOptions.doNotDereferenceSymlinks
    descendants.map(_.at[File].let(_.size()).or(0.b)).foldLeft(0.b)(_ + _)

  @targetName("child")
  infix def / (name: Name[GeneralForbidden]): Path = path / name

  @targetName("child2")
  inline infix def / (name: Text)(using Tactic[PathError]): Path = path / Name(name)
