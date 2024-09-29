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
import guillotine.*
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*
import symbolism.*
import turbulence.*
import vacuous.*

import scala.compiletime.*
import scala.jdk.StreamConverters.*

import java.io as ji
import java.nio as jn
import java.nio.file as jnf

import language.experimental.pureFunctions

object Path:
  given Path is GenericPath = _.fullname

  inline given add(using path: Tactic[PathError], followable: Relative is Followable[GeneralForbidden, ?, ?])
          : Addable with
    type Self = Path
    type Operand = Relative

    type Result = Path
    inline def add(left: Path, right: Relative): Path = left.append(right)

  inline given add2(using path: Tactic[PathError], followable: SafeRelative is Followable[GeneralForbidden, ?, ?])
          : Addable with
    type Self = Path
    type Operand = SafeRelative

    type Result = Path
    inline def add(left: Path, right: SafeRelative): Path = left.append(right)

  given Insertion[Sh.Parameters, Path] = path => Sh.Parameters(path.fullname)

  given (using io: Tactic[IoError], streamCut: Tactic[StreamError]) => Path is Writable by Bytes as writableBytes =
    Writable.outputStreamBytes.contramap: path =>
      if !path.stdlib.toFile.nn.canWrite then abort(IoError(path))
      ji.BufferedOutputStream(ji.FileOutputStream(path.stdlib.toFile, false))


  given PathCreator[Path, GeneralForbidden, Optional[Windows.Drive]] with
    def path(root: Optional[Windows.Drive], descent: List[Name[GeneralForbidden]]) = root match
      case drive@Windows.Drive(_) => Windows.SafePath(drive, descent)
      case _                      => Unix.SafePath(descent)

  given Path is Communicable = path => Message(path.render)

  inline given (using Tactic[PathError]) => Decoder[Path] as decoder:
    def decode(text: Text): Path = Navigable.decode(text)

  given Path is Showable as showable = _.render
  given encoder: Encoder[Path] = _.render
  given Path is Inspectable = _.render

  inline def apply[PathType: GenericPath](path: PathType): Path raises PathError =
    Navigable.decode(path.pathText)

trait Path:
  this: Path =>

  def stdlib: jnf.Path = jnf.Path.of(fullname.s).nn
  def exists(): Boolean = jnf.Files.exists(stdlib)
  def touch()(using Tactic[IoError]): Unit = jnf.Files.write(stdlib, Array[Byte]())

  def wipe()(using deleteRecursively: DeleteRecursively)(using io: Tactic[IoError]): Path = this.also:
    deleteRecursively.conditionally(this)(jnf.Files.deleteIfExists(stdlib))

  def make[EntryType <: Entry]()(using maker: EntryMaker[EntryType, this.type]): EntryType = maker(this)
