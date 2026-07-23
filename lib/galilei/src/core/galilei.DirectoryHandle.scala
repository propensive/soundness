                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.63.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package galilei

import scala.caps

import anticipation.*
import aperture.*
import contingency.*
import gossamer.*
import spectacular.*
import prepositional.*
import rudiments.*
import serpentine.*
import vacuous.*

import IoError.{Operation, Reason}

// The scoped capability provided by opening a directory: `path.open[Directory](Read & Write)`.
// Each handle's `Plane` is a fresh abstract type, so paths formed within one handle's scope
// (`dir / "src" / "main.scala"`) are typed on that handle's plane alone: using them under
// another handle, or letting them escape, is a compile error; and the plane's naming rules
// (see `Subtree`) make `..` inexpressible, so every path denotes an entry within the opened
// directory's subtree, by construction.
trait DirectoryHandle extends caps.ExclusiveCapability:
  type Plane <: Subtree
  type Under <: Platform

  val stem: Path on Under

  // The atomic modes this handle was opened with, for runtime attenuation and access-register
  // checks, which cannot see grants after erasure.
  val atoms: Set[Mode]

  def base: Path on Plane = Path[Plane, EmptyTuple.type, EmptyTuple](t"", Nil)

  // A method rather than an extension: generic `/` extensions (e.g. symbolism's) are lexically
  // visible at use sites and would be tried first, failing without falling through.
  transparent inline infix def / (child: Any) = base / child

  // Public: called from transparent-inline subtree operations, where a `private` member's
  // inline-accessor bridge would fail capture checking.
  def resolve(path: Path on Plane): Path on Under =
    path.descent.reverse.foldLeft(stem): (parent, name) =>
      parent.child(name)(using Unsafe)

// A named class rather than an anonymous given instance, for the reasons documented on
// `FileOpenable`. Opening verifies that the entry exists and is a directory, so a handle
// always denotes a real directory at the moment it is granted.
class DirectoryOpenable[filesystem <: Platform: Filesystem, path <: Path on filesystem]
  ( using backend: FilesystemBackend on filesystem, ioError: Tactic[IoError] )
extends Openable:

  type Self = path
  type Form = Directory
  type Operand = Nothing
  type Result = DirectoryHandle { type Under = filesystem }

  def open[grants <: Grant, result]
    ( value: path, mode: Mode granting grants, flags: List[Nothing] )
    ( block: (((DirectoryHandle { type Under = filesystem }) & Granting[grants])^) ?=> result )
  :   result =

    if backend.stat(value, true).entry != Directory
    then abort(IoError(value, Operation.Open, Reason.IsNotDirectory))

    // The register works on real paths, so two routes to one directory (via symlinks, or via
    // `.` and repeated separators) register as the same subtree.
    val real: Text = value.javaPath.toRealPath().nn.toString.tt

    if !AccessRegister.acquire(real, mode.atoms)
    then abort(IoError(value, Operation.Open, Reason.Busy))

    try
      val handle =
        new DirectoryHandle with Granting[grants]:
          type Under = filesystem
          val stem: Path on filesystem = value
          val atoms: Set[Mode] = mode.atoms

      block(using handle)
    finally AccessRegister.release(real, mode.atoms)
