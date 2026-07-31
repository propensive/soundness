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
┃    Soundness, version 0.64.0.                                                                    ┃
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
package zeppelin

import scala.caps

import proscenium.compat.*

import java.nio as jn
import java.nio.channels as jnc
import java.nio.file as jnf

import anticipation.*
import aperture.*
import contingency.*
import prepositional.*
import rudiments.*
import serpentine.*
import vacuous.*
import zephyrine.*

// The scoped capability provided by opening an archive as `Zip`: `path.open[Zip]()`. Unlike a
// detached `Zipfile` (whose `FileSource` re-opens the file for every read), a `ZipHandle` reads
// through a single channel held open for the duration of the block, so entry payloads resolve
// with no per-read open/close cost — and, correspondingly, must be consumed within the scope.
// (Zeppelin is not yet capture-checked, so the confinement is enforced only for callers
// compiled with capture checking; the annotations sharpen when the module joins the rollout.)
class ZipHandle private[zeppelin] (private[zeppelin] val zipfile: Zipfile)
extends caps.ExclusiveCapability:
  def entries: List[Zip.Entry] = zipfile.entries
  def entry(ref: Path on Zip): Zip.Entry raises ZipError = zipfile.entry(ref)
  def comment: Optional[Text] = zipfile.comment

// A named class rather than an anonymous given instance, for the reasons documented on
// galilei's `FileOpenable`. Archives open read-only: a `Write` mode is refused with
// `ZipError.Reason.WriteUnsupported` until writing lands.
class ZipOpenable[path: Abstractable across Paths to Text](using Tactic[ZipError])
extends Openable:

  type Self = path
  type Form = Zip
  type Operand = Nothing
  type Result = ZipHandle

  def open[grants <: Grant, result]
    ( value: path, mode: Mode granting grants, flags: List[Nothing] )
    ( block: (ZipHandle & Granting[grants]) ?=> result )
  :   result =

    if mode.atoms.has(Write) then abort(ZipError(ZipError.Reason.WriteUnsupported))

    val channel =
      jnc.FileChannel.open(jnf.Path.of(value.generic.s), jnf.StandardOpenOption.READ).nn

    try
      val zipfile = Zipfile.parse(Zipfile.ChannelSource(channel))
      block(using new ZipHandle(zipfile) with Granting[grants] {})
    finally channel.close()

// Opens an in-memory archive; no channel is involved, but access is scoped all the same, for
// consistency with every other form of `Zip` target.
class ZipDataOpenable(using Tactic[ZipError]) extends Openable:
  type Self = Data
  type Form = Zip
  type Operand = Nothing
  type Result = ZipHandle

  def open[grants <: Grant, result]
    ( value: Data, mode: Mode granting grants, flags: List[Nothing] )
    ( block: (ZipHandle & Granting[grants]) ?=> result )
  :   result =

    if mode.atoms.has(Write) then abort(ZipError(ZipError.Reason.WriteUnsupported))
    block(using new ZipHandle(Zipfile.parse(Zipfile.DataSource(value))) with Granting[grants] {})
