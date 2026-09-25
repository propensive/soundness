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
package galilei

import anticipation.*
import contingency.*
import distillate.*
import gossamer.*
import prepositional.*
import spectacular.*
import vacuous.*

// A file-creation mask, as POSIX `umask(2)` defines it: the permission bits withheld from
// every file and directory created while it applies, so that `Umask(0o077)` yields files
// readable by their owner alone. It is process-wide state for an ordinary process, which is
// why the operating system applies it for free; a daemon serving several invocations, each
// with its own mask, cannot rely on the process's, and applies the invocation's explicitly
// to what it creates. The mask reaches galilei as a contextual value: a `Provider` — a
// daemon's service handle, say — makes the invocation's mask summonable with no import, and
// where none is in scope the operating system's own mask applies, as it always did.
object Umask extends UmaskPriority:
  trait Provider:
    def umask: Optional[Umask]

  // The process's own mask, whatever it is, applied by the operating system at creation.
  val process: Umask = -1

  // The conventional bits an entry is created with before any mask applies (Scala has no
  // octal literals): `0o666` for a file or FIFO, `0o777` for a directory.
  val fileBits: Int = 0x1b6
  val directoryBits: Int = 0x1ff

  def apply(bits: Int): Umask = bits & directoryBits

  // The conventional octal rendering, as `umask` prints it: `022`, `077`.
  def parse(text: Text): Optional[Umask] =
    try Umask(Integer.parseInt(text.s, 8)) catch case _: NumberFormatException => Unset

  given provided: (provider: Provider^) => Umask = provider.umask.or(process)
  given showable: Umask is Showable = _.octal
  given encodable: Umask is Encodable in Text = _.octal
  given decodable: Umask is Decodable in Text = text => parse(text).or(process)

  extension (umask: Umask)
    def octal: Text =
      if umask == -1 then t"" else
        val digits: String = Integer.toOctalString(umask).nn
        ("000".substring(digits.length).nn + digits).tt

    def bits: Int = umask

    // The mode a creation should request for an entry that would conventionally be created
    // with `bits` (`fileBits` or `directoryBits`), or `Unset` when the process's own mask is
    // to apply.
    def mode(bits: Int): Optional[Int] = if umask == -1 then Unset else bits & ~umask

// Lower priority than the companion's `provided` (a class's givens take precedence over its
// parent's), so a `Provider` in scope wins and, absent one, nothing changes.
private[galilei] trait UmaskPriority:
  given inherited: Umask = Umask.process

opaque type Umask = Int
