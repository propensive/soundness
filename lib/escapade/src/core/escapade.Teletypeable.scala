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
package escapade
import rudiments.*

import anticipation.*
import fulminate.*
import gossamer.*
import hieroglyph.*
import iridescence.*
import prepositional.*
import spectacular.*
import vacuous.*

// The low-priority home of the plain `Showable` rendering: in the companion it competed,
// ambiguously, with `colorable` — both subjects are bare type parameters, and a type with
// both upstream instances matched both (issue #261). Owner priority resolves the choice
// (`Decodable2` is the established precedent), preferring `colorable` where a `Colorable`
// exists and falling back here; an inline `summonFrom` was rejected because its per-summon
// expansion trips the `-scalajs` SAM/capture divergence at downstream sites.
trait Teletypeable2:
  given showable: [value: Showable] => value is Teletypeable = value => Teletype(value.show)

object Teletypeable extends Teletypeable2:
  given teletype: Teletype is Teletypeable = identity(_)
  given text: Text is Teletypeable = text => Teletype(text)

  given message: Message is Teletypeable = _.fold[Teletype](e""): (acc, next, level) =>
    level match
      case 0 => e"$acc$next"
      case 1 => e"$acc$Italic(${Fg(Chroma(0xefe68b))}($next))"
      case _ => e"$acc$Italic($Bold(${Fg(Chroma(0xffd600))}($next)))"

  given option: [value: Teletypeable] => Option[value] is Teletypeable =
    case None        => Teletype("empty".show)
    case Some(value) => value.teletype

  // A `Showable` value renders through its `Colorable` instance where one exists — in that
  // type's colour — and plainly otherwise. A single prioritized given rather than two peers:
  // as two, both subjects were bare type parameters, so a type with both upstream instances
  // matched both, ambiguously (issue #261); `summonFrom` chooses `Colorable` first, in the
  // manner of `telekinesis.Servable.media`.
  given colorable: [value: {Showable as showable, Colorable as colorable}]
  =>  value is Teletypeable =

    value => e"${value.color}(${value.show})"

  given error: Error is Teletypeable = _.message.teletype

  given double: (decimalizer: Decimalizer) => Double is Teletypeable = double =>
    Teletype.styled(decimalizer.decimalize(double))(_.copy(fg = Chroma(0xffd600)))

  given throwable: Throwable is Teletypeable = throwable =>
    // The simple name: the trailing run of characters after the last `.` in the binary name.
    val name: Text = throwable.getClass.getName.nn.show.keep(_ != '.', Bidi.Rtl)

    Teletype.styled[String](name.s)(_.copy(fg = Chroma(0xdc133b)))

trait Teletypeable extends Typeclass.Pure:
  def teletype(value: Self): Teletype

  def contramap[self2](lambda: self2 -> Self): self2 is Teletypeable =
    value => teletype(lambda(value))
