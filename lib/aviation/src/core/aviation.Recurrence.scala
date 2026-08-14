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
package aviation

import anticipation.*
import proscenium.compat.*
import fulminate.*
import rudiments.*
import contingency.*
import cosmopolite.{Locale, en, fr, de, es}
import distillate.*
import gossamer.*
import prepositional.*
import quantitative.*
import spectacular.*
import symbolism.*
import vacuous.*

// A recurrent event in the ISO 8601 "repeating interval" form `R[n]/<start>/<period>`: an event at
// `start` that repeats every `period` (an ISO duration). `repetitions` is the count of occurrences
// (`Unset` is unbounded, ISO's `R/`); the occurrences are `start`, `start + period`,
// `start + 2·period`, … The period's type is preserved so it adds correctly — `1*Month` honours
// calendar months, `8*Hour` is a fixed physical span — so a recurrence is typed by its point
// (`Topic`, via `of`) and its period (`Operand`, via `by`): a `Recurrence of Date by (Timespan of
// Month.type)`.
object Recurrence:
  def apply[point, span](start: point, period: span, repetitions: Optional[Int] = Unset)
  :   Recurrence of point by span =

    Impl(start, period, repetitions)

  private case class Impl[point, span](start: point, period: span, repetitions: Optional[Int])
  extends Recurrence:
    type Topic = point
    type Operand = span

  given encodable: [point: Encodable in Text, span <: Timespan]
  =>  (Recurrence of point by span) is Encodable in Text =

    recurrence =>
      val number = recurrence.repetitions.lay(t""): repetitions => repetitions.show

      val period: Timespan = recurrence.period
      t"R$number/${recurrence.start.encode}/${period.encode}"

  // A natural-language description, e.g. "every 2 weeks, 5 times from <start>". `.encode` is the
  // ISO wire form; `.show` is for people. Each language is a `Vernacular`, gated on its `Locale`.
  given englishShowable: [point: Showable, span <: Timespan] => Locale[en]
  =>  (Recurrence of point by span) is Showable =
    r => Vernacular.english.recurrence(r.period, r.repetitions, r.start.show)

  given frenchShowable: [point: Showable, span <: Timespan] => Locale[fr]
  =>  (Recurrence of point by span) is Showable =
    r => Vernacular.french.recurrence(r.period, r.repetitions, r.start.show)

  given germanShowable: [point: Showable, span <: Timespan] => Locale[de]
  =>  (Recurrence of point by span) is Showable =
    r => Vernacular.german.recurrence(r.period, r.repetitions, r.start.show)

  given spanishShowable: [point: Showable, span <: Timespan] => Locale[es]
  =>  (Recurrence of point by span) is Showable =
    r => Vernacular.spanish.recurrence(r.period, r.repetitions, r.start.show)

  // Parsing erases the period's static radix set, so the caller names the period type they expect
  // (e.g. `decode[Recurrence of Timestamp by (Timespan of Month.type)]`); the parsed duration is
  // tagged with it. A mismatched topic mis-reads the duration, so name the one the data uses.
  given decodable: [point: Decodable in Text, topic <: Radix]
  =>  ( Tactic[Recurrence.Error], Tactic[Moment.Error], Tactic[Timespan.Error] )
  =>  (Recurrence of point by (Timespan of topic)) is Decodable in Text =

    text =>
      text.cut(t"/") match
        case List(repeats, start, period) =>
          val repetitions =
            if repeats == t"R" then Unset else
              val digits = repeats.skip(1).s

              if repeats.starts(t"R") && digits.nonEmpty && digits.forall(_.isDigit)
              then digits.toInt
              else abort(Recurrence.Error(text))

          val span = period.as[Timespan].asInstanceOf[Timespan of topic]

          Recurrence(start.as[point], span, repetitions)

        case _ =>
          abort(Recurrence.Error(text))

  // A `Recurrence` is `Recurrent`: its occurrences are `start`, `start + period`, … bounded by
  // `repetitions` if set, otherwise infinite. The `Addable` it needs is captured here, so generic
  // `Recurrent` code (`occurrences`/`until`/`within`) works on a `Recurrence`.
  given recurrent: [point, span] => (addable: point is Addable by span to point)
  =>  ( (Recurrence of point by span) is Recurrent { type Topic = point } ) =

    sequence =>
      val all = Chain.iterate(sequence.start)(addable.add(_, sequence.period))
      sequence.repetitions.lay(all)(all.take(_))

  // RecurrenceError → Recurrence.Error
  case class Error(value: Text)(using Diagnostics)
  extends fulminate.Error(m"the value $value is not a valid ISO 8601 repeating interval")

trait Recurrence extends Topical, Operable:
  def start: Topic
  def period: Operand
  def repetitions: Optional[Int]
