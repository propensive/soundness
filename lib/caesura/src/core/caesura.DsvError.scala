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
package caesura

import anticipation.*
import denominative.*
import fulminate.*

object DsvError:
  given communicable: Reason is Communicable =
    case Reason.MisplacedQuote =>
      m"a quote was found after the start of a cell"

    case Reason.Absent =>
      m"the cell was absent"

    case Reason.Unparseable(value, expected) =>
      m"the value $value could not be parsed as $expected"

  // `MisplacedQuote` is a parse-phase error; `Absent` and `Unparseable` are
  // decode-phase errors (a cell missing from, or unparseable in, a row being
  // decoded into a product). The latter accrue through a `Foci[CellRef]`, which
  // carries the real cell location, so they construct via the position-free
  // companion `apply` below.
  enum Reason(val number: Int) extends Clarification:
    case MisplacedQuote                          extends Reason(1)
    case Absent                                  extends Reason(2)
    case Unparseable(value: Text, expected: Text) extends Reason(3)

  // Decode-phase constructor: the offending cell's position is carried by the
  // accrued `CellRef` focus, not by the `DsvError` itself, so `row`/`column`/
  // `offset` are filled with placeholders.
  def apply(format: DsvFormat, reason: Reason)(using Diagnostics): DsvError =
    DsvError(format, reason, Prim, Prim, 0)

// `row` and `column` are the 1-based position of the offending cell, and `offset`
// is the character (UTF-16 code-unit) offset into the input at which the failure
// was detected. A byte offset is not available here because the parser operates on
// already-decoded `Text`, the source bytes having been discarded upstream.
case class DsvError
  ( format: DsvFormat, reason: DsvError.Reason, row: Ordinal, column: Ordinal, offset: Int )
  ( using Diagnostics )
extends Error(364, reason.number)
  ( reason match
      case DsvError.Reason.MisplacedQuote =>
        m"could not parse row data at row ${row.n1}, column ${column.n1} because $reason"

      case _ =>
        m"could not decode the cell because $reason" )
