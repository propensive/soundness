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
package turbulence

import java.lang as jl

import anticipation.*
import beneficence.*
import prepositional.*
import zephyrine.*

object LineSeparation:
  // Line splitting as a pipeline stage: a `LineSeparation` policy value applies
  // directly with `stream.via(policy)` (or the `.lines` combinators), yielding a
  // record stream on the boxed medium — one `Text` per line, without its
  // terminator. The duct dispatches through the `Action` table each packaged
  // `LineSeparation` policy defines (`Nl`/`Cr`/`Lf`/…), so all the CR/LF/CRLF
  // handling lives in one place. A separator's first char at a window boundary
  // is carried in
  // `pending` (the two-char sequences resolve across windows); the incomplete
  // final line is carried in `partial` and emitted by `flush`, so an input
  // without a trailing separator still yields its last line, and an empty input
  // yields no lines at all.
  //
  // A separator-free input grows `partial` without bound — the inherent
  // exposure of any line reader; cap upstream if the input is adversarial.
  given lines: Ductile.Of[LineSeparation, Text, IArray[Text], Credit, Credit] =
    new Ductile:
      type Self = LineSeparation
      type Operand = Text
      type Result = IArray[Text]
      type Transport = Credit
      type Upstream = Credit

      def duct(consume stage: LineSeparation^)(using Buffering)
      :   (Duct[Text, IArray[Text]] { type Transport = Credit; type Upstream = Credit })^ =

        new Duct[Text, IArray[Text]]:
          type Transport = Credit
          type Upstream = Credit

          // The incomplete current line, carried across steps.
          private val partial: jl.StringBuilder = jl.StringBuilder(64)

          // A separator's first char at a window boundary (10 or 13; 0 = none).
          private var pending: Int = 0

          // End-of-stream lines (the resolved `pending` may complete some, plus
          // the final unterminated line), built once and drained by `flush`.
          private var drained: Boolean = false
          private var tail: scala.collection.immutable.List[Text] = Nil.stdlib

          def regulation: Credit is Regulation = summon[Credit is Regulation]

          // One char completes at most one line, so line-credit is a sound
          // char-credit as it stands; surplus consumption is carried in
          // `partial`, per the conservative-bound contract.
          def translate(demand: Credit): Credit = demand

          // An `NlNl` action emits two lines at once.
          override def quantum: Int = 2

          // Completed lines from the most recent action, staged in fields (at
          // most two — `NlNl`) rather than emitted through a callback: a
          // closure's captures hide from subsequent statements under
          // separation checking, so the caller drains these instead.
          private var out0: Text = "".tt
          private var out1: Text = "".tt
          private var emitted: Int = 0

          private update def newline(): Unit =
            val line = partial.toString.tt
            partial.setLength(0)
            if emitted == 0 then out0 = line else out1 = line
            emitted += 1

          // Apply `action` to the current line state, staging completed lines.
          private update def act(action: LineSeparation.Action): Unit = action match
            case Action.Nl   => newline()
            case Action.NlCr => newline(); partial.append('\r')
            case Action.NlLf => newline(); partial.append('\n')
            case Action.CrNl => partial.append('\r'); newline()
            case Action.NlNl => newline(); newline()
            case Action.Cr   => partial.append('\r')
            case Action.Lf   => partial.append('\n')
            case Action.LfNl => partial.append('\n'); newline()
            case Action.Skip => ()

          // Deliver the staged lines into the target window at `at`, returning
          // how many were written. The caller guarantees two free slots.
          private update def deliver(slots: Array[AnyRef]^, at: Int): Int =
            var written: Int = 0

            if emitted > 0 then
              slots(at) = out0.asInstanceOf[AnyRef]
              written = 1

              if emitted == 2 then
                slots(at + 1) = out1.asInstanceOf[AnyRef]
                written = 2

              emitted = 0

            written

          update def step
            ( source: input.Storage,
              sourceOffset: Int,
              sourceLength: Int,
              target: output.Storage,
              targetOffset: Int,
              targetSpace: Int )
          :   Duct.Progress =

            val chars = source.asInstanceOf[Array[Char]]
            var consumed: Int = 0
            var produced: Int = 0

            // The guard reserves `quantum` slots so any action fits whole; a
            // stop here with unconsumed input is a partial step, which the
            // kernel handles (the produced window is delivered first).
            while consumed < sourceLength && produced + 2 <= targetSpace do
              if pending != 0 then
                val first = pending
                pending = 0
                val char = chars(sourceOffset + consumed)

                if first == 10 then
                  if char == '\r' then { consumed += 1; act(stage.lfcr) }
                  else act(stage.lf)
                else
                  if char == '\n' then { consumed += 1; act(stage.crlf) }
                  else act(stage.cr)

                produced += deliver(target.asInstanceOf[Array[AnyRef]^], targetOffset + produced)
              else
                val char = chars(sourceOffset + consumed)

                if char == '\n' then
                  consumed += 1

                  if consumed < sourceLength then
                    if chars(sourceOffset + consumed) == '\r'
                    then { consumed += 1; act(stage.lfcr) }
                    else act(stage.lf)

                    produced +=
                      deliver(target.asInstanceOf[Array[AnyRef]^], targetOffset + produced)
                  else pending = 10
                else if char == '\r' then
                  consumed += 1

                  if consumed < sourceLength then
                    if chars(sourceOffset + consumed) == '\n'
                    then { consumed += 1; act(stage.crlf) }
                    else act(stage.cr)

                    produced +=
                      deliver(target.asInstanceOf[Array[AnyRef]^], targetOffset + produced)
                  else pending = 13
                else
                  // Bulk-append the run of ordinary chars up to the next
                  // separator (or the window's end).
                  val start = sourceOffset + consumed
                  val stop = sourceOffset + sourceLength
                  var end = start

                  while end < stop && { val c = chars(end); c != '\n' && c != '\r' } do end += 1

                  partial.append(chars, start, end - start)
                  consumed += end - start

            Duct.Progress(consumed, produced)

          override update def flush
            ( target: output.Storage, targetOffset: Int, targetSpace: Int )
          :   Int =

            if !drained then
              drained = true
              var lines: scala.collection.immutable.List[Text] = Nil.stdlib

              // A dangling separator-initial char at end-of-stream resolves as
              // its bare single-char sequence.
              if pending == 10 then act(stage.lf)
              else if pending == 13 then act(stage.cr)
              pending = 0

              if emitted > 0 then
                lines ::= out0
                if emitted == 2 then lines ::= out1
                emitted = 0

              // The final line, if the input didn't end with a separator.
              if partial.length > 0 then
                lines ::= partial.toString.tt
                partial.setLength(0)

              tail = lines.reverse

            var count: Int = 0

            while count < targetSpace && tail.nonEmpty do
              target.asInstanceOf[Array[AnyRef]^](targetOffset + count) =
                tail.head.asInstanceOf[AnyRef]

              tail = tail.tail
              count += 1

            count

  enum NewlineSeq:
    case Cr, Lf, CrLf, LfCr

    def text: Text = this match
      case Cr   => "\r".tt
      case Lf   => "\n".tt
      case CrLf => "\r\n".tt
      case LfCr => "\n\r".tt

  enum Action:
    case Nl, NlCr, NlLf, LfNl, CrNl, NlNl, Cr, Lf, Skip

case class LineSeparation
  ( newline: LineSeparation.NewlineSeq,
    cr:      LineSeparation.Action,
    lf:      LineSeparation.Action,
    crlf:    LineSeparation.Action,
    lfcr:    LineSeparation.Action )
extends Findable:

  def newlineData = newline match
    case LineSeparation.NewlineSeq.Cr   => Data(13)
    case LineSeparation.NewlineSeq.Lf   => Data(10)
    case LineSeparation.NewlineSeq.CrLf => Data(13, 10)
    case LineSeparation.NewlineSeq.LfCr => Data(10, 13)
