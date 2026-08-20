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
package turbulence
import rudiments.reverse

import java.io as ji
import java.lang as jl
import java.nio.charset as jnc

import anticipation.*
import contingency.*
import denominative.*
import vacuous.*
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
  given lines: Ductile.Instance[LineSeparation, Text, Array[Text]^{}, Credit, Credit] =
    new Ductile:
      type Self = LineSeparation
      type Operand = Text
      type Result = Array[Text]^{}
      type Transport = Credit
      type Upstream = Credit

      def duct(consume stage: LineSeparation^)(using Buffering)
      :   (Duct[Text, Array[Text]^{}] { type Transport = Credit; type Upstream = Credit })^ =

        new Duct[Text, Array[Text]^{}]:
          type Transport = Credit
          type Upstream = Credit

          // The incomplete current line, carried across steps.
          private val partial: jl.StringBuilder = jl.StringBuilder(64)

          // A separator's first char at a window boundary (10 or 13; 0 = none).
          private var pending: Int = 0

          // End-of-stream lines (the resolved `pending` may complete some, plus
          // the final unterminated line), built once and drained by `flush`.
          private var drained: Boolean = false
          private var tail: List[Text] = Nil

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

          // Stage a line that is already built. `step`'s fast path constructs a
          // line straight from the source window and stages it here, so the
          // common case never touches `partial`.
          private update def emit(line: Text): Unit =
            if emitted == 0 then out0 = line else out1 = line
            emitted += 1

          private update def newline(): Unit =
            val line = partial.toString.tt
            partial.setLength(0)
            emit(line)

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
          private update def deliver(slots: scala.Array[AnyRef]^, at: Int): Int =
            var written: Int = 0

            if emitted > 0 then
              slots(at) = out0.asInstanceOf[AnyRef]
              written = 1

              if emitted == 2 then
                slots(at + 1) = out1.asInstanceOf[AnyRef]
                written = 2

              emitted = 0

            written

          // The index of the next separator at or after `from`, or `stop` if the
          // window holds none. An ordinary char is rejected by a single
          // comparison — both '\n' (10) and '\r' (13) are below 14, so only the
          // rarer control chars need the exact re-check — and runs are skipped
          // eight at a time, as `Ductile`'s UTF-8 kernel skips ASCII runs. That
          // kernel combines its eight with a bitwise OR, which cannot serve here:
          // OR only sets bits, so it cannot spot a low char among high ones. The
          // minimum can, and compiles to conditional moves rather than branches.
          private def scan(chars: scala.Array[Char], from: Int, stop: Int): Int =
            inline def least(left: Int, right: Int): Int = if left < right then left else right

            inline def lowest(at: Int): Int =
              least
               ( least(least(chars(at), chars(at + 1)), least(chars(at + 2), chars(at + 3))),
                 least(least(chars(at + 4), chars(at + 5)), least(chars(at + 6), chars(at + 7))) )

            var index: Int = from
            var found: Int = -1

            while found < 0 && index < stop do
              while index + 8 <= stop && lowest(index) > 13 do index += 8

              if index < stop then
                val char = chars(index)
                if char == '\n' || char == '\r' then found = index else index += 1

            if found < 0 then stop else found

          update def step(source: Region[Text])(range: Interval in source.type)
            ( target: Slate[Array[Text]^{}] )(space: Interval in target.type)
          :   Duct.Progress =

            val sourceInterval: Interval = range
            val sourceOffset = sourceInterval.start.n0
            val sourceLength = sourceInterval.size
            val targetInterval: Interval = space
            val targetOffset = targetInterval.start.n0
            val targetSpace = targetInterval.size
            val chars = unsafely(source.raw.asInstanceOf[scala.Array[Char]])
            val slots: scala.Array[AnyRef]^ =
              unsafely(target.raw.asInstanceOf[scala.Array[AnyRef]]).asInstanceOf[scala.Array[AnyRef]^]
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
                  if char == '\r' then { consumed += 1; act(stage.lfcr) } else act(stage.lf)
                else
                  if char == '\n' then { consumed += 1; act(stage.crlf) }
                  else act(stage.cr)

                produced += deliver(slots, targetOffset + produced)
              else
                val char = chars(sourceOffset + consumed)

                if char == '\n' then
                  consumed += 1

                  if consumed < sourceLength then
                    if chars(sourceOffset + consumed) == '\r'
                    then { consumed += 1; act(stage.lfcr) }
                    else act(stage.lf)

                    produced += deliver(slots, targetOffset + produced)
                  else pending = 10
                else if char == '\r' then
                  consumed += 1

                  if consumed < sourceLength then
                    if chars(sourceOffset + consumed) == '\n'
                    then { consumed += 1; act(stage.crlf) }
                    else act(stage.cr)

                    produced += deliver(slots, targetOffset + produced)
                  else pending = 13
                else
                  // The run of ordinary chars up to the next separator (or the
                  // window's end).
                  val start = sourceOffset + consumed
                  val stop = sourceOffset + sourceLength
                  val end = scan(chars, start, stop)
                  val length = end - start

                  // How many chars of a separator the fast path may take, or 0 if
                  // it does not apply. It applies when no line is carried from an
                  // earlier window, this line's separator lies inside the window
                  // with the char that resolves a two-char sequence present, and
                  // the policy maps that sequence to a plain line break. The line
                  // is then built with a single copy straight from the source
                  // window, leaving `partial` out of the common path — and with it
                  // the second copy, and the coder that `setLength` never resets,
                  // so a line with a non-Latin-1 char no longer taxes every line
                  // after it. Everything else falls through to the state machine
                  // above, which keeps owning every subtle case.
                  val separator: Int =
                    if end >= stop || end + 1 >= stop || partial.length > 0 then 0
                    else if chars(end) == '\n' then
                      if chars(end + 1) == '\r' then (if stage.lfcr == Action.Nl then 2 else 0)
                      else if stage.lf == Action.Nl then 1 else 0
                    else
                      if chars(end + 1) == '\n' then (if stage.crlf == Action.Nl then 2 else 0)
                      else if stage.cr == Action.Nl then 1 else 0

                  if separator > 0 then
                    emit(String(chars, start, length).tt)
                    consumed += length + separator
                    produced += deliver(slots, targetOffset + produced)
                  else
                    partial.append(chars, start, length)
                    consumed += length

            Duct.Progress(consumed, produced)

          override update def flush(target: Slate[Array[Text]^{}])(space: Interval in target.type)
          :   Int =

            val targetInterval: Interval = space
            val targetOffset = targetInterval.start.n0
            val targetSpace = targetInterval.size

            if !drained then
              drained = true
              var lines: List[Text] = Nil

              // A dangling separator-initial char at end-of-stream resolves as
              // its bare single-char sequence.
              if pending == 10 then act(stage.lf) else if pending == 13 then act(stage.cr)
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

            val slots: scala.Array[AnyRef]^ =
              unsafely(target.raw.asInstanceOf[scala.Array[AnyRef]]).asInstanceOf[scala.Array[AnyRef]^]

            while count < targetSpace && !tail.nil do
              slots(targetOffset + count) = tail.stdlib.head.asInstanceOf[AnyRef]
              tail = List.of(tail.stdlib.tail)
              count += 1

            count

  // Whether a `0x0A` or `0x0D` byte in this encoding can only be a line
  // terminator. UTF-8 is self-synchronising — every byte of a multi-byte
  // sequence has its high bit set — and the single-byte encodings are
  // ASCII-transparent by construction, so in all of these a terminator can be
  // found without decoding first. UTF-16 emphatically is not: `0x0A` occurs
  // inside its code units, and splitting its bytes would cut characters in half.
  def asciiTransparent(charset: jnc.Charset): Boolean =
    charset == jnc.StandardCharsets.UTF_8 || charset == jnc.StandardCharsets.US_ASCII
      || charset == jnc.StandardCharsets.ISO_8859_1

  // The byte-level twin of `lines`, for an ASCII-transparent encoding: it finds
  // terminators in the raw bytes and decodes each completed line in one step
  // with `String(bytes, …, charset)`, the JDK's fused decode-and-construct. That
  // removes the separate decoding stage and its intermediate `char[]` — the JFR
  // profile of the 4 MB corpus attributed 53% of line splitting to decoding,
  // against 44% to the splitting itself — and, because only whole lines are ever
  // decoded, a multi-byte character split across two windows needs no handling
  // at all: its bytes simply accumulate like any others.
  //
  // Deliberately not a `given`: it is applied by name from `delineate`, which
  // has checked the encoding, rather than found by `via`'s implicit search,
  // which has not. `lines` above remains the reference implementation, and the
  // test harness runs every policy and every fragmentation through both.
  def byteLines(consume policy: LineSeparation^, charset: jnc.Charset)(using Buffering)
  :   (Duct[Data, Array[Text]^{}] { type Transport = Credit; type Upstream = Credit })^ =

    new Duct[Data, Array[Text]^{}]:
      type Transport = Credit
      type Upstream = Credit

      // The incomplete current line's bytes, carried across steps: the
      // counterpart of the char duct's `StringBuilder`, and free of its coder
      // trap, since bytes carry no encoding state to inflate.
      private val partial: ji.ByteArrayOutputStream = ji.ByteArrayOutputStream(64)

      // A separator's first byte at a window boundary (10 or 13; 0 = none).
      private var pending: Int = 0

      private var drained: Boolean = false
      private var tail: List[Text] = Nil

      def regulation: Credit is Regulation = summon[Credit is Regulation]

      // One byte completes at most one line, so line-credit is a sound
      // byte-credit; surplus consumption is carried in `partial`.
      def translate(demand: Credit): Credit = demand

      override def quantum: Int = 2

      private var out0: Text = "".tt
      private var out1: Text = "".tt
      private var emitted: Int = 0

      private update def emit(line: Text): Unit =
        if emitted == 0 then out0 = line else out1 = line
        emitted += 1

      private update def newline(): Unit =
        val line = partial.toString(charset).nn.tt
        partial.reset()
        emit(line)

      private update def act(action: LineSeparation.Action): Unit = action match
        case Action.Nl   => newline()
        case Action.NlCr => newline(); partial.write(13)
        case Action.NlLf => newline(); partial.write(10)
        case Action.CrNl => partial.write(13); newline()
        case Action.NlNl => newline(); newline()
        case Action.Cr   => partial.write(13)
        case Action.Lf   => partial.write(10)
        case Action.LfNl => partial.write(10); newline()
        case Action.Skip => ()

      private update def deliver(slots: scala.Array[AnyRef]^, at: Int): Int =
        var written: Int = 0

        if emitted > 0 then
          slots(at) = out0.asInstanceOf[AnyRef]
          written = 1

          if emitted == 2 then
            slots(at + 1) = out1.asInstanceOf[AnyRef]
            written = 2

          emitted = 0

        written

      // The index of the next separator at or after `from`, or `stop` if the
      // window holds none. 10 and 13 differ only in their bottom three bits, so
      // one mask-and-compare rejects every byte outside 8-15 — and, because
      // `&&` short-circuits, does it in a single branch where two comparisons
      // take two. Only the 8-15 range pays the exact test. Sign extension needs
      // no masking away, since the mask clears those bits, and every byte of a
      // multi-byte UTF-8 sequence is negative and so rejected outright.
      //
      // Measured at 53% faster than two comparisons per byte over the 4 MB
      // corpus (the "Separator scan variants" suite, which keeps all three
      // honest). Tab (9) falls in the admitted range and pays the exact test
      // where it occurs, which is far cheaper than a second comparison on every
      // byte; biasing by two to narrow the range to 10-13 and exclude it
      // measured slower still, its extra operation costing more than the
      // false positives it avoids.
      private def scan(bytes: scala.Array[Byte], from: Int, stop: Int): Int =
        var index: Int = from

        while index < stop
            && { val byte = bytes(index)
                 (byte & 0xf8) != 0x08 || (byte != 10 && byte != 13) }
        do index += 1

        index

      update def step(source: Region[Data])(range: Interval in source.type)
        ( target: Slate[Array[Text]^{}] )(space: Interval in target.type)
      :   Duct.Progress =

        val sourceInterval: Interval = range
        val sourceOffset = sourceInterval.start.n0
        val sourceLength = sourceInterval.size
        val targetInterval: Interval = space
        val targetOffset = targetInterval.start.n0
        val targetSpace = targetInterval.size
        val bytes = unsafely(source.raw.asInstanceOf[scala.Array[Byte]])

        val slots: scala.Array[AnyRef]^ =
          unsafely(target.raw.asInstanceOf[scala.Array[AnyRef]]).asInstanceOf[scala.Array[AnyRef]^]

        var consumed: Int = 0
        var produced: Int = 0

        while consumed < sourceLength && produced + 2 <= targetSpace do
          if pending != 0 then
            val first = pending
            pending = 0
            val byte = bytes(sourceOffset + consumed)

            if first == 10 then
              if byte == 13 then { consumed += 1; act(policy.lfcr) } else act(policy.lf)
            else
              if byte == 10 then { consumed += 1; act(policy.crlf) } else act(policy.cr)

            produced += deliver(slots, targetOffset + produced)
          else
            val byte = bytes(sourceOffset + consumed)

            if byte == 10 then
              consumed += 1

              if consumed < sourceLength then
                if bytes(sourceOffset + consumed) == 13
                then { consumed += 1; act(policy.lfcr) }
                else act(policy.lf)

                produced += deliver(slots, targetOffset + produced)
              else pending = 10
            else if byte == 13 then
              consumed += 1

              if consumed < sourceLength then
                if bytes(sourceOffset + consumed) == 10
                then { consumed += 1; act(policy.crlf) }
                else act(policy.cr)

                produced += deliver(slots, targetOffset + produced)
              else pending = 13
            else
              val start = sourceOffset + consumed
              val stop = sourceOffset + sourceLength
              val end = scan(bytes, start, stop)
              val length = end - start

              // The fast path, as in the char duct: no line carried, the
              // separator inside the window with its resolving byte present,
              // and a policy that maps the sequence to a plain line break — so
              // the line's bytes go straight to the decoder in one call.
              val separator: Int =
                if end >= stop || end + 1 >= stop || partial.size > 0 then 0
                else if bytes(end) == 10 then
                  if bytes(end + 1) == 13 then (if policy.lfcr == Action.Nl then 2 else 0)
                  else if policy.lf == Action.Nl then 1 else 0
                else
                  if bytes(end + 1) == 10 then (if policy.crlf == Action.Nl then 2 else 0)
                  else if policy.cr == Action.Nl then 1 else 0

              if separator > 0 then
                emit(String(bytes, start, length, charset).tt)
                consumed += length + separator
                produced += deliver(slots, targetOffset + produced)
              else
                partial.write(bytes, start, length)
                consumed += length

        Duct.Progress(consumed, produced)

      override update def flush(target: Slate[Array[Text]^{}])(space: Interval in target.type)
      :   Int =

        val targetInterval: Interval = space
        val targetOffset = targetInterval.start.n0
        val targetSpace = targetInterval.size

        if !drained then
          drained = true
          var lines: List[Text] = Nil

          if pending == 10 then act(policy.lf) else if pending == 13 then act(policy.cr)
          pending = 0

          if emitted > 0 then
            lines ::= out0
            if emitted == 2 then lines ::= out1
            emitted = 0

          if partial.size > 0 then
            lines ::= partial.toString(charset).nn.tt
            partial.reset()

          tail = lines.reverse

        var count: Int = 0

        val slots: scala.Array[AnyRef]^ =
          unsafely(target.raw.asInstanceOf[scala.Array[AnyRef]]).asInstanceOf[scala.Array[AnyRef]^]

        while count < targetSpace && !tail.nil do
          slots(targetOffset + count) = tail.stdlib.head.asInstanceOf[AnyRef]
          tail = List.of(tail.stdlib.tail)
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
