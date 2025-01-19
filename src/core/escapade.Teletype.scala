/*
    Escapade, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package escapade

import anticipation.*
import contextual.*
import denominative.*
import gossamer.*
import prepositional.*
import rudiments.*
import spectacular.*
import symbolism.*
import turbulence.*
import vacuous.*

import scala.util.*

import language.experimental.pureFunctions

object Teletype:
  given add: NotGiven[Teletype is Textual] => Teletype is Addable:
    type Operand = Teletype
    type Result = Teletype
    inline def add(left: Teletype, right: Teletype): Teletype = left.append(right)

  given writableOut: Stdio => SimpleWritable[Out.type, Teletype] =
    (_, output) => Out.print(output)

  given writableErr: Stdio => SimpleWritable[Err.type, Teletype] =
    (_, output) => Err.print(output)

  given Teletype is Textual:
    type Show[ValueType] = ValueType is Teletypeable
    def classTag: ClassTag[Teletype] = summon[ClassTag[Teletype]]
    def size(text: Teletype): Int = text.plain.s.length
    def text(teletype: Teletype): Text = teletype.plain
    def length(text: Teletype): Int = text.plain.s.length
    def apply(text: Text): Teletype = Teletype(text)

    def map(text: Teletype, lambda: Char => Char): Teletype =
      Teletype(Text(text.plain.s.map(lambda)), text.spans, text.insertions)

    def segment(text: Teletype, interval: Interval): Teletype =
      text.dropChars(interval.start.n0).takeChars(interval.size)

    val empty: Teletype = Teletype.empty
    def concat(left: Teletype, right: Teletype): Teletype = left.append(right)
    def unsafeChar(text: Teletype, index: Ordinal): Char = text.plain.s.charAt(index.n0)
    def indexOf(text: Teletype, sub: Text, start: Ordinal): Optional[Ordinal] =
      text.plain.s.indexOf(sub.s, start.n0).puncture(-1).let(_.z)

    def show[ValueType: Teletypeable](value: ValueType) = value.teletype
    def buffer(size: Optional[Int] = Unset): TeletypeBuffer = TeletypeBuffer(size)

  val empty: Teletype = Teletype(t"")
  given joinable: Teletype is Joinable = _.fold(empty)(_ + _)
  given printable: Teletype is Printable = _.render(_)

  given cuttable: Teletype is Cuttable by Text = (text, delimiter, limit) =>
    import java.util.regex.*
    val pattern = Pattern.compile(t"(.*)${Pattern.quote(delimiter.s).nn}(.*)".s).nn

    @tailrec
    def recur(source: Teletype, limit: Int, acc: List[Teletype]): List[Teletype] =
      if limit <= 0 then acc
      else
        val matcher = pattern.matcher(source.plain.s).nn
        if matcher.matches
        then
          val output = source.keep(matcher.group(2).nn.length, Rtl)
          recur(source.keep(matcher.group(1).nn.length), limit - 1, output :: acc)
        else source :: acc

    recur(text, limit, Nil)

  given Ordering[Teletype] = Ordering.by(_.plain)

  def make[ValueType: Showable](value: ValueType, transform: Ansi.Transform): Teletype =
    val text: Text = value.show
    Teletype(text, TreeMap(CharSpan(0, text.s.length) -> transform))

case class Teletype
   (plain:      Text,
    spans:      TreeMap[CharSpan, Ansi.Transform] = TreeMap(),
    insertions: TreeMap[Int, Text] = TreeMap()):

  def explicit: Text = render(termcapDefinitions.xtermTrueColor).flatMap: char =>
    if char.toInt == 27 then t"\\e" else char.show

  @targetName("add")
  def append(text: Text): Teletype = Teletype(t"$plain$text", spans)

  @targetName("add2")
  def append(text: Teletype): Teletype =
    val newSpans: TreeMap[CharSpan, Ansi.Transform] = text.spans.map:
      case (span, transform) => (span.shift(plain.length): CharSpan) -> transform

    Teletype(plain+text.plain, spans ++ newSpans)

  def dropChars(n: Int, dir: Bidi = Ltr): Teletype = dir match
    case Rtl =>
      takeChars(plain.length - n)

    case Ltr =>
      val newSpans: TreeMap[CharSpan, Ansi.Transform] =
        spans.map:
          case (span, transform) =>
            val charSpan: CharSpan = span.trimLeft(n)
            charSpan -> transform

        . view.filterKeys { k => k.isEmpty || k != CharSpan.Nowhere }.to(TreeMap)

      Teletype(plain.skip(n), newSpans)

  def takeChars(n: Int, dir: Bidi = Ltr): Teletype = dir match
    case Rtl =>
      dropChars(plain.length - n)

    case Ltr =>
      val newSpans: TreeMap[CharSpan, Ansi.Transform] =
        spans.map:
          case (span, tf) =>
            val charSpan: CharSpan = span.takeLeft(n)
            charSpan -> tf

        . view.filterKeys { k => k.isEmpty || k != CharSpan.Nowhere }.to(TreeMap)

      Teletype(plain.keep(n), newSpans)

  def render(termcap: Termcap): Text =
    val buf = StringBuilder()

    @tailrec
    def recur
       (spans:      TreeMap[CharSpan, Ansi.Transform],
        pos:        Int                               = 0,
        style:      TextStyle                         = TextStyle(),
        stack:      List[(CharSpan, TextStyle)]       = Nil,
        insertions: TreeMap[Int, Text]                = TreeMap())
            : Text =

      inline def addSpan(): Text =
        val newInsertions = addText(pos, spans.head(0).start, insertions)
        val newStyle = spans.head(1)(style)
        style.addChanges(buf, newStyle, termcap.color)
        val newStack = if spans.head(0).isEmpty then stack else (spans.head(0) -> style) :: stack
        recur(spans.tail, spans.head(0).start, newStyle, newStack, newInsertions)

      @tailrec
      def addText(from: Int, to: Int, insertions: TreeMap[Int, Text]): TreeMap[Int, Text] =
        if insertions.isEmpty then
          buf.add(plain.segment(from.max(0).z ~ Ordinal.natural(to.max(0))))
          insertions
        else if insertions.head(0) < to then
          buf.add(plain.segment(pos.z ~ Ordinal.natural(insertions.head(0))))
          buf.add(insertions.head(1))
          addText(insertions.head(0), to, insertions.tail)
        else
          buf.add(plain.segment(from.z ~ Ordinal.natural(to)))
          insertions

      if stack.isEmpty then
        if spans.isEmpty then
          val remaining = addText(pos, plain.length, insertions)
          remaining.values.each(buf.add(_))
          buf.text
        else addSpan()
      else
        if spans.isEmpty || stack.head(0).end <= spans.head(0).start then
          val newInsertions = addText(pos, stack.head(0).end, insertions)
          val newStyle = stack.head(1)
          style.addChanges(buf, newStyle, termcap.color)
          recur(spans, stack.head(0).end, newStyle, stack.tail, newInsertions)
        else addSpan()

    if termcap.ansi then recur(spans, insertions = insertions) else plain
