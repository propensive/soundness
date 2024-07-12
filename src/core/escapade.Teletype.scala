/*
    Escapade, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import gossamer.*
import rudiments.*
import anticipation.*
import symbolism.*
import turbulence.*
import contextual.*
import spectacular.*

import scala.util.*

import language.experimental.pureFunctions

object Teletype:
  given (using NotGiven[Teletype is Textual]) => Teletype is Addable as add:
    type Operand = Teletype
    type Result = Teletype
    inline def add(left: Teletype, right: Teletype): Teletype = left.append(right)

  given (using Stdio) => SimpleAppendable[Out.type, Teletype] as appendableOut =
    (_, output) => Out.print(output)

  given (using Stdio) => SimpleAppendable[Err.type, Teletype] as appendableErr =
    (_, output) => Err.print(output)

  given Teletype is Textual:
    type Show[ValueType] = ValueType is Teletypeable
    def classTag: ClassTag[Teletype] = summon[ClassTag[Teletype]]
    def text(teletype: Teletype): Text = teletype.plain
    def length(text: Teletype): Int = text.plain.s.length
    def apply(text: Text): Teletype = Teletype(text)

    def map(text: Teletype, lambda: Char => Char): Teletype =
      Teletype(Text(text.plain.s.map(lambda)), text.spans, text.insertions)

    def range(text: Teletype, start: Int, end: Int): Teletype = text.dropChars(start).takeChars(end - start)
    val empty: Teletype = Teletype.empty
    def concat(left: Teletype, right: Teletype): Teletype = left.append(right)
    def unsafeChar(text: Teletype, index: Int): Char = text.plain.s.charAt(index)
    def indexOf(text: Teletype, sub: Text): Int = text.plain.s.indexOf(sub.s)
    def show[ValueType: Teletypeable](value: ValueType) = value.teletype

  val empty: Teletype = Teletype(t"")
  given Teletype is Joinable as joinable = _.fold(empty)(_ + _)
  given Teletype is Printable as printable = _.render(_)

  given cuttable: Cuttable[Teletype, Text] = (text, delimiter, limit) =>
    import java.util.regex.*
    val pattern = Pattern.compile(t"(.*)${Pattern.quote(delimiter.s).nn}(.*)".s).nn

    @tailrec
    def recur(source: Teletype, limit: Int, acc: List[Teletype]): List[Teletype] =
      if limit <= 0 then acc
      else
        val matcher = pattern.matcher(source.plain.s).nn
        if matcher.matches
        then
          val output = source.take(matcher.group(2).nn.length, Rtl)
          recur(source.take(matcher.group(1).nn.length), limit - 1, output :: acc)
        else source :: acc

    IArray.from(recur(text, limit, Nil))

  given Ordering[Teletype] = Ordering.by(_.plain)

  def make[ValueType: Showable](value: ValueType, transform: Ansi.Transform): Teletype =
    val text: Text = value.show
    Teletype(text, TreeMap(CharSpan(0, text.s.length) -> transform))

case class Teletype
    (plain: Text, spans: TreeMap[CharSpan, Ansi.Transform] = TreeMap(),
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
        .view.filterKeys { k => k.isEmpty || k != CharSpan.Nowhere }.to(TreeMap)

      Teletype(plain.drop(n), newSpans)

  def takeChars(n: Int, dir: Bidi = Ltr): Teletype = dir match
    case Rtl =>
      dropChars(plain.length - n)

    case Ltr =>
      val newSpans: TreeMap[CharSpan, Ansi.Transform] =
        spans.map:
          case (span, tf) =>
            val charSpan: CharSpan = span.takeLeft(n)
            charSpan -> tf
        .view.filterKeys { k => k.isEmpty || k != CharSpan.Nowhere }.to(TreeMap)

      Teletype(plain.take(n), newSpans)

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
          buf.add(plain.slice(from, to))
          insertions
        else if insertions.head(0) < to then
          buf.add(plain.slice(pos, insertions.head(0)))
          buf.add(insertions.head(1))
          addText(insertions.head(0), to, insertions.tail)
        else
          buf.add(plain.slice(from, to))
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
