/*
    Escapade, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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
import digression.*
import iridescence.*
import contextual.*
import spectacular.*

opaque type CharSpan = Long

object CharSpan:
  def apply(start: Int, end: Int): CharSpan = (start.toLong << 32) + (Int.MaxValue - end)
  given Ordering[CharSpan] = Ordering.Long.on[CharSpan](identity(_))
  val Nowhere: CharSpan = CharSpan(Int.MaxValue, Int.MaxValue)

extension (span: CharSpan)
  def start: Int = (span >> 32).toInt
  def end: Int = Int.MaxValue - span.toInt
  def isEmpty: Boolean = start == end
  
  def trimLeft(n: Int): CharSpan = 
    if n >= end then CharSpan.Nowhere else if n <= start then CharSpan(start - n, end - n)
    else CharSpan(0, end - n)
  
  def takeLeft(n: Int): CharSpan =
    if n <= start then CharSpan.Nowhere else if n >= end then span else CharSpan(start, n)

  def shift(n: Int): CharSpan = CharSpan(start + n, end + n)

object TextStyle:
  val esc: Char = 27.toChar

case class TextStyle
    (fg: Maybe[Rgb24] = Unset, bg: Maybe[Rgb24] = Unset, italic: Boolean = false,
        bold: Boolean = false, reverse: Boolean = false, underline: Boolean = false,
        conceal: Boolean = false, strike: Boolean = false):
  import escapes.*
  import TextStyle.esc
  
  private def italicEsc: Text = if italic then styles.Italic.on else styles.Italic.off
  private def boldEsc: Text = if bold then styles.Bold.on else styles.Bold.off
  private def reverseEsc: Text = if reverse then styles.Reverse.on else styles.Reverse.off
  private def underlineEsc: Text = if underline then styles.Underline.on else styles.Underline.off
  private def concealEsc: Text = if conceal then styles.Conceal.on else styles.Conceal.off
  private def strikeEsc: Text = if strike then styles.Strike.on else styles.Strike.off
  
  def addChanges(buf: StringBuilder, next: TextStyle): Unit =
    if fg != next.fg then buf.add(next.fg.mm(_.ansiFg).or(t"$esc[39m"))
    if bg != next.bg then buf.add(next.bg.mm(_.ansiBg).or(t"$esc[49m"))
    if italic != next.italic then buf.add(t"${esc}${next.italicEsc}")
    if bold != next.bold then buf.add(t"${esc}${next.boldEsc}")
    if reverse != next.reverse then buf.add(t"${esc}${next.reverseEsc}")
    if underline != next.underline then buf.add(t"${esc}${next.underlineEsc}")
    if conceal != next.conceal then buf.add(t"${esc}${next.concealEsc}")
    if strike != next.strike then buf.add(t"${esc}${next.strikeEsc}")

object rendering:
  given plain: Show[AnsiText] = _.plain
  given ansi: Show[AnsiText] = _.render
  
object Stylize:
  def apply(fn: TextStyle => TextStyle): Ansi.Input.Markup = Ansi.Input.Markup(fn)

object Ansi:
  type Transform = TextStyle => TextStyle

  def strip(txt: Text): Text = txt.sub(t"""\e\\[?.*?[\\@-~]""", t"")

  given Substitution[Ansi.Input, Text, "t"] = str => Ansi.Input.Textual(AnsiText(str))
  given Substitution[Ansi.Input, String, "t"] = str => Ansi.Input.Textual(AnsiText(Text(str)))
  
  given [ValueType](using Show[ValueType]): Substitution[Ansi.Input, ValueType, "t"] =
    value => Ansi.Input.Textual(AnsiText(value.show))
  
  given [ValueType: Display]: Substitution[Ansi.Input, ValueType, "t"] =
    value => Ansi.Input.Textual(summon[Display[ValueType]].ansi(value))
  
  given Stylize[Escape] = identity(_)
  given Stylize[Color] = color => Stylize(_.copy(fg = color.standardSrgb.rgb24))
  given Stylize[Rgb24] = color => Stylize(_.copy(fg = color))
  given Stylize[Bg] = bgColor => Stylize(_.copy(bg = bgColor.color))

  given Stylize[Bold.type] = _ => Stylize(_.copy(bold = true))
  given Stylize[Italic.type] = _ => Stylize(_.copy(italic = true))
  given Stylize[Underline.type] = _ => Stylize(_.copy(underline = true))
  given Stylize[Strike.type] = _ => Stylize(_.copy(strike = true))
  given Stylize[Conceal.type] = _ => Stylize(_.copy(conceal = true))
  given Stylize[Reverse.type] = _ => Stylize(_.copy(reverse = true))
  
  enum Input:
    case Textual(text: AnsiText)
    case Markup(transform: Transform)
    case Escape(on: Text, off: Text)

  case class Frame(bracket: Char, start: Int, transform: Transform)
  
  case class State
      (text: Text = t"", last: Option[Transform] = None, stack: List[Frame] = Nil,
          spans: TreeMap[CharSpan, Transform] = TreeMap(),
          insertions: TreeMap[Int, Text] = TreeMap()):

    def add(span: CharSpan, transform: Transform): State =
      copy(spans = spans.updated(span, spans.get(span).fold(transform)(transform.andThen(_))))
    
    def add(pos: Int, esc: Escape): State =
      val insertions2 = insertions.get(pos).fold(t"\e"+esc.on)(_+t"\e"+esc.on)
      copy(insertions = insertions.updated(pos, insertions2))

  object Interpolator extends contextual.Interpolator[Input, State, AnsiText]:
    private val complement = Map('[' -> ']', '(' -> ')', '{' -> '}', '<' -> '>', '«' -> '»')
    def initial: State = State()

    def parse(state: State, text: Text): State =
      state.last.fold(closures(state, text)): transform =>
        safely(text(0)) match
          case '\\' =>
            closures(state.copy(last = None), text.drop(1))
          case '[' | '(' | '<' | '«' | '{' =>
            val frame = Frame(unsafely(complement(text(0))), state.text.length, transform)
            closures(state.copy(stack = frame :: state.stack, last = None), text.drop(1))
  
          case _ =>
            val state2 = state.add(CharSpan(state.text.length, state.text.length), transform)
            closures(state2.copy(last = None), text)

    private def closures(state: State, text: Text): State =
      state.stack.headOption.fold(state.copy(text = state.text+Interpolation.escape(text))):
        frame =>
          safely(text.where(_ == frame.bracket)) match
            case Unset =>
              state.copy(text = state.text+text)
            
            case idx: Int =>
              val text2 = state.text+text.take(idx)
              val span2: CharSpan = CharSpan(frame.start, state.text.length + idx)
              val state2: State = state.add(span2, frame.transform)
              val state3: State = state2.copy(text = text2, last = None, stack = state.stack.tail)
              closures(state3, text.drop(idx + 1))

    def insert(state: State, value: Input): State = value match
      case Input.Textual(text) =>
        val textSpans: TreeMap[CharSpan, Transform] = text.spans.map:
          case (span, transform) => (span.shift(state.text.length): CharSpan) -> transform

        val textInsertions: TreeMap[Int, Text] = text.insertions.map:
          case (pos, ins) => (pos + state.text.length) -> ins

        state.copy(text = state.text+text.plain, last = None, spans = state.spans ++ textSpans,
            insertions = state.insertions ++ textInsertions)
      
      case Input.Markup(transform) =>
        state.copy(last = Some(transform))
    
      case esc@Input.Escape(on, off) =>
        state.copy(last = None).add(state.text.length, esc)
    
    def skip(state: State): State = insert(state, Input.Textual(AnsiText.empty))
    
    def complete(state: State): AnsiText =
      if !state.stack.isEmpty then throw InterpolationError(t"mismatched closing brace")

      AnsiText(state.text, state.spans, state.insertions)

object AnsiText:

  given Show[AnsiText] = _.plain

  given textual: Textual[AnsiText] with
    type ShowType[-ValueType] = Display[ValueType]
    def string(text: AnsiText): String = text.plain.s
    def length(text: AnsiText): Int = text.plain.s.length
    def make(string: String): AnsiText = AnsiText(Text(string))
    
    def map(text: AnsiText, fn: Char => Char): AnsiText =
      AnsiText(Text(text.plain.s.map(fn)), text.spans, text.insertions)

    def slice(text: AnsiText, start: Int, end: Int): AnsiText =
      text.dropChars(start).takeChars(end - start)
    
    def empty: AnsiText = AnsiText.empty
    def concat(left: AnsiText, right: AnsiText): AnsiText = left+right
    def unsafeChar(text: AnsiText, index: Int): Char = text.plain.s.charAt(index)
    def indexOf(text: AnsiText, sub: Text): Int = text.plain.s.indexOf(sub.s)
    
    def show[ValueType](value: ValueType)(using ansiShow: Display[ValueType]) =
      ansiShow.ansi(value)

  def empty: AnsiText = AnsiText(t"")
  given joinable: Joinable[AnsiText] = _.fold(empty)(_ + _)

  given printable: Printable[AnsiText] = _.render

  given cuttable: Cuttable[AnsiText, Text] = (text, delimiter, limit) =>
    import java.util.regex.*
    val pattern = Pattern.compile(t"(.*)${Pattern.quote(delimiter.s).nn}(.*)".s).nn
    
    @tailrec
    def recur(source: AnsiText, limit: Int, acc: List[AnsiText]): List[AnsiText] =
      if limit <= 0 then acc
      else
        val matcher = pattern.matcher(source.plain.s).nn
        if matcher.matches
        then
          val ansiText = source.take(matcher.group(2).nn.length, Rtl)
          recur(source.take(matcher.group(1).nn.length), limit - 1, ansiText :: acc)
        else source :: acc



    recur(text, limit, Nil)

  given Ordering[AnsiText] = Ordering.by(_.plain)

  def make
      [ValueType]
      (value: ValueType, transform: Ansi.Transform)(using Show[ValueType])
      : AnsiText =
    val text: Text = value.show
    AnsiText(text, TreeMap(CharSpan(0, text.s.length) -> transform))

case class AnsiText(plain: Text, spans: TreeMap[CharSpan, Ansi.Transform] = TreeMap(),
                        insertions: TreeMap[Int, Text] = TreeMap()):
  def explicit: Text = render.flatMap { ch => if ch.toInt == 27 then t"\\e" else ch.show }

  @targetName("add")
  infix def +(text: Text): AnsiText = AnsiText(t"$plain$text", spans)

  @targetName("add2")
  infix def +(text: AnsiText): AnsiText =
    val newSpans: TreeMap[CharSpan, Ansi.Transform] = text.spans.map:
      case (span, transform) => (span.shift(plain.length): CharSpan) -> transform
    
    AnsiText(plain+text.plain, spans ++ newSpans)

  def dropChars(n: Int, dir: Bidi = Ltr): AnsiText = dir match
    case Rtl =>
      takeChars(plain.length - n)
    
    case Ltr =>
      val newSpans: TreeMap[CharSpan, Ansi.Transform] =
        spans.map:
          case (span, transform) =>
            val charSpan: CharSpan = span.trimLeft(n)
            charSpan -> transform
        .view.filterKeys { k => k.isEmpty || k != CharSpan.Nowhere }.to(TreeMap)
      
      AnsiText(plain.drop(n), newSpans)

  def takeChars(n: Int, dir: Bidi = Ltr): AnsiText = dir match
    case Rtl =>
      dropChars(plain.length - n)

    case Ltr =>
      val newSpans: TreeMap[CharSpan, Ansi.Transform] =
        spans.map:
          case (span, tf) =>
            val charSpan: CharSpan = span.takeLeft(n)
            charSpan -> tf
        .view.filterKeys { k => k.isEmpty || k != CharSpan.Nowhere }.to(TreeMap)
      
      AnsiText(plain.take(n), newSpans)

  def render: Text =
    val buf = StringBuilder()

    @tailrec
    def recur
        (spans: TreeMap[CharSpan, Ansi.Transform], pos: Int = 0, style: TextStyle = TextStyle(),
            stack: List[(CharSpan, TextStyle)] = Nil, insertions: TreeMap[Int, Text] = TreeMap())
        : Text =

      inline def addSpan(): Text =
        val newInsertions = addText(pos, spans.head(0).start, insertions)
        val newStyle = spans.head(1)(style)
        style.addChanges(buf, newStyle)
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
          remaining.values.foreach(buf.add(_))
          buf.text
        else addSpan()
      else
        if spans.isEmpty || stack.head(0).end <= spans.head(0).start then
          val newInsertions = addText(pos, stack.head(0).end, insertions)
          val newStyle = stack.head(1)
          style.addChanges(buf, newStyle)
          recur(spans, stack.head(0).end, newStyle, stack.tail, newInsertions)
        else addSpan()

    recur(spans, insertions = insertions)

object Bold
object Italic
object Underline
object Strike
object Reverse
object Conceal

object Bg:
  def apply(color: Color): Bg = Bg(color.standardSrgb.rgb24)

case class Bg(color: Rgb24)

type Stylize[T] = Substitution[Ansi.Input, T, "esc"]
