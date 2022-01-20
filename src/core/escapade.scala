/*
    Escapade, version 0.4.0. Copyright 2021-22 Jon Pretty, Propensive OÜ.

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

import contextual.*
import iridescence.*
import rudiments.*
import gossamer.*

import java.text.*

object AnsiString:
  def empty: AnsiString = AnsiString(t"")

  given Joinable[AnsiString] = _.fold(empty)(_ + _)
    
  def make[T: Show](value: T, transform: Ansi.Transform): AnsiString =
    val str: Text = value.show
    AnsiString(str, TreeMap(Span(0, str.length) -> transform))

object Span:
  def apply(start: Int, end: Int): Span = (start.toLong << 32) + end
  given Ordering[Span] = Ordering.Long.on[Span](identity(_))

opaque type Span = Long

extension (span: Span)
  def start: Int = (span >> 32).toInt
  def end: Int = span.toInt
  def isEmpty: Boolean = start == end
  def drop(n: Int): Span = Span((start - n) max 0, (end - n) max 0)
  def take(n: Int): Span = Span(start min n, end min n)
  def +(n: Int): Span = Span(start + n, end + n)

object rendering:
  given plain: Show[AnsiString] = _.plain
  given ansi: Show[AnsiString] = _.render
  
case class AnsiString(string: Text, escapes: TreeMap[Span, Ansi.Transform] = TreeMap()):
  def length: Int = string.length
  def span(n: Int): AnsiString = take(n).padTo(n)
  def plain: Text = string
  def explicit: Text = render.flatMap { ch => if ch.toInt == 27 then t"\\e" else ch.show }
  def upper: AnsiString = AnsiString(string.upper, escapes)
  def lower: AnsiString = AnsiString(string.lower, escapes)

  def drop(n: Int): AnsiString =
    val newEscapes: TreeMap[Span, Ansi.Transform] =
      escapes.map:
        case (span, transform) => (span.drop(n): Span, transform)
      .filter(!_(0).isEmpty)
    
    AnsiString(string.drop(n), newEscapes)

  def take(n: Int): AnsiString =
    val newEscapes: TreeMap[Span, Ansi.Transform] =
      escapes.map:
        case (span, transform) => (span.take(n): Span, transform)
      .filter(!_(0).isEmpty)
    
    AnsiString(string.take(n), newEscapes)

  def padTo(n: Int, char: Char = ' ') =
    if length < n then this + AnsiString(char.show*(n - length)) else this

  def cut(delim: Text): List[AnsiString] =
    val parts = plain.cut(delim)
    parts.zipWithIndex.map:
      case (part, idx) =>
        drop(parts.take(idx).map(_.length).sum + idx*delim.length).take(part.length)

  @targetName("times")
  def *(n: Int): AnsiString = if n == 0 then AnsiString.empty else this*(n - 1)+this

  def render: Text =
    val buf = StringBuilder()
    
    def recur(spans: TreeMap[Span, Ansi.Transform], pos: Int = 0, style: TextStyle = TextStyle(),
                  stack: List[(Span, TextStyle)] = Nil): Text =
      if spans.isEmpty then
        buf.add(string.slice(pos, string.length))
        buf.text
      else if stack.isEmpty || spans.head(0).start <= stack.head(0).end then
        val newStyle = spans.head(1)(style)
        buf.add(style.changes(newStyle))
        buf.add(string.slice(pos, spans.head(0).start))
        recur(spans.tail, spans.head(0).start, newStyle, spans.head(0) -> style :: stack)
      else
        buf.add(style.changes(stack.head(1)))
        buf.add(string.slice(pos, stack.head(0).end))
        recur(spans, stack.head(0).end, stack.head(1), stack.tail)

    recur(escapes)

  private def shift(n: Int): TreeMap[Span, Ansi.Transform] =
    escapes.map { (s, v) => (s + n: Span) -> v }

  @targetName("add")
  infix def +(str: Text): AnsiString = AnsiString(t"$string$str", escapes)
  //def addEsc(esc: Ansi.Change): AnsiString = addEsc(string.length, esc)
  
  def addEsc(pos: Int, esc: Ansi.Change): AnsiString =
    AnsiString(string, escapes.updated(span,
        escapes.get(span).fold(transform)(_.andThen(transform))))
  
  @targetName("add")
  infix def +(other: AnsiString): AnsiString =
    AnsiString(t"$string${other.string}", escapes ++ other.shift(length))

type Stylize[T] = Substitution[Ansi.Input, T, "esc"]

object Stylize:
  def apply(fn: TextStyle => TextStyle): Ansi.Input.Apply = Ansi.Input.Apply(fn)

object Ansi:
  type Transform = Ansi.Change => Ansi.Change
  def strip(txt: Text): Text = txt.sub(t"""\e\\[?.*?[\\@-~]""", t"")

  given Substitution[Ansi.Input, Text, "t"] = str => Ansi.Input.Str(AnsiString(str))
  given Substitution[Ansi.Input, String, "t"] = str => Ansi.Input.Str(AnsiString(Text(str)))
  
  given [T: Show]: Substitution[Ansi.Input, T, "t"] =
    value => Ansi.Input.Str(AnsiString(summon[Show[T]].show(value)))
  
  given Stylize[Escape] = identity(_)
  given Stylize[Color] = color => Stylize(_.copy(fg = color.standardSrgb))
  given Stylize[Bg] =
    bgColor => Stylize(_.copy(bg = Some(bgColor.color.standardSrgb)))
  
  given [T: AnsiShow]: Substitution[Ansi.Input, T, "t"] =
    value => Ansi.Input.Str(summon[AnsiShow[T]].ansiShow(value))

  given Stylize[Bold.type] = _ => Stylize(_.copy(bold = true))
  given Stylize[Italic.type] = _ => Stylize(_.copy(italic = true))
  given Stylize[Underline.type] = _ => Stylize(_.copy(underline = true))
  given Stylize[Strike.type] = _ => Stylize(_.copy(strike = true))
  given Stylize[Conceal.type] = _ => Stylize(_.copy(conceal = true))
  given Stylize[Reverse.type] = _ => Stylize(_.copy(reverse = true))

  enum Input:
    case Str(string: AnsiString)
    case Esc(on: Text, off: Text)
    case Apply(color: TextStyle => TextStyle)

  case class State(string: AnsiString, last: Option[Transform], stack: List[(Char, Transform)]):
    def add(str: Text): State = copy(string = string + str, last = None)
    def addEsc(esc: Ansi.Change): State = copy(string = string.addEsc(esc), last = None)
    def addEsc(pos: Int, esc: Ansi.Change): State = copy(string = string.addEsc(pos, esc), last = None)
    def isEmpty: Boolean =
      string.string == t"" && string.escapes.isEmpty && last.isEmpty && stack.isEmpty

  object Interpolator extends contextual.Interpolator[Input, State, AnsiString]:
    //erased given CanThrow[OutOfRangeError] = compiletime.erasedValue
    def initial: State = State(AnsiString(t""), None, Nil)

    private def closures(state: State, str: Text): State =
      if state.stack.isEmpty then state.add(str)
      else
        str.indexOf(state.stack.head(0)) match
          case -1 =>
            state.add(str)
          
          case i =>
            val newState = state.copy(stack = state.stack.tail).add(str.show.slice(0, i).s).addEsc(state.stack.head(1))
            closures(newState, str.show.slice(i + 1, str.length).s)


    private def complement(ch: '[' | '(' | '{' | '<' | '«'): ']' | ')' | '}' | '>' | '»' = ch match
      case '[' => ']'
      case '(' => ')'
      case '{' => '}'
      case '<' => '>'
      case '«' => '»'

    def parse(state: State, string: Text): State =
      if state.isEmpty then State(AnsiString(string.show), None, Nil)
      else state.last match
        case None =>
          closures(state, string)
        
        case Some(last) =>
          if string.length == 0 then closures(state, string)
          else string(0) match
            case '\\' =>
              closures(state, string.drop(1))

            case ch@('[' | '(' | '{' | '<' | '«') =>
              closures(state.copy(last = None, stack = (complement(ch), last) :: state.stack),
                  string.drop(1))
            
            case _ =>
              closures(state, string)

    def insert(state: State, value: Input): State =
      value match
        case Input.Str(string) =>
          State(state.string + string, None, state.stack)

        case Input.Apply(fn) =>
          state.addEsc(Change.Push(fn)).copy(last = Some(Change.Pop))
        
        case Input.Esc(start, end) =>
          state.addEsc(Change.Literal(start)).copy(last = Some(Change.Literal(end)))

    def skip(state: State): State = insert(state, Input.Str(AnsiString.empty))

    override def substitute(state: State, value: Text): State =
      
      val dummy = value match
        case t"esc" => Ansi.Input.Esc(t"[0m", t"")
        case _      => Ansi.Input.Str(AnsiString.empty)
      
      insert(state, dummy)

    def complete(state: State): AnsiString =
      if !state.stack.isEmpty then throw InterpolationError(t"mismatched closing brace")

      state.string

case class Bg(color: Color)

case class TextStyle(fg: Srgb = colors.White, bg: Option[Srgb] = None, italic: Boolean = false,
                         bold: Boolean = false, reverse: Boolean = false, underline: Boolean = false,
                         conceal: Boolean = false, strike: Boolean = false):

  import escapes.*
  
  val esc = 27.toChar
  
  private def italicEsc: Text = if italic then styles.Italic.on else styles.Italic.off
  private def boldEsc: Text = if bold then styles.Bold.on else styles.Bold.off
  private def reverseEsc: Text = if reverse then styles.Reverse.on else styles.Reverse.off
  private def underlineEsc: Text = if underline then styles.Underline.on else styles.Underline.off
  private def concealEsc: Text = if conceal then styles.Conceal.on else styles.Conceal.off
  private def strikeEsc: Text = if strike then styles.Strike.on else styles.Strike.off
  
  def changes(next: TextStyle): Text = List(
    if fg != next.fg then next.fg.ansiFg24 else t"",
    if bg != next.bg then next.bg.map(_.ansiBg24).getOrElse(t"$esc[49m") else t"",
    if italic != next.italic then t"${esc}${next.italicEsc}" else t"",
    if bold != next.bold then t"${esc}${next.boldEsc}" else t"",
    if reverse != next.reverse then t"${esc}${next.reverseEsc}" else t"",
    if underline != next.underline then t"${esc}${next.underlineEsc}" else t"",
    if conceal != next.conceal then t"${esc}${next.concealEsc}" else t"",
    if strike != next.strike then t"${esc}${next.strikeEsc}" else t""
  ).join

object Bold
object Italic
object Underline
object Strike
object Reverse
object Conceal
