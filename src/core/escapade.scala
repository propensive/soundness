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
    
  def apply[T: Show](value: T, wrapper: TextStyle => TextStyle): AnsiString =
    val str: Text = value.show

    AnsiString(str, TreeMap(
      0          -> List(Ansi.Change.Push(wrapper)),
      str.length -> List(Ansi.Change.Pop)
    ))

object rendering:
  given plain: Show[AnsiString] = _.plain
  given ansi: Show[AnsiString] = _.render

case class AnsiString(string: Text, escapes: TreeMap[Int, List[Ansi.Change]] = TreeMap()):
  def length: Int = string.length

  def drop(n: Int): AnsiString =
    val pushes = escapes.filter(_(0) < n).flatMap(_(1)).foldLeft(List[Ansi.Change]()):
      case (stack, Ansi.Change.Push(fn))   => Ansi.Change.Push(fn) :: stack
      case (head :: tail, Ansi.Change.Pop) => tail
      case (stack, _)                      => stack
    .reverse

    val init = escapes.getOrElse(n, Nil) ++ pushes

    AnsiString(string.drop(n), escapes.collect:
      case (i, e) if i >= n => (i - n, e)
    .updated(0, init))

  def padTo(n: Int, char: Char = ' ') =
    if length < n then this + AnsiString(char.show*(n - length)) else this

  def span(n: Int): AnsiString = take(n).padTo(n)

  def take(n: Int): AnsiString =
    val pops = List.fill(0.max(escapes.filter(_(0) > n).flatMap(_(1)).foldLeft(0):
      case (count, Ansi.Change.Push(_))    => count - 1
      case (count, Ansi.Change.Pop)        => count + 1
      case (count, Ansi.Change.Literal(_)) => count
    ))(Ansi.Change.Pop)
    
    val newEscapes = escapes.filter(_(0) <= n)
    
    AnsiString(string.take(n), newEscapes.updated(n, escapes.getOrElse(n, Nil) ++ pops))

  def cut(delim: Text): List[AnsiString] =
    val parts = plain.cut(delim)
    parts.zipWithIndex.map:
      case (part, idx) =>
        drop(parts.take(idx).map(_.length).sum + idx*delim.length).take(part.length)

  def plain: Text = string
  
  def explicit: Text = render.flatMap:
    ch => if ch.toInt == 27 then t"\\e" else ch.show
  
  def upper: AnsiString = AnsiString(string.upper, escapes)
  def lower: AnsiString = AnsiString(string.lower, escapes)

  @targetName("times")
  def *(n: Int): AnsiString = if n == 0 then AnsiString.empty else this*(n - 1)+this

  def render: Text =
    val buf = StringBuilder()
    
    def build(treeMap: TreeMap[Int, List[Ansi.Change]], pos: Int = 0, stack: List[TextStyle] = Nil): Text =

      if treeMap.isEmpty then
        buf.add(string.slice(pos, string.length))
        buf.text
      else
        buf.add(string.show.slice(pos, treeMap.head(0)))
        
        val newStack = treeMap.head(1).sortBy(_ != Ansi.Change.Pop).foldLeft(stack):
          case (head :: tail, Ansi.Change.Pop) =>
            val next = tail.headOption.getOrElse(TextStyle())
            buf.add(head.changes(next))
            tail
          
          case (Nil, Ansi.Change.Pop) =>
            Nil
          
          case (stack, Ansi.Change.Push(fn)) =>
            val currentStyle = stack.headOption.getOrElse(TextStyle())
            val next = fn(currentStyle)
            buf.add(currentStyle.changes(next))
            next :: stack
          
          case (stack, Ansi.Change.Literal(str)) =>
            buf.add(27.toChar)
            buf.add(str)
            stack
        
        build(treeMap.tail, treeMap.head(0), newStack)
    
    build(escapes)

  private def shift(n: Int): TreeMap[Int, List[Ansi.Change]] =
    escapes.map:
      (k, v) => (k + n, v)
    .to(TreeMap)

  @targetName("add")
  infix def +(str: Text): AnsiString = AnsiString(t"$string$str", escapes)
  def addEsc(esc: Ansi.Change): AnsiString = addEsc(string.length, esc)
  
  def addEsc(pos: Int, esc: Ansi.Change): AnsiString =
    AnsiString(string, escapes.updated(string.length, escapes.get(string.length).getOrElse(Nil) :+ esc))
  
  @targetName("add")
  infix def +(ansi: AnsiString): AnsiString =
    AnsiString(t"$string${ansi.string}", escapes ++ ansi.shift(length))

type Stylize[T] = Substitution[Ansi.Input, T, "esc"]

object Stylize:
  def apply(fn: TextStyle => TextStyle): Ansi.Input.Apply = Ansi.Input.Apply(fn)

object Ansi:
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

  enum Change:
     case Push(stateChange: TextStyle => TextStyle)
     case Pop
     case Literal(str: Text)

  case class State(string: AnsiString, last: Option[Ansi.Change], stack: List[(Char, Ansi.Change)]):
    def add(str: Text): State = copy(string = string + str, last = None)
    def addEsc(esc: Ansi.Change): State = copy(string = string.addEsc(esc), last = None)
    def addEsc(pos: Int, esc: Ansi.Change): State = copy(string = string.addEsc(pos, esc), last = None)
    def isEmpty: Boolean =
      string.string == t"" && string.escapes.isEmpty && last.isEmpty && stack.isEmpty

  object Interpolator extends contextual.Interpolator[Input, State, AnsiString]:
    erased given CanThrow[OutOfRangeError] = compiletime.erasedValue
    def initial: State = State(AnsiString(t""), None, Nil)

    private def closures(state: State, str: Text): State =
      if state.stack.isEmpty then state.add(str)
      else
        try
          val i = str.where(_ == state.stack.head(0))
          val newState = state.copy(stack = state.stack.tail).add(str.take(i)).addEsc(state.stack.head(1))
          closures(newState, str.show.slice(i + 1, str.length))
        catch case err: OutOfRangeError => state.add(str)

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
