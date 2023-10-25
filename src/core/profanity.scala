/*
    Profanity, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package profanity

import rudiments.*
import fulminate.*
import gossamer.*
import spectacular.*
import eucalyptus.*
import perforate.*
import parasite.*
import turbulence.*
import anticipation.*, timeApi.long

import sun.misc.Signal
import java.io as ji

enum Keypress:
  case Printable(char: Char)
  case Function(number: Int)
  case Ctrl(char: Char)
  case EscapeSeq(id: Char, content: Char*)
  case Resize(rows: Int, columns: Int)
  case Enter, Escape, Tab, Backspace, Delete, PageUp, PageDown, LeftArrow, RightArrow, UpArrow,
      DownArrow, CtrlLeftArrow, CtrlRightArrow, CtrlUpArrow, CtrlDownArrow, End, Home, Insert

trait Keyboard[+KeyType]:
  def interpret(bytes: List[Byte]): LazyList[KeyType]

object StdKeyboard:
  def process(stream: LazyList[Char]): LazyList[Keypress] = stream match
    case '\u001b' #:: rest                    =>
      safely(supervise(Async(rest.head).await(10L))) match
        case Unset => Keypress.Escape #:: process(rest)
        case _ => rest match
          case 'O' #:: fn #:: rest                  => Keypress.Function(fn.toInt - 79) #:: process(rest)
          case '[' #:: rest                         => rest match
            case '3' #:: '~' #:: rest                 => Keypress.Delete #:: process(rest)
            case '2' #:: '~' #:: rest                 => Keypress.Insert #:: process(rest)
            case 'F' #:: rest                         => Keypress.End #:: process(rest)
            case 'H' #:: rest                         => Keypress.Home #:: process(rest)
            case '5' #:: '~' #:: rest                 => Keypress.PageUp #:: process(rest)
            case '6' #:: '~' #:: rest                 => Keypress.PageDown #:: process(rest)
            case 'A' #:: rest                         => Keypress.UpArrow #:: process(rest)
            case '1' #:: ';' #:: '5' #:: 'A' #:: rest => Keypress.CtrlUpArrow #:: process(rest)
            case 'B' #:: rest                         => Keypress.DownArrow #:: process(rest)
            case '1' #:: ';' #:: '5' #:: 'B' #:: rest => Keypress.CtrlDownArrow #:: process(rest)
            case 'C' #:: rest                         => Keypress.RightArrow #:: process(rest)
            case '1' #:: ';' #:: '5' #:: 'C' #:: rest => Keypress.CtrlRightArrow #:: process(rest)
            case 'D' #:: rest                         => Keypress.LeftArrow #:: process(rest)
            case '1' #:: ';' #:: '5' #:: 'D' #:: rest => Keypress.CtrlLeftArrow #:: process(rest)
            case other =>
              val sequence = other.takeWhile(!_.isLetter)
              val rest = other.drop(sequence.length)
              def continue = process(rest.tail)
              
              rest.head match
                case 'R'  => sequence.map(_.show).join.cut(';') match
                  case List(As[Int](rows), As[Int](cols)) => Keypress.Resize(rows, cols) #:: continue
                  case _                                  => Keypress.Resize(20, 30) #:: continue
                case char => Keypress.EscapeSeq(char, sequence*) #:: continue
          case rest => process(rest)
    case ('\b' | '\u007f') #:: rest           => Keypress.Backspace #:: process(rest)
    case '\u0009' #:: rest                    => Keypress.Tab #:: process(rest)
    case ('\u000a' | '\u000d') #:: rest       => Keypress.Enter #:: process(rest)
    case char #:: rest if char < 32           => Keypress.Ctrl(char) #:: process(rest)
    case other #:: rest                       => Keypress.Printable(other) #:: process(rest)
    case _                                    => LazyList()
    //case '\u001b' #:: rest => Keypress.Enter #:: process(rest)

case class TtyError(ttyMsg: Text) extends Error(msg"STDIN is not attached to a TTY: $ttyMsg")

case class Tty(out: ji.PrintStream, in: LazyList[Bytes])

object Tty:
  final val noopOut: ji.PrintStream = ji.PrintStream((_ => ()): ji.OutputStream)

  def capture[T](fn: (tty: Tty) ?=> T)(using Log, InputSource): T throws TtyError =
    val tty = summon[InputSource].init()
    Signal.handle(Signal("WINCH"), sig => reportSize()(using tty))
    try Console.withOut(noopOut)(fn(using tty)) finally summon[InputSource].cleanup(tty)

  def reportSize()(using Tty, Log): Unit =
    val esc = 27.toChar
    Log.fine(msg"Sent ANSI escape codes to TTY to attempt to get console dimensions")
    Tty.print(t"${esc}[s${esc}[4095C${esc}[4095B${esc}[6n${esc}[u")

  def stream[K](using Tty, Log, Keyboard[K], Monitor): LazyList[K] =
    summon[Tty].in.cluster(15L).flatMap: data =>
      unsafely(summon[Keyboard[K]].interpret(data.map(_.to(List)).flatten))

  def print(msg: Text)(using Tty) = summon[Tty].out.print(msg.s)
  def println(msg: Text)(using Tty) = summon[Tty].out.println(msg.s)

object Keyboard:
  given Keyboard[Int] with
    def interpret(bytes: List[Byte]): LazyList[Int] = bytes.map(_.toInt).to(LazyList)
  
  given Keyboard[List[Int]] with
    def interpret(bytes: List[Byte]): LazyList[List[Int]] = LazyList(bytes.map(_.toInt))
  
  private def readResize(bytes: List[Int]): Keypress.Resize =
    val size = String(bytes.map(_.toByte).init.to(Array)).show.cut(t";")
    val columns = size(0).toString.toInt
    val rows = size(1).toString.toInt
    
    Keypress.Resize(rows, columns)

  given Keyboard[Keypress] with
    def interpret(bytes: List[Byte]): LazyList[Keypress] =
      bytes.map(_.toInt).to(List) match
        case 9 :: Nil             => LazyList(Keypress.Tab)
        case 10 :: Nil            => LazyList(Keypress.Enter)
        case 27 :: Nil            => LazyList(Keypress.Escape)
        case 27 :: 79 :: i :: Nil => LazyList(Keypress.Function(i - 79))
        case 27 :: 91 :: tail     => LazyList(control(tail))
        case (127 | 8) :: Nil     => LazyList(Keypress.Backspace)
        case i :: Nil if i < 32   => LazyList(Keypress.Ctrl((i + 64).toChar))
        
        case other                => String(bytes.to(Array), 0, bytes.length)
                                       .toCharArray.nn
                                       .immutable(using Unsafe)
                                       .to(LazyList)
                                       .map(Keypress.Printable(_))
    
    private def control(bytes: List[Int]): Keypress = bytes match
      case List(51, 126)        => Keypress.Delete
      case List(50, 126)        => Keypress.Insert
      case List(70)             => Keypress.End
      case List(72)             => Keypress.Home
      case List(53, 126)        => Keypress.PageUp
      case List(54, 126)        => Keypress.PageDown
      case List(68)             => Keypress.LeftArrow
      case List(49, 59, 53, 68) => Keypress.CtrlLeftArrow
      case List(67)             => Keypress.RightArrow
      case List(49, 59, 53, 67) => Keypress.CtrlRightArrow
      case List(65)             => Keypress.UpArrow
      case List(49, 59, 53, 65) => Keypress.CtrlUpArrow
      case List(66)             => Keypress.DownArrow
      case List(49, 59, 53, 66) => Keypress.CtrlDownArrow
      case ks if ks.last == 82  => readResize(ks)
      case other                => Keypress.EscapeSeq(other.last.toChar, other.init.map(_.toChar)*)

def esc(code: Text): Text = t"${27.toChar}[${code}"

object LineEditor:
  def concealed(str: Text): Text = str.mapChars { _ => '*' }

  def ask(initial: Text = t"", render: Text => Text = identity(_))(using Tty, Log, Monitor): Text =
    Tty.print(render(initial))
    
    def finished(key: Keypress) =
      key == Keypress.Enter || key == Keypress.Ctrl('D') || key == Keypress.Ctrl('C')
    
    Tty.stream[Keypress].takeWhile(!finished(_)).foldLeft(LineEditor(initial, initial.length)):
      case (ed, next) =>
        if ed.pos > 0 then Tty.print(esc(t"${ed.pos}D"))
        val newEd = ed(next)
        val line = t"${newEd.content}${t" "*(ed.content.length - newEd.content.length)}"
        Tty.print(esc(t"0K"))
        Tty.print(render(line))
        if line.length > 0 then Tty.print(esc(t"${line.length}D"))
        if newEd.pos > 0 then Tty.print(esc(t"${newEd.pos}C"))
        newEd
    .content

case class LineEditor(content: Text = t"", pos: Int = 0):
  import Keypress.*

  def apply(keypress: Keypress): LineEditor = try keypress match
    case Printable(ch)  => copy(t"${content.take(pos)}$ch${content.drop(pos)}", pos + 1)
    case Ctrl('U')      => copy(content.drop(pos), 0)
    
    case Ctrl('W')      => val prefix = content.take(0 max (pos - 1)).reverse.dropWhile(_ != ' ').reverse
                           copy(t"$prefix${content.drop(pos)}", prefix.length)
    
    case Delete         => copy(t"${content.take(pos)}${content.drop(pos + 1)}")
    case Backspace      => copy(t"${content.take(pos - 1)}${content.drop(pos)}", (pos - 1) max 0)
    case Home           => copy(pos = 0)
    case End            => copy(pos = content.length)
    case LeftArrow      => copy(pos = (pos - 1) max 0)
    case CtrlLeftArrow  => copy(pos = (pos - 2 max 0 to 0 by -1).find(content(_) == ' ').fold(0)(_ + 1))
    
    case CtrlRightArrow => val range = ((pos + 1) min (content.length - 1)) to (content.length - 1)
                           val newPos = range.find(content(_) == ' ').fold(content.length)(_ + 1)
                           copy(pos = newPos min content.length)
    case RightArrow     => copy(pos = (pos + 1) min content.length)
    case _              => this
  catch case e: OutOfRangeError => this

  def unapply(stream: LazyList[Keypress])(using renderer: Renderer[LineEditor, Text])
             (using Monitor, Tty, Log)
             : Option[(Text, LazyList[Keypress])] =
    renderer(Tty.stream[Keypress], this)(_(_))

trait Renderer[State, R]:
  def before(): Unit = ()
  def render(old: Maybe[State], menu: State): Unit
  def after(): Unit = ()
  def result(state: State): R

  @tailrec
  final def recur(stream: LazyList[Keypress], state: State, oldState: Maybe[State])
                 (key: (State, Keypress) => State)
                 : Option[(R, LazyList[Keypress])] =
    render(oldState, state)
    stream match
      case Keypress.Enter #:: tail           => Some((result(state), tail))
      case Keypress.Ctrl('C' | 'D') #:: tail => None
      case other #:: tail                    => recur(tail, key(state, other), state)(key)
      case _                                 => None

  def apply(stream: LazyList[Keypress], state: State)(key: (State, Keypress) => State)
           : Option[(R, LazyList[Keypress])] =
    before()
    recur(stream, state, Unset)(key).tap(after().waive)

object Renderer:
  given [T: Show](using Tty): Renderer[SelectMenu[T], T] with
    override def before(): Unit = Tty.print(esc(t"?25l"))
    override def after(): Unit = Tty.print(esc(t"J")+esc(t"?25h"))
    
    def render(old: Maybe[SelectMenu[T]], menu: SelectMenu[T]) =
      menu.options.foreach: opt =>
        Tty.print((if opt == menu.current then t" > $opt" else t"   $opt")+esc(t"K")+t"\n")
      Tty.print(esc(t"${menu.options.length}A"))
    
    def result(state: SelectMenu[T]): T = state.current
  
  given (using Tty): Renderer[LineEditor, Text] with
    def render(oldEd: Maybe[LineEditor], newEd: LineEditor): Unit =
      val old = oldEd.or(newEd)
      if old.pos > 0 then Tty.print(esc(t"${old.pos}D"))
      val line = t"${newEd.content}${t" "*(old.content.length - newEd.content.length)}"
      Tty.print(esc(t"0K"))
      Tty.print(line)
      if line.length > 0 then Tty.print(esc(t"${line.length}D"))
      if newEd.pos > 0 then Tty.print(esc(t"${newEd.pos}C"))

    def result(editor: LineEditor): Text = editor.content

case class SelectMenu[T](options: List[T], current: T):
  import Keypress.*
  def apply(keypress: Keypress): SelectMenu[T] = try keypress match
    case UpArrow   => copy(current = options(0 max options.indexOf(current) - 1))
    case DownArrow => copy(current = options(options.size - 1 min options.indexOf(current) + 1))
    case _         => this
  catch case e: OutOfRangeError => this

  def unapply(stream: LazyList[Keypress])(using tty: Tty, log: Log, renderer: Renderer[SelectMenu[T], T])
             (using Monitor)
             : Option[(T, LazyList[Keypress])] =
    renderer(Tty.stream[Keypress], this)(_(_))

given realm: Realm = realm"profanity"

trait InputSource:
  def init()(using Log): Tty throws TtyError
  def cleanup(tty: Tty): Unit
