/*
    Profanity, version 0.4.0. Copyright 2021-22 Jon Pretty, Propensive OÜ.

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
import gossamer.*
import eucalyptus.*
import escapade.*
import iridescence.*
import turbulence.*

import com.sun.jna.*
import sun.misc.Signal
import java.nio.*, charset.*
import java.io as ji

trait Libc extends Library:
  def tcgetattr(fd: Int, termios: Termios): Int
  def tcsetattr(fd: Int, opt: Int, termios: Termios): Int
  def isatty(fd: Int): Int

enum Keypress:
  case Printable(char: Char)
  case Function(number: Int)
  case Ctrl(char: Char)
  case EscapeSeq(bytes: Byte*)
  case Resize(rows: Int, columns: Int)
  case Enter, Escape, Tab, Backspace, Delete, PageUp, PageDown, LeftArrow, RightArrow, UpArrow,
      DownArrow, CtrlLeftArrow, CtrlRightArrow, CtrlUpArrow, CtrlDownArrow, End, Home, Insert

trait Keyboard[+KeyType]:
  def interpret(bytes: IArray[Byte])(using Log): LazyList[KeyType]

case class TtyError(ttyMsg: Text)(using Codepoint)
extends Error(err"STDIN is not attached to a TTY: $ttyMsg")(pos)

case class Tty(out: ji.PrintStream, in: DataStream)

object Tty:
  final val noopOut: ji.PrintStream = ji.PrintStream((_ => ()): ji.OutputStream)

  def capture[T](fn: Tty ?=> T)(using Log, InputSource): T throws TtyError =
    val tty = summon[InputSource].init()
    Signal.handle(Signal("WINCH"), sig => reportSize()(using tty))
    try Console.withOut(noopOut)(fn(using tty)) finally summon[InputSource].cleanup(tty)

  def reportSize()(using Tty, Log): Unit =
    val esc = 27.toChar
    Log.fine(ansi"Sent ANSI escape codes to TTY to attempt to get console dimensions")
    Tty.print(t"${esc}[s${esc}[4095C${esc}[4095B${esc}[6n${esc}[u")

  def stream[K](using Tty, Keyboard[K], Log): LazyList[K] = summon[Tty].in.flatMap:
    data => unsafely(summon[Keyboard[K]].interpret(data))

  def print(msg: Text)(using Tty) = summon[Tty].out.print(msg.s)
  def println(msg: Text)(using Tty) = summon[Tty].out.println(msg.s)


object Keyboard:
  given Keyboard[Int] with
    def interpret(bytes: IArray[Byte])(using Log): LazyList[Int] = bytes.map(_.toInt).to(LazyList)
  
  given Keyboard[IArray[Int]] with
    def interpret(bytes: IArray[Byte])(using Log): LazyList[IArray[Int]] =
      LazyList(bytes.map(_.toInt))
  
  private def readResize(bytes: List[Int])(using Log): Keypress.Resize =
    val size = String(bytes.map(_.toByte).init.to(Array)).show.cut(t";")
    val columns = size(0).toString.toInt
    val rows = size(1).toString.toInt
    Log.fine(ansi"Console has been resized to $columns×$rows")
    
    Keypress.Resize(rows, columns)

  given Keyboard[Keypress] with
    def interpret(bytes: IArray[Byte])(using Log): LazyList[Keypress] =
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
    
    private def control(bytes: List[Int])(using Log): Keypress = bytes match
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
      case other                => Keypress.EscapeSeq(other.map(_.toByte)*)

def esc(code: Text): Text = t"${27.toChar}[${code}"

object LineEditor:
  def concealed(str: Text): Text = str.map { _ => '*' }

  def ask(initial: Text = t"", render: Text => Text = identity(_))(using Tty, Log): Text =
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

  def apply(keypress: Keypress)(using Log): LineEditor = try keypress match
    case Printable(ch)  => copy(t"${content.take(pos)}$ch${content.drop(pos)}", pos + 1)
    case Ctrl('U')      => copy(content.drop(pos), 0)
    
    case Ctrl('W')      => val prefix = content.take(0 max (pos - 1)).reverse.dropWhile(_ != ' ').reverse
                           copy(t"$prefix${content.drop(pos)}", prefix.length)
    
    case Delete         => copy(t"${content.take(pos)}${content.drop(pos + 1)}")
    case Backspace      => copy(t"${content.take(pos - 1)}${content.drop(pos)}", (pos - 1) max 0)
    case Home           => copy(pos = 0)
    case End            => copy(pos = content.length)
    case LeftArrow      => copy(pos = (pos - 1) max 0)
    
    case CtrlLeftArrow  => copy(pos = (pos - 2 max 0 to 0 by -1).find(content(_) == ' ').fold(0)(_ +
                               1))
    
    case CtrlRightArrow => val range = ((pos + 1) min (content.length - 1)) to (content.length - 1)
                           val newPos = range.find(content(_) == ' ').fold(content.length)(_ + 1)
                           copy(pos = newPos min content.length)
    case RightArrow     => copy(pos = (pos + 1) min content.length)
    case _              => this
  catch case e: OutOfRangeError => this

object SelectMenu:
  def ask[T](options: List[T], initial: T, renderOn: T => Text, renderOff: T => Text)
         (using Tty, Log): T =
    
    def finished(key: Keypress) =
      key == Keypress.Enter || key == Keypress.Ctrl('D') || key == Keypress.Ctrl('C')

    def render(options: List[T], current: T): Unit =
      options.foreach:
        case opt =>
          Tty.print(if opt == current then renderOn(opt) else renderOff(opt))
          Tty.print(esc(t"K"))
          Tty.print(t"\n")
      
      Tty.print(esc(t"${options.length}A"))

    Tty.print(esc(t"?25l"))
    
    val menu = SelectMenu(options, initial)
    render(menu.options, menu.current)
    
    val result = Tty.stream[Keypress].takeWhile(!finished(_)).foldLeft(menu) {
      case (menu, next) =>
        val newMenu = menu(next)
        render(newMenu.options, newMenu.current)
        newMenu
    }

    Tty.print(esc(t"J"))
    Tty.print(esc(t"?25h"))

    result.current

case class SelectMenu[T](options: List[T], current: T):
  import Keypress.*
  def apply(keypress: Keypress)(using Log): SelectMenu[T] = try keypress match
    case UpArrow   => copy(current = options(0 max options.indexOf(current) - 1))
    case DownArrow => copy(current = options(options.size - 1 min options.indexOf(current) + 1))
    case _         => this
  catch case e: OutOfRangeError => this

given realm: Realm = Realm(t"profanity")

trait InputSource:
  def init()(using Log): Tty throws TtyError
  def cleanup(tty: Tty): Unit

package inputSource:
  given jvm: InputSource = Jvm

object Jvm extends InputSource:
  lazy val libc: Libc = Native.load("c", classOf[Libc]).nn
  lazy val oldTermios: Termios = Termios()

  def init()(using Log): Tty throws TtyError =
    Log.fine(ansi"Loading native libc library")
    libc
    Log.fine(ansi"Checking if process in running within a TTY")
    if libc.isatty(0) != 1 then throw TtyError(t"the program is not running within a TTY")
    oldTermios
    Log.fine(ansi"Calling ${colors.Purple}(libc.tcgetattr)")
    libc.tcgetattr(0, oldTermios)
    val newTermios = Termios(oldTermios)
    Log.fine(ansi"Updating ${colors.Purple}(termios) flags")
    newTermios.c_lflag = oldTermios.c_lflag & -76
    Log.fine(ansi"Calling ${colors.Purple}(libc.tcsetattr) flags")
    libc.tcsetattr(0, 0, newTermios)
    
    val stdout = Option(System.out).map(_.nn).getOrElse:
      throw TtyError(t"the STDOUT stream is null")
    
    if stdout == Tty.noopOut then throw TtyError(t"the TTY has already been captured")

    Log.info(ansi"Capturing ${colors.Red}(stdout) for TTY input/output")
    Log.info(ansi"Any output to ${colors.Red}(stdout) henceforth will be discarded")
    
    val tty: Tty = Tty(stdout, Util.readInputStream(System.in.nn, 1.kb))
    System.setOut(Tty.noopOut)
    Log.info(ansi"Setting ${colors.Orange}(SIGWINCH) signal handler")
    tty
      
  def cleanup(tty: Tty): Unit =
    System.setOut(tty.out)
    libc.tcsetattr(0, 0, oldTermios)