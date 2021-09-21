package rivulet

import rudiments.*

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
  def interpret(bytes: List[Byte]): LazyList[KeyType]

case class TtyError() extends Exception("rivulet: STDIN is not attached to a TTY")

sealed case class Tty(private[rivulet] val out: ji.PrintStream)

object Tty:
  def capture[T](fn: Tty ?=> T): T =
    val libc: Libc = Native.load("c", classOf[Libc]).nn
    if libc.isatty(0) != 1 then throw TtyError()
    val oldTermios: Termios = Termios()
    libc.tcgetattr(0, oldTermios)
    val newTermios = Termios(oldTermios)
    newTermios.c_lflag = oldTermios.c_lflag & -76
    libc.tcsetattr(0, 0, newTermios)
    val noopOut = ji.PrintStream(_ => ())
    val stdout = System.out

    val tty: Tty = Tty(stdout)
    System.setOut(noopOut)
    Signal.handle(Signal("WINCH"), sig => reportSize()(using tty))
    try Console.withOut(noopOut)(fn(using tty))
    finally
      System.setOut(stdout)
      libc.tcsetattr(0, 0, oldTermios)

  def reportSize()(using Tty): Unit =
    val esc = 27.toChar
    Tty.print(s"${esc}[s${esc}[4095C${esc}[4095B${esc}[6n${esc}[u")

  def stream[K](using Tty, Keyboard[K]): LazyList[K] =
    val buf: Array[Byte] = new Array[Byte](16)
    val count: Int = System.in.nn.read(buf)
    
    summon[Keyboard[K]].interpret(buf.take(count).to(List)) #::: stream[K]

  def print(msg: String)(using Tty) = summon[Tty].out.print(msg)
  def println(msg: String)(using Tty) = summon[Tty].out.println(msg)


object Keyboard:
  given Keyboard[Byte] with
    def interpret(bytes: List[Byte]): LazyList[Byte] = bytes.to(LazyList)
  
  given Keyboard[List[Byte]] with
    def interpret(bytes: List[Byte]): LazyList[List[Byte]] = LazyList(bytes)
  
  private def readResize(bytes: List[Byte]): Keypress.Resize =
    val size = String(bytes.init.to(Array)).split(";")
    Keypress.Resize(size(0).toInt, size(1).toInt)

  given Keyboard[Keypress] with
    def interpret(bytes: List[Byte]): LazyList[Keypress] = bytes match
      case 9 :: Nil             => LazyList(Keypress.Tab)
      case 10 :: Nil            => LazyList(Keypress.Enter)
      case 27 :: Nil            => LazyList(Keypress.Escape)
      case 27 :: 79 :: i :: Nil => LazyList(Keypress.Function(i - 79))
      case 27 :: 91 :: tail     => LazyList(control(tail))
      case (127 | 8) :: Nil     => LazyList(Keypress.Backspace)
      case i :: Nil if i < 32   => LazyList(Keypress.Ctrl((i + 64).toChar))
      
      case other                => String(bytes.to(Array), 0, bytes.length)
                                     .to(LazyList)
                                     .map(Keypress.Printable(_))
  
    private def control(bytes: List[Byte]): Keypress = bytes match
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
      case other                => Keypress.EscapeSeq(other*)

def esc(code: String): String = s"${27.toChar}[${code}"

object LineEditor:

  def concealed(str: String): String = str.map { _ => '*' }

  def ask(initial: String = "", render: String => String = identity(_))(using Tty): String =
    Tty.print(render(initial))
    
    def finished(key: Keypress) =
      key == Keypress.Enter || key == Keypress.Ctrl('D') || key == Keypress.Ctrl('C')
    
    Tty.stream[Keypress].takeWhile(!finished(_)).foldLeft(LineEditor(initial, initial.length)) {
      case (ed, next) =>
        if ed.pos > 0 then Tty.print(esc(s"${ed.pos}D"))
        val newEd = ed(next)
        val line = newEd.content+" "*(ed.content.length - newEd.content.length)
        Tty.print(render(line))
        if line.length > 0 then Tty.print(esc(s"${line.length}D"))
        if newEd.pos > 0 then Tty.print(esc(s"${newEd.pos}C"))
        newEd
    }.content

case class LineEditor(content: String = "", pos: Int = 0):
  import Keypress.*

  def apply(keypress: Keypress): LineEditor = keypress match
    case Printable(ch)  => copy(content.take(pos)+ch+content.drop(pos), pos + 1)
    case Ctrl('U')      => copy(content.drop(pos), 0)
    case Ctrl('W')      => val prefix = content.take(0 max (pos - 1)).reverse.dropWhile(_ != ' ').reverse
                           copy(prefix+content.drop(pos), prefix.length)
    case Delete         => copy(content.take(pos)+content.drop(pos + 1))
    case Backspace      => copy(content.take(pos - 1)+content.drop(pos), (pos - 1) max 0)
    case Home           => copy(pos = 0)
    case End            => copy(pos = content.length)
    case LeftArrow      => copy(pos = (pos - 1) max 0)
    case CtrlLeftArrow  => val newPos = (((pos - 2) max 0) to 0 by -1).find(content(_) == ' ').fold(0)(_ + 1)
                           copy(pos = newPos max 0)
    case CtrlRightArrow => val range = ((pos + 1) min (content.length - 1)) to (content.length - 1)
                           val newPos = range.find(content(_) == ' ').fold(content.length)(_ + 1)
                           copy(pos = newPos min content.length)
    case RightArrow     => copy(pos = (pos + 1) min content.length)
    case _              => this