package rivulet

import com.sun.jna.*
import java.nio.*, charset.*

trait Libc extends Library:
  def tcgetattr(fd: Int, termios: Termios): Int
  def tcsetattr(fd: Int, opt: Int, termios: Termios): Int
  def isatty(fd: Int): Int

@main
def setup(): Unit =
  Tty.capture {
    Tty.stream.foreach { key =>
      println(key)
    }
  }

enum Keypress:
  case Printable(char: Char)
  case Function(number: Int)
  case Control(char: Char)
  case EscapeSeq(bytes: Byte*)
  case Enter, Escape, Tab, Backspace, Delete, PageUp, PageDown, LeftArrow, RightArrow, UpArrow,
      DownArrow, End, Home, Insert

trait Keyboard[+KeyType]:
  def interpret(bytes: List[Byte]): KeyType

case class TtyError() extends Exception("rivulet: STDIN is not attached to a TTY")

sealed class Tty()

object Tty:

  private val tty: Tty = Tty()

  def capture[T](fn: Tty ?=> T): T =
    val libc: Libc = Native.load("c", classOf[Libc])
    if libc.isatty(0) != 1 then throw TtyError()
    val oldTermios: Termios = Termios()
    libc.tcgetattr(0, oldTermios)
    val newTermios = Termios(oldTermios)
    newTermios.c_lflag = oldTermios.c_lflag & -76
    libc.tcsetattr(0, 0, newTermios)
    
    try fn(using tty) finally libc.tcsetattr(0, 0, oldTermios)

  def stream[K](using Tty, Keyboard[K]): LazyList[K] =
    val buf: Array[Byte] = new Array[Byte](6)
    val count: Int = System.in.read(buf)
    
    summon[Keyboard[K]].interpret(buf.take(count).to(List)) #:: stream[K]


object Keyboard:
  given Keyboard[Keypress] with
    def interpret(bytes: List[Byte]): Keypress = bytes match
      case 9 :: Nil             => Keypress.Tab
      case 10 :: Nil            => Keypress.Enter
      case i :: Nil if i < 32   => Keypress.Control((i + 64).toChar)
      case 27 :: Nil            => Keypress.Escape
      case 27 :: 79 :: i :: Nil => Keypress.Function(i - 79)
      case 27 :: 91 :: tail     => control(tail)
      case (127 | 8) :: Nil     => Keypress.Backspace
      case other                => Keypress.Printable(String(bytes.to(Array), 0, bytes.length).head)
  
    private def control(bytes: List[Byte]): Keypress = bytes match
      case List(51, 126)     => Keypress.Delete
      case List(50, 126)     => Keypress.Insert
      case List(70)          => Keypress.End
      case List(72)          => Keypress.Home
      case List(53, 126)     => Keypress.PageUp
      case List(54, 126)     => Keypress.PageDown
      case List(68)          => Keypress.LeftArrow
      case List(67)          => Keypress.RightArrow
      case List(65)          => Keypress.UpArrow
      case List(66)          => Keypress.DownArrow
      case other             => Keypress.EscapeSeq(other*)
    