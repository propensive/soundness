package cellulose

import rudiments.*
import gossamer.*
import eucalyptus.*

import java.io.*

import language.experimental.captureChecking

opaque type Character = Long

object Character:
  val End: Character = Long.MaxValue
  def apply(int: Int, line: Int, col: Int): Character =
    int.toLong | ((line.toLong&0xffffff) << 48) | ((col.toLong&0xffffff) << 24)
  
  given Typeable[Character] with
    def unapply(value: Any): Option[value.type & Character] = value match
      case char: Char => Some(value.asInstanceOf[value.type & Character])
      case _          => None

  erased given CanEqual[Char, Character] = compiletime.erasedValue
  erased given CanEqual[Character, Char] = compiletime.erasedValue

  extension (char: Character)
    def char: Char = if char == -1 then '\u0000' else char.toChar
    def line: Int = ((char >> 48) & 0xffffff).toInt
    def column: Int = ((char >> 24) & 0xffffff).toInt

import Character.*

class PositionReader(private val in: Reader):
  private var lastLine: Int = 0
  private var lastCol: Int = 0
  private var startLine: Int = 0
  private var startCol: Int = 0
  private var requireCr: Maybe[Boolean] = Unset
  private var finished: Boolean = false
  private val buf: StringBuilder = StringBuilder()
  
  private def advance(char: Character): Unit = char.char match
    case '\n' =>
      lastLine += 1
      lastCol = 0
    case _ =>
      lastCol += 1
  
  def next()(using Log): Character throws CodlParseError =
    if finished then throw IllegalStateException("Attempted to read past the end of the stream")
    in.read() match
      case -1 =>
        finished = true
        Character.End
      case '\r' =>
        requireCr match
          case Unset => requireCr = true
          case false => throw CodlParseError(lastLine, lastCol, CarriageReturnMismatch(false))
          case true  => ()
        
        if in.read() != '\n' then throw CodlParseError(lastLine, lastCol, UnexpectedCarriageReturn)
        
        Character('\n', lastLine, lastCol).tap(advance)
      
      case '\n' =>
        requireCr match
          case true  => throw CodlParseError(lastLine, lastCol, CarriageReturnMismatch(true))
          case Unset => requireCr = false
          case false => ()
        
        Character('\n', lastLine, lastCol).tap(advance)
      
      case ch =>
        Character(ch, lastLine, lastCol).tap(advance)
  
  def start(): (Int, Int) = (startLine, startCol)
  def get(): Text = buf.toString.show.tap(buf.clear().waive)
  
  def put(char: Character): Unit =
    if buf.isEmpty then
      startLine = char.line
      startCol = char.column

    buf.append(char.char)
