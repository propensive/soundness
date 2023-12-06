/*
    Cellulose, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package cellulose

import rudiments.*
import gossamer.*
import anticipation.*
import kaleidoscope.*
import spectacular.*
import perforate.*

import language.experimental.captureChecking

opaque type Character = Long

object Character:
  val End: Character = Long.MaxValue
  def apply(int: Int, line: Int, col: Int): Character =
    int.toLong | ((line.toLong&0xffffff) << 48) | ((col.toLong&0xffffff) << 24)

  given Encoder[Character] with
    def encode(char: Character): Text =
      if char == End then t"[END]" else t"[${char.char}:${char.line}:${char.column}]"
    
  given Decoder[Character] with
    def decode(txt: Text): Character = txt match
      case r"[$ch(.):${As[Int](l)}([0-9]+):${As[Int](c)}([0-9]+)]" =>
        import unsafeExceptions.canThrowAny
        Character(ch(0).toInt, l, c)
      
      case _ =>
        End

  given Typeable[Character] with
    def unapply(value: Any): Option[value.type & Character] = value.matchable(using Unsafe) match
      case char: Char => Some(value.asInstanceOf[value.type & Character])
      case _          => None

  erased given CanEqual[Char, Character] = ###
  erased given CanEqual[Character, Char] = ###

  extension (char: Character)
    def char: Char = if char == -1 then '\u0000' else char.toChar
    def line: Int = ((char >> 48) & 0xffffff).toInt
    def column: Int = if char == End then 0 else ((char >> 24) & 0xffffff).toInt

import Character.*

class PositionReader(private var in: LazyList[Text]):
  private var lastLine: Int = 0
  private var lastCol: Int = 0
  private var startLine: Int = 0
  private var startCol: Int = 0
  private var requireCr: Maybe[Boolean] = Unset
  private var finished: Boolean = false
  private val buf: StringBuilder = StringBuilder()
  
  private var current: Int = -1

  def charStream(): LazyList[Char] = LazyList.continually(read()).takeWhile(_ != -1).map(_.toChar)
  
  private var text: Text =
    if in.isEmpty then t""
    else
      val result = in.head
      in = in.tail
      result

  @tailrec
  private def read(): Int =
    import unsafeExceptions.canThrowAny
    current += 1
    
    if current < text.length then text(current).toInt else if in.isEmpty then -1 else
      text = in.head
      in = in.tail
      current = -1
      read()

  private def advance(char: Character): Unit = char.char match
    case '\n' =>
      lastLine += 1
      lastCol = 0
    case _ =>
      lastCol += 1
  
  def next()(using Raises[CodlError]): Character =
    if finished then throw IllegalStateException("Attempted to read past the end of the stream")
    read() match
      case -1 =>
        finished = true
        Character.End
      case '\r' =>
        requireCr match
          case Unset => requireCr = true
          case false => raise(CodlError(lastLine, lastCol, 1, CarriageReturnMismatch(false)))(())
          case true  => ()
        
        if read() != '\n' then raise(CodlError(lastLine, lastCol, 1, UnexpectedCarriageReturn))(())
        
        Character('\n', lastLine, lastCol).tap(advance)
      
      case '\n' =>
        requireCr match
          case true  => raise(CodlError(lastLine, lastCol, 1, CarriageReturnMismatch(true)))(())
          case Unset => requireCr = false
          case false => ()
        
        Character('\n', lastLine, lastCol).tap(advance)
      
      case ch =>
        Character(ch, lastLine, lastCol).tap(advance)
  
  def start(): (Int, Int) = (startLine, startCol)
  def get(): Text = buf.toString.show.also(buf.clear())
  
  def put(char: Character): Unit =
    if buf.isEmpty then
      startLine = char.line
      startCol = char.column

    buf.append(char.char)
