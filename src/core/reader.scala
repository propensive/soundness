/*
    Cellulose, version 0.4.0. Copyright 2022-23 Jon Pretty, Propensive OÜ.

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
import deviation.*
import gossamer.*
import eucalyptus.*
import kaleidoscope.*

import java.io.*

import language.experimental.captureChecking

opaque type Character = Long

object Character:
  val End: Character = Long.MaxValue
  def apply(int: Int, line: Int, col: Int): Character =
    int.toLong | ((line.toLong&0xffffff) << 48) | ((col.toLong&0xffffff) << 24)

  given Canonical[Character] with
    def serialize(ch: Character): Text = if ch == End then t"[END]" else t"[${ch.char}:${ch.line}:${ch.column}]"
    
    def deserialize(txt: Text): Character = txt match
      case r"[$ch@(.):${As[Int](l)}@([0-9]+):${As[Int](c)}@([0-9]+)]" => Character(unsafely(ch(0).toInt), l, c)
      case _                                                          => End

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
  
  def next()(using Log): Character throws CodlError =
    if finished then throw IllegalStateException("Attempted to read past the end of the stream")
    in.read() match
      case -1 =>
        finished = true
        Character.End
      case '\r' =>
        requireCr match
          case Unset => requireCr = true
          case false => throw CodlError(lastLine, lastCol, 1, CarriageReturnMismatch(false))
          case true  => ()
        
        if in.read() != '\n' then throw CodlError(lastLine, lastCol, 1, UnexpectedCarriageReturn)
        
        Character('\n', lastLine, lastCol).tap(advance)
      
      case '\n' =>
        requireCr match
          case true  => throw CodlError(lastLine, lastCol, 1, CarriageReturnMismatch(true))
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
