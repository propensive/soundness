/*
    Escritoire, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package escritoire

import anticipation.*
import gossamer.*

enum BoxLine:
  case Blank, Thin, Thick, Double

enum LineCharset:
  case Default, Rounded, Ascii

  def apply(): IArray[Char] = this match
    case Default => BoxDrawing.defaultChars
    case Rounded => BoxDrawing.roundedChars
    case Ascii   => BoxDrawing.asciiChars

object BoxDrawing:
  val asciiChars: IArray[Char] =
    List
      (t" -- |+++|++  + +--- +++ +++ +++ --- +++++++  + +   -+ ++   -+ ++|+++|+++|+++    +++ +++ +++     ++++",
       t"++++++++    + +++ +++ ++    |++ |+++|++  + ++++ +++ +++ +++ +++ +++++ +  + +   -+ ++   -+ ++ + +    ",
       t" + +|+|++++     +++ +++  + +     + +|+|++ ++    + +++ ++").join.chars

  val defaultChars: IArray[Char] =
    List
      (t" ╴╸ ╷┐┑╕╻┒┓  ╖ ╗╶─╾ ┌┬┭ ┎┰┱ ╓╥╓ ╺╼━ ┍┮┯╕┏┲┳  ╖ ╗   ═╒ ╒╤   ═╔ ╔╦╵┘┙╛│┤┥╡╽┧┪╛    └┴┵ ├┼┽ ┟╁╅     ┕┶┷╛",
       t"┝┾┿╡┢╆╈╛    ╘ ╘╧╞ ╞╪╘ ╘╧    ╹┚┛ ╿┦┩╕┃┨┫  ╖ ╗┖┸┹ ┞╀╃ ┠╂╊ ╓╥╓ ┗┺┻ ┡╄╇╕┣ ╋  ╖ ╗   ═╒ ╒╤   ═╔ ╔╦ ╜ ╝    ",
       t" ╜ ╝║╢║╣╙╨╙     ╙╨╙ ╟╫╟  ╜ ╝     ╜ ╝║╢║╣╚ ╚╩    ╚ ╚╩╠ ╠╬").join.chars
  
  val roundedChars: IArray[Char] = defaultChars.map:
    case '┌'  => '╭'
    case '┘'  => '╯'
    case '┐'  => '╮'
    case '└'  => '╰'
    case char => char

  def apply
      (top: BoxLine = BoxLine.Blank,
       right: BoxLine = BoxLine.Blank,
       bottom: BoxLine = BoxLine.Blank,
       left: BoxLine = BoxLine.Blank,
       charset: LineCharset = LineCharset.Default)
          : Char =
    charset()(left.ordinal + bottom.ordinal*4 + right.ordinal*16 + top.ordinal*64)
  
  def simple(vertical: BoxLine, horizontal: BoxLine, charset: LineCharset): Char =
    apply(vertical, horizontal, vertical, horizontal, charset)
