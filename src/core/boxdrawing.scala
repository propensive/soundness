package escritoire

import anticipation.*
import gossamer.*

enum BoxLine:
  case None, Thin, Thick, Double

object BoxDrawing:
  private val lineChars: IArray[Char] =
    List
      (t" ╴╸ ╷┐┑╕╻┒┓  ╖ ╗╶─╾ ┌┬┭ ┎┰┱ ╓╥╓ ╺╼━ ┍┮┯╕┏┲┳  ╖ ╗   ═╒ ╒╤   ═╔ ╔╦╵┘┙╛│┤┥╡╽┧┪╛    └┴┵ ├┼┽ ┟╁╅     ┕┶┷╛",
       t"┝┾┿╡┢╆╈╛    ╘ ╘╧╞ ╞╪╘ ╘╧    ╹┚┛ ╿┦┩╕┃┨┫  ╖ ╗┖┸┹ ┞╀╃ ┠╂╊ ╓╥╓ ┗┺┻ ┡╄╇╕┣ ╋  ╖ ╗   ═╒ ╒╤   ═╔ ╔╦ ╜ ╝    ",
       t" ╜ ╝║╢║╣╙╨╙     ╙╨╙ ╟╫╟  ╜ ╝     ╜ ╝║╢║╣╚ ╚╩    ╚ ╚╩╠ ╠╬").join.chars
  
  private def roundCorners(chars: IArray[Char]): IArray[Char] = chars.map:
    case '┌'  => '╭'
    case '┘'  => '╯'
    case '┐'  => '╮'
    case '└'  => '╰'
    case char => char

  def apply(top: BoxLine, right: BoxLine, bottom: BoxLine, left: BoxLine): Char =
    lineChars(left.ordinal + bottom.ordinal*4 + right.ordinal*16 + top.ordinal*64)
  
  def apply(vertical: BoxLine, horizontal: BoxLine): Char = apply(vertical, horizontal, vertical, horizontal)
