package hieroglyph

import rudiments.*

object Chars:
  val superscript: PartialFunction[Char, Char] =
    case '0' => '⁰'
    case '1' => '¹'
    case '2' => '²'
    case '3' => '³'
    case '4' => '⁴'
    case '5' => '⁵'
    case '6' => '⁶'
    case '7' => '⁷'
    case '8' => '⁸'
    case '9' => '⁹'
    case '(' => '₍'
    case ')' => '₎'
    case '+' => '₊'
    case '-' => '₋'
    case '=' => '₌'
  
  val subscript: PartialFunction[Char, Char] =
    case '0' => '₀'
    case '1' => '₁'
    case '2' => '₂'
    case '3' => '₃'
    case '4' => '₄'
    case '5' => '₅'
    case '6' => '₆'
    case '7' => '₇'
    case '8' => '₈'
    case '9' => '₉'

extension (char: Char)
  def superscript: Maybe[Char] = Chars.superscript.applyOrElse(char, _ => Unset)
  def subscript: Maybe[Char] = Chars.subscript.applyOrElse(char, _ => Unset)
