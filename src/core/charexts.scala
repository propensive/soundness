/*
    Hieroglyph, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package hieroglyph

import vacuous.*

import language.experimental.captureChecking

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
  def superscript: Optional[Char] = Chars.superscript.applyOrElse(char, _ => Unset)
  def subscript: Optional[Char] = Chars.subscript.applyOrElse(char, _ => Unset)
