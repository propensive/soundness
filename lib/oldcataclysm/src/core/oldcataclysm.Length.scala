                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.54.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package oldcataclysm

import anticipation.*
import gossamer.*
import spectacular.*

object Length:
  given showable: Length is Showable =
    case Auto        => t"auto"
    case Px(value)   => t"${value}px"
    case Pt(value)   => t"${value}pt"
    case In(value)   => t"${value}in"
    case Pc(value)   => t"${value}%"
    case Cm(value)   => t"${value}cm"
    case Mm(value)   => t"${value}mm"
    case Em(value)   => t"${value}em"
    case Ex(value)   => t"${value}ex"
    case Ch(value)   => t"${value}ch"
    case Rem(value)  => t"${value}rem"
    case Vw(value)   => t"${value}vw"
    case Vh(value)   => t"${value}vh"
    case Vmin(value) => t"${value}vmin"
    case Vmax(value) => t"${value}vmax"
    case Calc(calc)  => t"calc($calc)"

enum Length:
  case Px(value: Double)
  case Pt(value: Double)
  case In(value: Double)
  case Auto
  case Pc(value: Double)
  case Cm(value: Double)
  case Mm(value: Double)
  case Em(value: Double)
  case Ex(value: Double)
  case Ch(value: Double)
  case Rem(value: Double)
  case Vw(value: Double)
  case Vh(value: Double)
  case Vmin(value: Double)
  case Vmax(value: Double)
  case Calc(value: Text)

  @targetName("add")
  infix def + (dim: Length): Length = infixOp(t" + ", dim)

  @targetName("sub")
  infix def - (dim: Length): Length = infixOp(t" - ", dim)

  @targetName("mul")
  infix def * (double: Double): Length = infixOp(t" * ", double)

  @targetName("div")
  infix def / (double: Double): Length = infixOp(t" / ", double)

  private def infixOp(operator: Text, dim: Length | Double): Length.Calc = this match
    case Calc(calc) =>
      dim match
        case double: Double => Calc(t"($calc)$operator${double}")
        case Calc(calc2)    => Calc(t"($calc)$operator($calc2)")
        case length: Length => Calc(t"($calc)$operator$length")

    case other =>
      dim match
        case double: Double => Calc(t"${this.show}$operator$double")
        case Calc(calc2)    => Calc(t"${this.show}$operator($calc2)")
        case length: Length => Calc(t"${this.show}$operator$length")

  def function(name: Text, right: Length | Double): Length =
    Calc(t"$name(${infixOp(t", ", right).value})")
