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
package ypsiloid

import anticipation.*

object YamlAst:
  // Byte constants used by the parser. Values match Java/JVM byte
  // representation (signed, but only ASCII so positive). Inline `final val`
  // turns these into compile-time constants suitable for `@switch` matches.
  object Byte:
    inline final val Tab:           9   = 9
    inline final val Newline:       10  = 10
    inline final val Return:        13  = 13
    inline final val Space:         32  = 32
    inline final val Bang:          33  = 33   // '!'
    inline final val Quote:         34  = 34   // '"'
    inline final val Hash:          35  = 35   // '#'
    inline final val Amp:           38  = 38   // '&'
    inline final val Apostrophe:    39  = 39   // '\''
    inline final val Star:          42  = 42   // '*'
    inline final val Plus:          43  = 43   // '+'
    inline final val Comma:         44  = 44
    inline final val Minus:         45  = 45   // '-'
    inline final val Period:        46  = 46
    inline final val Slash:         47  = 47
    inline final val Num0:          48  = 48
    inline final val Num1:          49  = 49
    inline final val Num2:          50  = 50
    inline final val Num3:          51  = 51
    inline final val Num4:          52  = 52
    inline final val Num5:          53  = 53
    inline final val Num6:          54  = 54
    inline final val Num7:          55  = 55
    inline final val Num8:          56  = 56
    inline final val Num9:          57  = 57
    inline final val Colon:         58  = 58
    inline final val Question:      63  = 63   // '?'
    inline final val UpperE:        69  = 69
    inline final val UpperF:        70  = 70
    inline final val UpperX:        88  = 88
    inline final val OpenBracket:   91  = 91
    inline final val Backslash:     92  = 92
    inline final val CloseBracket:  93  = 93
    inline final val Caret:         94  = 94
    inline final val Underscore:    95  = 95
    inline final val LowerA:        97  = 97
    inline final val LowerB:        98  = 98
    inline final val LowerE:        101 = 101
    inline final val LowerF:        102 = 102
    inline final val LowerL:        108 = 108
    inline final val LowerN:        110 = 110
    inline final val LowerO:        111 = 111
    inline final val LowerR:        114 = 114
    inline final val LowerS:        115 = 115
    inline final val LowerT:        116 = 116
    inline final val LowerU:        117 = 117
    inline final val LowerV:        118 = 118
    inline final val LowerX:        120 = 120
    inline final val OpenBrace:     123 = 123
    inline final val Pipe:          124 = 124
    inline final val CloseBrace:    125 = 125
    inline final val Tilde:         126 = 126
    inline final val Greater:       62  = 62

enum YamlAst:
  case Null
  case Bool(value: Boolean)
  case Integer(value: Long)
  case Decimal(value: Double)
  case Str(value: Text)
  case Sequence(items: IArray[YamlAst])
  case Mapping(entries: IArray[(YamlAst, YamlAst)])

  override def equals(other: Any): Boolean =
    (this.asInstanceOf[AnyRef] eq other.asInstanceOf[AnyRef])
    || ((this: YamlAst, other) match
      case (Bool(a), Bool(b))       => a == b
      case (Integer(a), Integer(b)) => a == b
      case (Decimal(a), Decimal(b)) => a == b
      case (Str(a), Str(b))         => a == b

      case (Sequence(a), Sequence(b)) =>
        a.length == b.length && a.indices.forall(index => a(index) == b(index))

      case (Mapping(a), Mapping(b)) =>
        a.length == b.length && a.indices.forall: index =>
          a(index)(0) == b(index)(0) && a(index)(1) == b(index)(1)

      case _ => false)

  override def hashCode: Int = this match
    case Null         => 0
    case Bool(v)      => v.hashCode
    case Integer(v)   => v.hashCode
    case Decimal(v)   => v.hashCode
    case Str(v)       => v.hashCode
    case Sequence(xs) => xs.foldLeft(xs.length)(_ * 31 + _.hashCode)

    case Mapping(es) =>
      es.foldLeft(es.length)((acc, e) => acc*31 + e._1.hashCode*31 + e._2.hashCode)
