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
package merino

import anticipation.*
import contingency.*
import fulminate.*
import prepositional.*
import proscenium.*
import turbulence.*
import vacuous.*
import zephyrine.*

object JsonAst extends Format:
  def name: Text = "JSON"

  case class Position(line: Int, column: Int) extends Format.Position:
    def describe: Text = ("line "+line+", column "+column).tt

  enum Issue extends Format.Issue:
    case EmptyInput
    case UnexpectedChar(found: Char)
    case ExpectedTrue
    case ExpectedFalse
    case ExpectedNull
    case ExpectedSomeValue(char: Char)
    case ExpectedColon(found: Char)
    case InvalidWhitespace
    case ExpectedString(found: Char)
    case ExpectedHexDigit(found: Char)
    case PrematureEnd
    case NumberHasLeadingZero
    case SpuriousContent(found: Char)
    case LeadingDecimalPoint
    case NotEscaped(char: Char)
    case IncorrectEscape(char: Char)
    case MultipleDecimalPoints
    case ExpectedDigit(found: Char)

    def describe: Message = this match
      case EmptyInput              => m"the input was empty"
      case UnexpectedChar(found)   => m"the character $found was not expected here"
      case ExpectedTrue            => m"true was expected"
      case ExpectedFalse           => m"false was expected"
      case ExpectedNull            => m"null was expected"
      case ExpectedSomeValue(char) => m"a value was expected but $char was found instead"
      case ExpectedColon(found)    => m"a colon was expected but $found was found instead"
      case InvalidWhitespace       => m"invalid whitespace was found"
      case ExpectedString(found)   => m"a string was expected but $found was found instead"
      case ExpectedHexDigit(found) => m"a hexadecimal digit was expected"
      case PrematureEnd            => m"the content ended prematurely"
      case SpuriousContent(found)  => m"$found was found after the full JSON value was read"
      case LeadingDecimalPoint     => m"a number cannot start with a decimal point"
      case NotEscaped(char)        => m"the character $char must be escaped with a backslash"
      case ExpectedDigit(found)    => m"a digit was expected but $found was found instead"
      case MultipleDecimalPoints   => m"a number cannot contain more than one decimal point"

      case NumberHasLeadingZero =>
        m"a number cannot start with a zero except when followed by a decimal point"

      case IncorrectEscape(char) =>
        m"the character $char was escaped with a backslash unnecessarily"

  object AsciiByte:
    inline final val Tab:          9   = 9   // '\t'
    inline final val Newline:      10  = 10  // '\n'
    inline final val Return:       13  = 13  // '\r'
    inline final val Space:        32  = 32  // ' '
    inline final val Comma:        44  = 44  // ','
    inline final val Quote:        34  = 34  // '"'
    inline final val Minus:        45  = 45  // '-'
    inline final val Plus:         43  = 43  // '+'
    inline final val Slash:        47  = 47  // '/'
    inline final val Period:       46  = 46  // '.'
    inline final val Num0:         48  = 48  //'0'
    inline final val Num1:         49  = 49  //'1'
    inline final val Num2:         50  = 50  //'2'
    inline final val Num3:         51  = 51  //'3'
    inline final val Num4:         52  = 52  //'4'
    inline final val Num5:         53  = 53  //'5'
    inline final val Num6:         54  = 54  //'6'
    inline final val Num7:         55  = 55  //'7'
    inline final val Num8:         56  = 56  //'8'
    inline final val Num9:         57  = 57  //'9'
    inline final val Colon:        58  = 58  // ':'
    inline final val UpperA:       65  = 65  // 'A'
    inline final val UpperB:       66  = 66  // 'B'
    inline final val UpperC:       67  = 67  // 'C'
    inline final val UpperD:       68  = 68  // 'D'
    inline final val UpperE:       69  = 69  // 'E'
    inline final val UpperF:       70  = 70  // 'F'
    inline final val OpenBracket:  91  = 91  // '['
    inline final val CloseBracket: 93  = 93  // ']'
    inline final val Backslash:    92  = 92  // '\\'
    inline final val LowerA:       97  = 97  // 'a'
    inline final val LowerB:       98  = 98  // 'b'
    inline final val LowerC:       99  = 99  // 'c'
    inline final val LowerD:       100 = 100 // 'd'
    inline final val LowerE:       101 = 101 // 'e'
    inline final val LowerF:       102 = 102 // 'f'
    inline final val LowerL:       108 = 108 // 'l'
    inline final val LowerN:       110 = 110 // 'n'
    inline final val LowerR:       114 = 114 // 'r'
    inline final val LowerS:       115 = 115 // 's'
    inline final val LowerT:       116 = 116 // 't'
    inline final val LowerU:       117 = 117 // 'u'
    inline final val OpenBrace:    123 = 123 // '{'
    inline final val CloseBrace:   125 = 125 // '}'

  opaque type RawJson =
    Long | Double | Bcd | String | IArray[Any] | Boolean | Null | Unset.type

  // Sentinel used to pad an array whose original length is even, so that all
  // arrays have odd `IArray[Any]` length and can be distinguished from objects
  // (which are encoded as alternating `key, value, …` and therefore always
  // have even length). The sentinel only ever appears as the last element of a
  // padded array and is never part of the user-visible array contents.
  val arrayPad: AnyRef = new Object

  def apply
    ( value
      : Long | Double | Bcd | String | IArray[Any] | Boolean | Null | Unset.type )
  :   JsonAst =

    value

  // Build an object node from parallel `keys` and `values` arrays. The result
  // is stored as a single `IArray[Any]` of length `2 * keys.length`, with keys
  // at even indices and values at odd indices.
  def obj(keys: IArray[String], values: IArray[Any]): JsonAst =
    val n = keys.length
    val arr = new Array[Any](n*2)
    var i = 0
    while i < n do
      arr(i*2) = keys(i)
      arr(i*2 + 1) = values(i)
      i += 1
    arr.asInstanceOf[IArray[Any]]

  // Build an array node from `elements`. If the element count is even, a
  // single sentinel `arrayPad` is appended so the stored `IArray[Any]` has
  // odd length.
  def arr(elements: IArray[Any]): JsonAst =
    val n = elements.length
    if (n & 1) == 1 then elements
    else
      val padded = new Array[Any](n + 1)
      System.arraycopy(elements.asInstanceOf[Array[Any]], 0, padded, 0, n)
      padded(n) = arrayPad
      padded.asInstanceOf[IArray[Any]]

  // The number of user-visible elements in an array node (excludes the
  // sentinel padding, if present).
  def arrayLength(json: JsonAst): Int =
    val arr = json.asInstanceOf[Array[?]]
    val n = arr.length
    if n > 0 && (arr(n - 1).asInstanceOf[AnyRef] eq arrayPad) then n - 1 else n

  // The number of key/value pairs in an object node.
  def objectSize(json: JsonAst): Int = json.asInstanceOf[Array[?]].length/2


  given Tactic[ParseError] => JsonAst is Aggregable by Data =
    source => parse(source.iterator)

  def parse(source: Data): JsonAst raises ParseError = JsonAst(JsonParser.parse(source))

  def parse(source: Data, holes: Boolean): JsonAst raises ParseError =
    JsonAst(JsonParser.parse(source, holes))

  def parse(input: Iterator[Data]): JsonAst raises ParseError = JsonAst(JsonParser.parse(input))

  def parse(input: Iterator[Data], holes: Boolean): JsonAst raises ParseError =
    JsonAst(JsonParser.parse(input, holes))

