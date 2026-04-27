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
package kaleidoscope

import fulminate.*

object RegexError:
  enum Reason(val number: Int) extends Clarification:
    case UnclosedGroup       extends Reason(1)
    case ExpectedGroup       extends Reason(2)
    case BadRepetition       extends Reason(3)
    case Uncapturable        extends Reason(4)
    case UnexpectedChar      extends Reason(5)
    case NotInGroup          extends Reason(6)
    case IncompleteRepetition extends Reason(7)
    case InvalidPattern      extends Reason(8)
    case UnclosedEscape      extends Reason(9)
    case EmptyCharClass      extends Reason(10)
    case ZeroMaximum         extends Reason(11)

  object Reason:
    given communicable: Reason is Communicable =
      case UnclosedGroup => m"a capturing group was not closed"

      case ExpectedGroup =>
        m"a capturing group was expected immediately following an extractor"

      case BadRepetition =>
        m"the maximum number of repetitions is less than the minimum"

      case Uncapturable =>
        m"a capturing group inside a repeating group can not be extracted"

      case UnexpectedChar =>
        m"the repetition range contained an unexpected character"

      case NotInGroup =>
        m"a closing parenthesis was found without a corresponding opening parenthesis"

      case IncompleteRepetition =>
        m"the repetition range was not closed"

      case InvalidPattern =>
        m"the pattern was invalid"

      case UnclosedEscape =>
        m"nothing followed the escape character `\`"

      case EmptyCharClass =>
        m"the character class is empty"

      case ZeroMaximum =>
        m"the maximum number of repetitions must be greater than zero"

case class RegexError(index: Int, reason: RegexError.Reason)(using Diagnostics)
extends Error(realm"kd", 1, reason.number)
              (m"the regular expression could not be parsed because $reason at $index")
