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
┃    Soundness, version 0.64.0.                                                                    ┃
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
package revolution

import scala.collection.immutable.Seq
import scala.quoted.*

import anticipation.*
import contextual.*
import contingency.*
import distillate.*
import fulminate.*
import gigantism.*
import vacuous.*

object internal:
  // Where in a version's text a fault of the given kind lies, as an offset and a length. The
  // decoder gives the reason; the text is walked again here only to find the place.
  private def locate(text: String, reason: Semver.Error.Reason): (Int, Int) =
    import Semver.Error.Reason.*

    def digits(from: Int): Int =
      var index = from
      while index < text.length && text.charAt(index) >= '0' && text.charAt(index) <= '9' do index += 1
      index

    def identifiers(from: Int, to: Int): scala.List[(Int, Int)] =
      var found: scala.List[(Int, Int)] = scala.Nil
      var start = from
      var index = from

      while index <= to do
        if index == to || text.charAt(index) == '.' then
          found = (start, index) :: found
          start = index + 1

        index += 1

      found.reverse

    def valid(char: Char): Boolean =
      (char >= '0' && char <= '9') || (char >= 'A' && char <= 'Z') || (char >= 'a' && char <= 'z')
      || char == '-'

    // The three numbers, each as its start and end, or the place where the form is broken.
    var numbers: scala.List[(Int, Int)] = scala.Nil
    var broken: Int = -1
    var index = 0
    var count = 0

    while count < 3 && broken < 0 do
      val end = digits(index)

      if end == index then broken = index
      else
        numbers = (index, end) :: numbers
        count += 1

        if count < 3 then
          if end < text.length && text.charAt(end) == '.' then index = end + 1 else broken = end
        else
          index = end

    numbers = numbers.reverse

    val plus = if broken >= 0 then -1 else text.indexOf('+', index)

    val prerelease: scala.List[(Int, Int)] =
      if broken >= 0 || index >= text.length || text.charAt(index) != '-' then scala.Nil
      else identifiers(index + 1, if plus < 0 then text.length else plus)

    val build: scala.List[(Int, Int)] =
      if plus < 0 then scala.Nil else identifiers(plus + 1, text.length)

    val whole = (0, text.length.max(1))

    reason match
      case BadFormat =>
        if broken >= 0 then (broken.min((text.length - 1).max(0)), 1)
        else if index < text.length && text.charAt(index) != '-' && text.charAt(index) != '+'
        then (index, 1)
        else if index < text.length && text.charAt(index) == '-'
          && (index + 1 == text.length || text.charAt(index + 1) == '+')
        then (index, 1)
        else if plus >= 0 && plus + 1 == text.length then (plus, 1)
        else
          numbers.find { (start, end) => end - start > 18 }.map: (start, end) =>
            (start, end - start)

          . getOrElse(whole)

      case EmptyIdentifier =>
        if index < text.length then (index, 1) else whole

      case LeadingZero =>
        def zero(start: Int, end: Int): Boolean = end - start > 1 && text.charAt(start) == '0'

        (prerelease ++ build).find { (start, end) => zero(start, end) && digits(start) == end }
        . orElse:
            numbers.find: (start, end) =>
              zero(start, end) && (start until end).exists(text.charAt(_) != '0')

        . map { (start, end) => (start, end - start) }
        . getOrElse(whole)

      case InvalidCharacter =>
        (prerelease ++ build).collectFirst:
          case (start, end) if start == end =>
            ((start - 1).max(0), 1)

          case (start, end) if (start until end).exists { index => !valid(text.charAt(index)) } =>
            ((start until end).find { index => !valid(text.charAt(index)) }.get, 1)

        . getOrElse(whole)

  def semver(context0: Expr[StringContext]): Macro[Semver] =
    import quotes.reflect.*
    import unsafeExceptions.canThrowAny

    val versionText = context0.valueOrAbort match
      case StringContext(text*) => text match
        case Seq(text: String)  => text
        case _                  => panic(m"did not expect more than one part in StringContext")

    // The version is read once, as the code compiles, and the code which is made constructs
    // it: nothing is read when it runs. A version which is not valid is reported for the
    // reason the decoder gives, at the place in the literal where that reason applies.
    val semver: Semver =
      try throwErrors[Semver.Error](versionText.tt.as[Semver]) catch case error: Semver.Error =>
        val (offset, length) = locate(versionText, error.reason)

        val position =
          Interpolation.sourcePosition
            ( List(versionText), Interpolation.literalOrigins(context0, 1), 0, offset, length )

        halt(97, 1, error.reason, m"the version is not valid: ${error.reason}", position)

    def identifiers(elements: List[Long | Text]): Expr[List[Long | Text]] =
      if elements.stdlib.isEmpty then '{Nil} else
        val lifted: scala.List[Expr[Long | Text]] = elements.stdlib.toList.map: element =>
          element.absolve match
            case number: Long => Expr(number)
            case text: Text   => '{${Expr(text.s)}.tt}

        '{List[Long | Text](${Varargs(lifted)}*)}

    ' {
        Semver
          ( ${Expr(semver.major)},
            ${Expr(semver.minor)},
            ${Expr(semver.patch)},
            ${identifiers(semver.prerelease)},
            ${identifiers(semver.build)} )
      }
