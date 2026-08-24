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
package kaleidoscope

import java.util.regex as jur

import scala.language.experimental.pureFunctions

import anticipation.*
import denominative.*
import rudiments.*
import vacuous.*

object Jur:
  // In the subject type's companion (issue #1632), so `Regex in Jur` operations resolve with no
  // import. The bodies previously lived as methods on `Regex` itself; they move here so that a
  // `Regex in Re2` cannot silently fall back to `java.util.regex`.
  given engine: Jur is Regex.Engine:
    def matches(regex: Regex, text: Text)(using scanner: Scanner): Boolean =
      scanner.nextStart match
        case index: Int =>
          val matcher = regex.javaPattern.matcher(text.s).nn
          val found = matcher.find(index)

          if found then
            scanner.nextStart = matcher.start + 1
            scanner.matchEnd = matcher.end

          found

        case _ =>
          regex.javaPattern.matcher(text.s).nn.matches

    def seek(regex: Regex, input: Text, start: Ordinal): Optional[Interval] =
      val matcher: jur.Matcher = regex.javaPattern.matcher(input.s).nn
      if matcher.find(start.n0) then Interval.zerary(matcher.start, matcher.end) else Unset

    def search(regex: Regex, input: Text, start: Ordinal, overlap: Boolean): Chain[Interval] =
      val matcher: jur.Matcher = regex.javaPattern.matcher(input.s).nn

      def recur(offset: Int): Chain[Interval] =
        if offset > input.s.length then Chain()
        else if matcher.find(offset)
        then
          Interval.zerary(matcher.start, matcher.end) #::
            recur((if overlap then matcher.start else matcher.end) + 1)
        else
          Chain()

      recur(start.n0)


    private[kaleidoscope] def matchGroups(regex: Regex, text: Text)(using scanner: Scanner)
    :   Option[Array[List[Text | Char] | Optional[Text | Char]]^{}] =

      val matcher: jur.Matcher = regex.javaPattern.matcher(text.s).nn


      def recur
        ( todo:    List[Regex.Group],
          matches: List[Optional[Text | Char] | List[Text | Char]],
          index:   Int )
      :   List[Optional[Text | Char] | List[Text | Char]] =

        todo.absolve match
          case Nil => matches

          case group :: tail =>
            val matchedText = matcher.group(s"g$index").nn

            val matches2 =
              if group.capture then
                if group.charMatcher then
                  if group.quantifier.unitary then matchedText.head :: matches
                  else if group.quantifier == Regex.Quantifier.Between(0, 1)
                  then matchedText.headOption.getOrElse(Unset) :: matches
                  else matchedText.toCharArray.nn.iterator.to(List) :: matches
                else

                if group.quantifier.unitary then matcher.group(s"g$index").nn.tt :: matches
                else if group.charClass then
                  matchedText.toCharArray.nn.iterator.to(List) :: matches
                else
                  val subpattern = regex.pattern.s.substring(group.start, group.end).nn

                  val compiled =
                    Regex.cache.getOrElseUpdate(subpattern, jur.Pattern.compile(subpattern).nn)

                  val submatcher = compiled.matcher(matchedText).nn
                  var submatches: List[Text] = Nil

                  while submatcher.find()
                  do submatches ::= submatcher.toMatchResult.nn.group(0).nn.tt

                  if group.quantifier == Regex.Quantifier.Between(0, 1)
                  then submatches.prim :: matches
                  else submatches.reverse :: matches

              else
                matches

            recur(tail, matches2, index + 1)


      scanner.nextStart match
        case index: Int =>
          if !matcher.find(index) then None else
            scanner.nextStart = matcher.start + 1
            scanner.matchEnd = matcher.end
            val groups = recur(regex.captureGroups, Nil, 0).stdlib.reverse
            Some(Array.frozen(scala.IArray.from(groups)))

        case _ =>
          if !matcher.matches then None else
            val groups = recur(regex.captureGroups, Nil, 0).stdlib.reverse
            Some(Array.frozen(scala.IArray.from(groups)))

// The phantom marker for the `java.util.regex` backend (or, on JS and Native, the platform's
// emulation of it): the default `Form` of every `Regex`.
sealed trait Jur
