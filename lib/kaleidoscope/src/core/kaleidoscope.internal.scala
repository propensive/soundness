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

import scala.collection.immutable.Seq

import proscenium.compat.*

import scala.language.experimental.pureFunctions

import java.util.regex.*

import scala.quoted.*

import anticipation.*
import contextual.*
import contingency.*
import fulminate.*
import gigantism.*
import vacuous.*
import denominative.*
import denominative.asymptotics.linearSizeComplexity

object internal:
  def glob(context: Expr[StringContext]): Macro[Any] =
    val parts = context.value.get.parts.map(Text(_)).map(Glob.parse(_).regex.s).to(List)

    // The parts have been transformed from glob to regex syntax, so parser offsets no longer
    // correspond to the source literal; unknown origins make errors fall back to the whole
    // expansion.
    val parts2 = parts.stdlib
    extractor
      ( List.of(parts2.head :: parts2.tail.map("([^/\\\\]*)"+_)),
        List.fill(parts.size)((0, 0)) )

  def regex(context: Expr[StringContext]): Macro[Any] =
    val parts = context.value.get.parts.to(List)
    extractor(parts, Interpolation.literalOrigins(context, parts.size))

  private def extractor(parts: List[String], origins: List[(Int, Int)]): Macro[Any] =
    import quotes.reflect.*

    // A parser error's index refers to the parts joined with nothing standing in for the
    // substitutions, since a substitution binds the capture group that immediately follows it.
    // An index at end-of-input (an unclosed group, say) is clamped onto the last character.
    def fail(error: Regex.Error): Nothing =
      val length = parts.stdlib.map(_.length).sum
      val offset = error.index.min(length - 1).max(0)
      halt(error.labelled, Interpolation.sourcePosition(parts, origins, 0, offset))

    val regex =
      given Diagnostics = Diagnostics.omit

      given HaltTactic[Regex.Error, Regex] = new HaltTactic[Regex.Error, Regex]:
        override def abort(error: Diagnostics ?=> Regex.Error): Nothing = fail(error)

      Regex.parse(List.of(parts.stdlib.map(Text(_))))

    val types = regex.captureGroups.stdlib.map: group =>
      group.quantifier match
        case Regex.Quantifier.Exactly(1) =>
          if group.charMatcher then TypeRepr.of[Char] else TypeRepr.of[Text]

        case Regex.Quantifier.Between(0, 1) =>
          if group.charMatcher then TypeRepr.of[Optional[Char]] else TypeRepr.of[Optional[Text]]

        case _ =>
          if group.charMatcher then TypeRepr.of[List[Char]] else TypeRepr.of[List[Text]]

    // This needs to be `lazy`
    lazy val tupleType: TypeRepr =
      if types.length == 1 then types.head
      else AppliedType(defn.TupleClass(types.length).info.typeSymbol.typeRef, types)

    try Pattern.compile(parts.mkString) catch case exception: PatternSyntaxException =>
      import errorDiagnostics.emptyDiagnostics
      fail(Regex.Error(exception.getIndex, Regex.Error.Reason.InvalidPattern))

    if types.length == 0 then '{NoExtraction(${Expr(parts.head)})}
    else tupleType.asType.absolve match
      case '[resultType] => '{RExtractor[Option[resultType]](${Expr(parts.stdlib)})}

  class NoExtraction(pattern: String):
    inline def apply(): Regex = Regex(List(pattern))(using Unsafe)

    def unapply(scrutinee: Text)(using scanner: Scanner): Boolean =
      scanner.nextStart match
        case index: Int =>
          val regex = Regex(List(pattern))(using Unsafe)
          val matcher = regex.javaPattern.matcher(scrutinee.s).nn
          val found = matcher.find(index)

          if found then
            scanner.nextStart = matcher.start + 1
            scanner.matchEnd = matcher.end

          found

        case _ =>
          Regex(List(pattern))(using Unsafe).matches(scrutinee)

  class RExtractor[result](parts: Seq[String]):
    def unapply(scrutinee: Text)(using scanner: Scanner): result =
      val result = Regex(List.from(parts))(using Unsafe).matchGroups(scrutinee)
      val result2 = result.asInstanceOf[Option[Array[List[Text | Char] | Optional[Text | Char]]^{}]]

      if parts.length == 2
      then result2.map { (groups: Array[List[Text | Char] | Optional[Text | Char]]^{}) => groups.head }.asInstanceOf[result]
      else
        result2.map { (x: Array[List[Text | Char] | Optional[Text | Char]]^{}) =>
          Tuple.fromIArray(x.readable) }
        . asInstanceOf[result]
