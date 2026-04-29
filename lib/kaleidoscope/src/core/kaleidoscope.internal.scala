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

import language.experimental.pureFunctions

import java.util.regex.*

import scala.quoted.*

import anticipation.*
import contingency.*
import fulminate.*
import gigantism.*
import proscenium.*
import vacuous.*

object internal:
  private given realm: Realm = realm"kd"

  def glob(context: Expr[StringContext]): Macro[Any] =
    val parts = context.value.get.parts.map(Text(_)).map(Glob.parse(_).regex.s).to(List)

    extractor(parts.head :: parts.tail.map("([^/\\\\]*)"+_))

  def regex(context: Expr[StringContext]): Macro[Any] =
    extractor(context.value.get.parts.to(List))

  private def extractor(parts: List[String]): Macro[Any] =
    import quotes.reflect.*

    val regex = abortive(Regex.parse(parts.map(Text(_))))

    val types: List[TypeRepr] = regex.captureGroups.map: group =>
      group.quantifier match
        case Regex.Quantifier.Exactly(1) =>
          if group.charClass then TypeRepr.of[Char] else TypeRepr.of[Text]

        case Regex.Quantifier.Between(0, 1) =>
          if group.charClass then TypeRepr.of[Optional[Char]] else TypeRepr.of[Optional[Text]]

        case _ =>
          if group.charClass then TypeRepr.of[List[Char]] else TypeRepr.of[List[Text]]

    // This needs to be `lazy`
    lazy val tupleType: TypeRepr =
      if types.length == 1 then types.head
      else AppliedType(defn.TupleClass(types.length).info.typeSymbol.typeRef, types)

    try Pattern.compile(parts.mkString) catch case exception: PatternSyntaxException =>
      import errorDiagnostics.empty
      halt(RegexError(exception.getIndex, RegexError.Reason.InvalidPattern).labelled)

    if types.length == 0 then '{NoExtraction(${Expr(parts.head)})}
    else tupleType.asType.runtimeChecked match
      case '[resultType] => '{RExtractor[Option[resultType]](${Expr(parts)})}

  class NoExtraction(pattern: String):
    inline def apply(): Regex = Regex(List(pattern))(using Unsafe)

    def unapply(scrutinee: Text)(using scanner: Scanner): Boolean =
      scanner.nextStart match
        case Unset => Regex(List(pattern))(using Unsafe).matches(scrutinee)

        case index: Int =>
          val regex = Regex(List(pattern))(using Unsafe)
          val matcher = regex.javaPattern.matcher(scrutinee.s).nn
          val found = matcher.find(index)
          if found then scanner.nextStart = matcher.start
          found

  class RExtractor[result](parts: Seq[String]):
    def unapply(scrutinee: Text)(using scanner: Scanner): result =
      val result = Regex(parts)(using Unsafe).matchGroups(scrutinee)
      val result2 = result.asInstanceOf[Option[IArray[List[Text | Char] | Optional[Text | Char]]]]

      if parts.length == 2 then result2.map(_.head).asInstanceOf[result]
      else result2.map(Tuple.fromIArray(_)).asInstanceOf[result]
