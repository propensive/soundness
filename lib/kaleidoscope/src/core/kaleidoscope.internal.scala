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

import java.util.regex.*

import scala.collection.immutable.Seq
import scala.language.experimental.pureFunctions
import scala.quoted.*

import anticipation.*
import contextual.*
import contingency.*
import denominative.*
import fulminate.*
import gigantism.*
import praxinoscope.*
import prepositional.*
import vacuous.*

import denominative.dysasymptotics.linearSize
import murmuration.map
import rudiments.total

object internal:
  transparent inline def expandRegexJvm(inline context: StringContext): Any =
    ${regexJvm('context)}

  transparent inline def expandRegexRe2(inline context: StringContext): Any =
    ${regexRe2('context)}

  transparent inline def expandGlobJvm(inline context: StringContext): Any =
    ${globJvm('context)}

  transparent inline def expandGlobRe2(inline context: StringContext): Any =
    ${globRe2('context)}

  def globJvm(context: Expr[StringContext]): Macro[Any] = glob(context, false)
  def globRe2(context: Expr[StringContext]): Macro[Any] = glob(context, true)
  def regexJvm(context: Expr[StringContext]): Macro[Any] = regex(context, false)
  def regexRe2(context: Expr[StringContext]): Macro[Any] = regex(context, true)

  private def glob(context: Expr[StringContext], re2Backend: Boolean): Macro[Any] =
    val parts = context.value.get.parts.map(Text(_)).map(Glob.parse(_).regex.s).to(List)

    // The parts have been transformed from glob to regex syntax, so parser offsets no longer
    // correspond to the source literal; unknown origins make errors fall back to the whole
    // expansion.
    val parts2 = parts.stdlib

    extractor
      ( (parts2.head :: parts2.tail.map("([^/\\\\]*)"+_)).to(List),
        List.fill(parts.size)((0, 0)),
        re2Backend )

  private def regex(context: Expr[StringContext], re2Backend: Boolean): Macro[Any] =
    val parts = context.value.get.parts.to(List)
    extractor(parts, Interpolation.literalOrigins(context, parts.size), re2Backend)

  private def extractor(parts: List[String], origins: List[(Int, Int)], re2Backend: Boolean)
  :   Macro[Any] =

    import quotes.reflect.*

    // A parser error's index refers to the parts joined with nothing standing in for the
    // substitutions, since a substitution binds the capture group that immediately follows it.
    // An index at end-of-input (an unclosed group, say) is clamped onto the last character.
    def fail(error: Regex.Error): Nothing =
      val length = parts.map(_.length).total
      val offset = error.index.min(length - 1).max(0)
      halt(error.labelled, Interpolation.sourcePosition(parts, origins, 0, offset))

    val regex =
      given Diagnostics = Diagnostics.omit

      given HaltTactic[Regex.Error, Regex] = new HaltTactic[Regex.Error, Regex]:
        override def abort(error: Diagnostics ?=> Regex.Error): Nothing = fail(error)

      Regex.parse(parts.map(Text(_)))

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

    try Pattern.compile(parts.stdlib.mkString) catch case exception: PatternSyntaxException =>
      import errorDiagnostics.emptyDiagnostics
      fail(Regex.Error(exception.getIndex, Regex.Error.Reason.InvalidPattern))

    // `import regexBackends.re2` at the use site switches the expansion to the praxinoscope
    // engine, in which case the pattern is validated against the RE2 subset here (so that
    // backreferences, lookaround and the like fail at compile time), compiled, and staged into
    // the emitted extractor, which pre-seeds the engine's cache at class load.
    val re2Motif: Option[Motif] =
      if !re2Backend then None else
        given Diagnostics = Diagnostics.omit

        given HaltTactic[Motif.Error, Motif] = new HaltTactic[Motif.Error, Motif]:
          override def abort(error: Diagnostics ?=> Motif.Error): Nothing =
            halt(error.labelled, Interpolation.sourcePosition(parts, origins, 0, 0))

        Some(Motif.parse(regex.plainPattern))

    val formType: TypeRepr = if re2Backend then TypeRepr.of[Re2] else TypeRepr.of[JavaBaseRegex]

    import praxinoscope.internal.motif

    val staged: Expr[Optional[Motif]] = re2Motif match
      case Some(compiled) => '{${Expr(compiled)}: Optional[Motif]}
      case None           => '{Unset}

    // For a captureless literal, additionally compile the whole-match test by subset
    // construction to a static DFA: a specialized, allocation-free matcher that neither
    // backtracks nor tracks threads.
    val fsa: Expr[Optional[Fsa]] = re2Motif match
      case Some(compiled) if types.length == 0 =>
        praxinoscope.internal.matcher(compiled.program) match
          case generated: Expr[Fsa] @unchecked =>
            val key = Expr(compiled.pattern.s)
            '{Fsa.of($key)($generated): Optional[Fsa]}

          case _ =>
            '{Unset}

      case _ =>
        '{Unset}

    formType.asType.absolve match
      case '[form] =>
        val engine: Expr[form is Regex.Engine] = Expr.summon[form is Regex.Engine].get

        if types.length == 0
        then '{new NoExtraction[form](${Expr(parts.stdlib.head)}, $staged, $fsa)(using $engine)}
        else tupleType.asType.absolve match
          case '[resultType] =>
            ' {
                new RExtractor[Option[resultType], form](${Expr(parts.stdlib)}, $staged)(using
                    $engine)
              }

  class NoExtraction[form]
    ( pattern: String,
      staged:  Optional[Motif] = Unset,
      matcher: Optional[Fsa] = Unset )
    ( using engine: form is Regex.Engine ):

    staged match
      case staged: Motif => Regex.Engine.install(staged)
      case _             => ()

    def apply(): Regex in form = Regex(List(pattern))(using Unsafe).to[form]

    def unapply(scrutinee: Text)(using scanner: Scanner): Boolean =
      scanner.nextStart match
        case index: Int =>
          engine.matches(Regex(List(pattern))(using Unsafe), scrutinee)

        case _ =>
          matcher.lay(engine.matches(Regex(List(pattern))(using Unsafe), scrutinee)): fsa =>
            fsa.matches(scrutinee)

  class RExtractor[result, form](parts: Seq[String], staged: Optional[Motif] = Unset)
    ( using engine: form is Regex.Engine ):

    staged match
      case staged: Motif => Regex.Engine.install(staged)
      case _             => ()

    def unapply(scrutinee: Text)(using scanner: Scanner): result =
      val result = engine.matchGroups(Regex(List.from(parts))(using Unsafe), scrutinee)
      val result2 = result.asInstanceOf[Option[Array[List[Text | Char] | Optional[Text | Char]]^{}]]

      if parts.length == 2
      then
        result2.map: (groups: Array[List[Text | Char] | Optional[Text | Char]]^{}) =>
          groups.readable.head

        . asInstanceOf[result]
      else
        result2.map: (groups: Array[List[Text | Char] | Optional[Text | Char]]^{}) =>
          Tuple.fromIArray(groups.readable)

        . asInstanceOf[result]
