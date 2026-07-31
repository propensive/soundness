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
package decorum

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.config.Settings
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.{Diagnostic, Reporter}
import dotty.tools.dotc.util.SourceFile

object Parsing:
  // A Reporter that discards everything; we don't surface parser diagnostics
  // from Decorum — the main compiler will report parse errors itself.
  private class SilentReporter extends Reporter:
    def doReport(dia: Diagnostic)(using Context): Unit = ()

  // The language features the build enables globally (see `build.mill`).
  // The standalone parser must match the in-compiler parse — without
  // `relaxedLambdaSyntax`, for example, a one-line `f: x => body` colon
  // lambda misparses as a type ascription and misleads the tree rules.
  private val languageFeatures: List[String] =
    List
      ( "experimental.modularity", "experimental.dependent",
        "experimental.relaxedLambdaSyntax", "experimental.genericNumberLiterals",
        "experimental.into", "experimental.erasedDefinitions", "implicitConversions",
        "experimental.saferExceptions", "experimental.strictEqualityPatternMatching",
        "experimental.subCases", "experimental.multiSpreads" )

  // Parse the given source text into an untyped Scala 3 AST. Returns the
  // tree paired with its SourceFile. If parsing throws, returns the empty
  // tree — callers should treat that as "no structural info available".
  def parse(file: String, text: String): (untpd.Tree, SourceFile) =
    val source = SourceFile.virtual(file, text)
    try
      val base = new ContextBase
      val ctx  = base.initialCtx.fresh.setReporter(new SilentReporter).setSource(source)

      ctx.setSetting
        ( ctx.settings.language,
          languageFeatures.map(Settings.Setting.ChoiceWithHelp(_, "")) )

      val parser = new Parsers.Parser(source)(using ctx)
      (parser.parse(), source)
    catch case _: Throwable => (untpd.EmptyTree, source)
