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
package decorum

import scala.collection.mutable

import dotty.tools.dotc.*, ast.tpd, core.Contexts.*, plugins.*, util.{SourceFile, SourcePosition}
import dotty.tools.dotc.util.Spans.Span

class DecorumPhase(options: List[String]) extends PluginPhase:
  val phaseName: String                = "decorum"
  override val runsAfter: Set[String]  = Set("typer")
  override val runsBefore: Set[String] = Set("pickler")

  private val errors: Boolean   = options.contains("errors")
  private val seen: mutable.Set[String] = mutable.Set.empty

  private val esc: Char = 27.toChar
  private val bel: Char = 7.toChar
  private val gray   = s"$esc[38;2;128;128;128m"
  private val orange = s"$esc[38;2;255;165;0m"
  private val yellow = s"$esc[38;2;255;215;0m"
  private val cyan   = s"$esc[38;2;0;200;255m"
  private val reset  = s"$esc[0m"

  private def colourPrefix(rule: String, useColor: Boolean): String =
    if useColor then
      val hyperlink = false
      val rendered  = rule.replace(".", s"$gray.$cyan")
      val d         = rule.takeWhile(_ != '.')
      val link      = if hyperlink then s"$esc]8;;https://soundness.dev/SN-de/$d$bel" else ""
      val unlink    = if hyperlink then s"$esc]8;;$bel" else ""
      s"$link$gray[$orange↯SN$gray-${yellow}de$gray/$cyan$rendered$gray]$reset$unlink "
    else
      s"[↯SN-de/$rule] "

  override def transformUnit(tree: tpd.Tree)(using context: Context): tpd.Tree =
    val source: SourceFile = context.compilationUnit.source
    val path: String       = source.file.path
    if seen.add(path) then
      val text: String = String(source.content)
      val module       = Checker.expectedModule(path)
      val useColor     =
        try
          import dotty.tools.dotc.config.Settings.Setting.value
          value(context.settings.color)(using context) != "never"
        catch case _: Throwable => false
      Checker.check(path, module, text).foreach: violation =>
        val pos = position(source, violation.line, violation.column)
        val msg = colourPrefix(violation.rule, useColor)+violation.message
        if errors then report.error(msg, pos) else report.warning(msg, pos)
    super.transformUnit(tree)

  private def position(source: SourceFile, line: Int, column: Int): SourcePosition =
    val lineStart =
      try source.lineToOffset((line - 1).max(0))
      catch case _: Throwable => 0

    val offset = (lineStart + (column - 1).max(0)).min(source.content.length)
    SourcePosition(source, Span(offset))
