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
  private val cyan   = s"$esc[38;2;0;200;255m"
  private val reset  = s"$esc[0m"

  private def colourPrefix(rule: String, useColor: Boolean): String =
    if useColor then
      val hyperlink = false
      val rendered  = rule.replace(".", s"$gray.$cyan")
      val d         = rule.takeWhile(_ != '.')
      val link      = if hyperlink then s"$esc]8;;https://soundness.dev/SN-$d$bel" else ""
      val unlink    = if hyperlink then s"$esc]8;;$bel" else ""
      s"$link$gray[$orange↯SN$gray-$cyan$rendered$gray]$reset$unlink "
    else
      s"[↯SN-$rule] "

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

      val unitTree     = context.compilationUnit.untpdTree
      val siblingTypes = soundnessSiblingModules(path)
      Checker.check(path, module, text, unitTree, source, siblingTypes).foreach: violation =>
        val pos = position(source, violation.line, violation.column)
        val msg = colourPrefix(violation.rule, useColor)+violation.message
        if errors then report.error(msg, pos) else report.warning(msg, pos)
    super.transformUnit(tree)

  // When `path` is a `soundness_<component>_<suffix>.scala` export surface, return
  // the names of the sibling public modules (`<component>.<Name>.scala` files in
  // the same directory, excluding any `internal` module) that R-742 requires to
  // be re-exported. A module is included only when its file declares `Name`
  // publicly: modules with no accessible top-level declaration (e.g. a
  // `private[component] object`) cannot be — and need not be — re-exported.
  // Compiler-plugin source roots (`/src/plugin/`) are excluded: their extraction
  // helpers are deliberately unexported plugin internals. For every other file
  // the result is empty, making the check a no-op.
  private def soundnessSiblingModules(path: String): List[String] =
    if path.contains("/src/plugin/") then Nil else
      val libParts = path.split("/lib/").nn
      if libParts.length < 2 then Nil else
        val component = libParts(1).nn.split("/").nn(0).nn
        val file      = java.io.File(path)
        val fileName  = file.getName.nn
        if !fileName.startsWith(s"soundness_${component}_") then Nil else
          val parent   = file.getParentFile
          val siblings = if parent == null then null else parent.listFiles
          if siblings == null then Nil else
            val prefix = s"$component."
            val out    = mutable.ListBuffer[String]()
            siblings.foreach: sibling =>
              val name = sibling.nn.getName.nn
              if name.startsWith(prefix) && name.endsWith(".scala") then
                val mid    = name.substring(prefix.length, name.length - ".scala".length).nn
                val module = mid.takeWhile(_ != '.')
                if module.nonEmpty && !module.toLowerCase.nn.contains("internal")
                   && declaresPublicly(sibling.nn, module)
                then out += module
            out.to(List)

  // Modifiers that may precede a public top-level declaration; `private` and
  // `protected` are deliberately absent, so a `private[x] object Foo` line never
  // matches the public-declaration pattern below.
  private val PublicModifiers =
    "(?:final|sealed|abstract|case|open|transparent|inline|erased|lazy|override|implicit)"

  // Does `file` declare `module` with a publicly-accessible top-level definition?
  // A column-0 `object`/`class`/`trait`/`enum`/`type`/`val`/`def`/`given Name`
  // (optionally preceded by non-access modifiers) counts; a `private`/`protected`
  // declaration does not, because the line then begins with that access modifier.
  private def declaresPublicly(file: java.io.File, module: String): Boolean =
    val keywords = "opaque\\s+type|object|class|trait|enum|type|val|def|given"
    val pattern  = s"(?m)^(?:$PublicModifiers\\s+)*(?:$keywords)\\s+$module(?![A-Za-z0-9])"
    try
      val content = String(java.nio.file.Files.readAllBytes(file.toPath))
      pattern.r.findFirstIn(content).isDefined
    catch case _: Throwable => true

  private def position(source: SourceFile, line: Int, column: Int): SourcePosition =
    val lineStart =
      try source.lineToOffset((line - 1).max(0))
      catch case _: Throwable => 0

    val offset = (lineStart + (column - 1).max(0)).min(source.content.length)
    SourcePosition(source, Span(offset))
