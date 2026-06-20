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

      val unitTree          = context.compilationUnit.untpdTree
      val siblingTypes      = soundnessSiblingModules(path)
      val siblingExtensions = soundnessSiblingExtensions(path)
      val unexported        = soundnessUnexported(path) ++ soundnessSiblingSurfaceExports(path)

      val violations =
        Checker.check
          ( path, module, text, unitTree, source, siblingTypes, siblingExtensions, unexported )

      violations.foreach: violation =>
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

  // When `path` is a `soundness_<component>_<suffix>.scala` export surface, return
  // the names of the public top-level extension methods that R-742.1 requires to
  // be re-exported. They live in the sibling `<component>_<suffix>.scala` definition
  // files (e.g. `xylophone_core.scala`) in the same directory; their lowercase
  // `<component>_` prefix distinguishes them from the `soundness_…` and
  // `<other>_<component>_…` export surfaces, which begin with a different prefix.
  // `internal` definition files are skipped. Each matching file is parsed and its
  // top-level `extension` methods collected. For every file other than an export
  // surface the result is empty, making the check a no-op.
  private def soundnessSiblingExtensions(path: String): List[String] =
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
            val prefix = s"${component}_"
            val out    = mutable.ListBuffer[String]()
            siblings.foreach: sibling =>
              val name = sibling.nn.getName.nn
              if name.startsWith(prefix) && name.endsWith(".scala")
                 && !name.toLowerCase.nn.contains("internal")
              then out ++= extensionMethods(sibling.nn)
            out.distinct.to(List)

  // Parse `file` and return the leaf names of its public top-level extension
  // methods. Returns `Nil` if the file can't be read or parsed.
  private def extensionMethods(file: java.io.File): List[String] =
    try
      val content        = String(java.nio.file.Files.readAllBytes(file.toPath))
      val (tree, source) = Parsing.parse(file.getPath.nn, content)
      Extensions.extract(tree, source)
    catch case _: Throwable => Nil

  // When `path` is a `soundness_<component>_<suffix>.scala` export surface, return
  // the simple names of every sibling definition annotated `@unexported` — both in
  // the `<component>.<Name>.scala` module files and the `<component>_<suffix>.scala`
  // definition files in the same directory. R-742 and R-742.1 treat these names as
  // deliberately excluded from the `soundness` umbrella. Empty for every other file.
  private def soundnessUnexported(path: String): Set[String] =
    if path.contains("/src/plugin/") then Set.empty else
      val libParts = path.split("/lib/").nn
      if libParts.length < 2 then Set.empty else
        val component = libParts(1).nn.split("/").nn(0).nn
        val file      = java.io.File(path)
        val fileName  = file.getName.nn
        if !fileName.startsWith(s"soundness_${component}_") then Set.empty else
          val parent   = file.getParentFile
          val siblings = if parent == null then null else parent.listFiles
          if siblings == null then Set.empty else
            val out = mutable.Set[String]()
            siblings.foreach: sibling =>
              val name = sibling.nn.getName.nn
              if (name.startsWith(s"$component.") || name.startsWith(s"${component}_"))
                 && name.endsWith(".scala")
              then out ++= unexportedNames(sibling.nn)
            out.to(Set)

  // Parse `file` and return the simple names of definitions it annotates
  // `@unexported`. Returns the empty set if the file can't be read or parsed.
  private def unexportedNames(file: java.io.File): Set[String] =
    try
      val content        = String(java.nio.file.Files.readAllBytes(file.toPath))
      val (tree, source) = Parsing.parse(file.getPath.nn, content)
      Annotations.unexported(tree)
    catch case _: Throwable => Set.empty

  // When `path` is an export surface, return the export names from the *other*
  // `soundness_<component>_<suffix>.scala` surfaces in the same directory. A
  // component split across several surfaces (e.g. breviloquence's `core`/`parser`)
  // re-exports each module from exactly one of them; a module exported by a sibling
  // surface is therefore not missing from this one. Empty for non-surface files.
  private def soundnessSiblingSurfaceExports(path: String): Set[String] =
    if path.contains("/src/plugin/") then Set.empty else
      val libParts = path.split("/lib/").nn
      if libParts.length < 2 then Set.empty else
        val component = libParts(1).nn.split("/").nn(0).nn
        val file      = java.io.File(path)
        val fileName  = file.getName.nn
        if !fileName.startsWith(s"soundness_${component}_") then Set.empty else
          val parent   = file.getParentFile
          val siblings = if parent == null then null else parent.listFiles
          if siblings == null then Set.empty else
            val out = mutable.Set[String]()
            siblings.foreach: sibling =>
              val name = sibling.nn.getName.nn
              if name != fileName && name.startsWith(s"soundness_${component}_")
                 && name.endsWith(".scala")
              then out ++= surfaceExportNames(sibling.nn)
            out.to(Set)

  // Parse an export-surface `file` and return the simple names it re-exports.
  private def surfaceExportNames(file: java.io.File): Set[String] =
    try
      val content        = String(java.nio.file.Files.readAllBytes(file.toPath))
      val (tree, source) = Parsing.parse(file.getPath.nn, content)
      SoundnessExports.extract(tree, source).names
    catch case _: Throwable => Set.empty

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
