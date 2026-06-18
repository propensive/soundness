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

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.util.SourceFile

case class ExportInfo(names: Set[String], excluded: Set[String], firstLine: Int)

object SoundnessExports:
  // A directive comment, `// unexported: Foo, Bar`, marks modules that are
  // deliberately not re-exported into `soundness` (typically because the simple
  // name already belongs to another component's surface). R-742 treats these as
  // satisfied. The capture stops at end of line or an opening parenthesis, so a
  // trailing justification (`// unexported: Foo (clashes with bar.Foo)`) is fine.
  private val ExcludeDirective = "(?m)//\\s*unexported:\\s*([^\\n(]+)".r
  private val Identifier       = "[A-Za-z][A-Za-z0-9]*".r

  // Collect the simple leaf names re-exported by every top-level `export`
  // statement in the file (including those nested inside `package x:` blocks),
  // together with the line of the first such statement. Used by R-742 to check
  // that each public module in a component is re-exported into `soundness`.
  def extract(tree: untpd.Tree, source: SourceFile): ExportInfo =
    val names     = mutable.Set[String]()
    var firstLine = -1

    def record(exp: untpd.Export): Unit =
      val span = exp.span
      if span.exists then
        val line = source.offsetToLine(span.start) + 1
        if firstLine < 0 || line < firstLine then firstLine = line

      exp.selectors.foreach: selector =>
        val name = selector.imported.name.toString
        if name.nonEmpty && name != "_" && name != "*" then names += name

    def visit(t: untpd.Tree): Unit = t match
      case pkg: untpd.PackageDef => pkg.stats.foreach(visit)
      case exp: untpd.Export     => record(exp)
      case _                     => ()

    visit(tree)

    val content  = String(source.content)
    val excluded = mutable.Set[String]()
    ExcludeDirective.findAllMatchIn(content).foreach: directive =>
      Identifier.findAllIn(directive.group(1).nn).foreach(excluded += _)

    ExportInfo(names.to(Set), excluded.to(Set), if firstLine < 0 then PackageLine else firstLine)

  // The line of the `package soundness` declaration in export-surface files;
  // used as the fallback violation position when a surface contains no exports.
  private val PackageLine: Int = 33
