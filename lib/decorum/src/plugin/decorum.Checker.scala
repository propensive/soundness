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
import dotty.tools.dotc.util.SourceFile

// The per-file entry point: build the shared `Context` and run every
// registry rule over it, in registry order. The registry order *is* the
// emission order — dotty's reporter keeps only the first diagnostic per
// position, so `Rules.all` documents (and preserves) the collision
// resolutions of the old per-line walk.
object Checker:

  // Test-friendly entry point: parses `rawText` standalone via `Parsing.parse`
  // before delegating to the tree-aware overload. The plugin should call the
  // overload below directly with the compilation unit's existing untyped
  // tree to avoid re-parsing.
  def check
    ( file:             String,
      expectedModule:   Option[String],
      rawText:          String,
      siblingTypes:     List[String] = Nil,
      siblingExtensions: List[String] = Nil,
      unexported:       Set[String] = Set.empty )
  :   LazyList[Violation] =

    val (tree, source) = Parsing.parse(file, rawText)
    check(file, expectedModule, rawText, tree, source, siblingTypes, siblingExtensions, unexported)

  def check
    ( file:             String,
      expectedModule:   Option[String],
      rawText:          String,
      untpdTree:        untpd.Tree,
      source:           SourceFile,
      siblingTypes:     List[String],
      siblingExtensions: List[String],
      unexported:       Set[String] )
  :   LazyList[Violation] =

    val ctx =
      Context
        ( file, expectedModule, rawText, untpdTree, source, siblingTypes, siblingExtensions,
          unexported )

    LazyList.from(Rules.all.flatMap(_.check(ctx)))

  def expectedModule(filePath: String): Option[String] =
    val parts = filePath.split("/lib/").nn

    if parts.length < 2 then None
    else
      val moduleDir = parts(1).nn.split("/").nn(0).nn
      val segments = filePath.split("/").nn
      val fileName = segments(segments.length - 1).nn

      val base =
        if fileName.endsWith(".scala")
        then fileName.substring(0, fileName.length - ".scala".length).nn
        else fileName
      // Cross-module export files (e.g. `soundness_serpentine_core.scala`,
      // `anticipation_serpentine_core.scala`) declare a different package
      // — the prefix before `_<module>_<suffix>`. Detect this pattern and
      // return that prefix as the expected package.

      val prefix = s"_${moduleDir}_"
      val idx    = base.indexOf(prefix)
      if idx > 0 then Some(base.substring(0, idx).nn) else Some(moduleDir)
