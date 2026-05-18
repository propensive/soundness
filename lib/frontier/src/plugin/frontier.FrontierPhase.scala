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
package frontier

import dotty.tools.dotc.*, ast.tpd, core.*, Constants.Constant, Contexts.*,
    Symbols.*, plugins.*

class FrontierPhase() extends PluginPhase:
  val phaseName: String                = "frontier"
  // Run AFTER `inlining`/`splicing` so the macro splice inside the
  // `internal.explanation` inline def has been fully expanded into a
  // `Sentinel.missing[T]("...")` call we can recognise. Typer alone leaves
  // the inline def as an unexpanded `Apply(explanation, ...)`; the splice
  // doesn't fire until the `splicing` phase.
  override val runsAfter: Set[String]  = Set("splicing")
  override val runsBefore: Set[String] = Set("pickleQuotes")

  // Walk each unit's typed tree for `frontier.Sentinel.missing[T](text)`
  // calls emitted by the catch-all macro in place of missing implicits and
  // emit the pre-rendered diagnostic via `report.error` at the sentinel's
  // source position. Reporting from this phase (rather than from the macro
  // itself via `report.errorAndAbort`) is what lets the user see a real
  // error: a macro abort emitted during nested implicit-search elaboration
  // would be buffered, but a `report.error` at this phase reaches the user.
  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] =
    val sentinelMissing: Symbol =
      Symbols.requiredModule("frontier.Sentinel").requiredMethod("missing")

    def extractStringLiteral(tree: tpd.Tree): Option[String] = tree match
      case tpd.Literal(Constant(text: String)) => Some(text)
      case tpd.Inlined(_, _, body)             => extractStringLiteral(body)
      case tpd.Block(_, expr)                  => extractStringLiteral(expr)
      case tpd.Typed(expr, _)                  => extractStringLiteral(expr)
      case _                                   => None

    units.foreach: unit =>
      val traverser = new tpd.TreeTraverser:
        def traverse(tree: tpd.Tree)(using Context): Unit =
          tree match
            case tpd.Apply(fun, List(arg)) if fun.symbol == sentinelMissing =>
              extractStringLiteral(arg) match
                case Some(text) => report.error(text, tree.sourcePos)
                case None       => traverseChildren(tree)
            case _ =>
              traverseChildren(tree)
      traverser.traverse(unit.tpdTree)
    units
