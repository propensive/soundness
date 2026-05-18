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

import scala.collection.mutable

import dotty.tools.dotc.*, ast.tpd, core.*, Constants.Constant, Contexts.*,
    Flags.*, Symbols.*, Types.*, plugins.*

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
  // calls emitted by the catch-all macro in place of missing implicits. For
  // each sentinel, walk *up* through the surrounding Apply ancestors as
  // long as they are `given`-method calls (the inserted-implicit chain) and
  // render a "full chain" diagnostic at the outermost user-source call.
  // When the sentinel has no `given` ancestor — a direct `summon[X]` with
  // no chain — relay the macro's pre-rendered text unchanged.
  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] =
    val sentinelMissing: Symbol =
      Symbols.requiredModule("frontier.Sentinel").requiredMethod("missing")

    units.foreach { unit => processUnit(unit, sentinelMissing) }
    units

  private def processUnit(unit: CompilationUnit, sentinelMissing: Symbol)
                         (using Context): Unit =
    val stack: mutable.ArrayBuffer[tpd.Apply] = mutable.ArrayBuffer.empty

    val traverser = new tpd.TreeTraverser:
      def traverse(tree: tpd.Tree)(using Context): Unit =
        tree match
          case apply @ tpd.Apply(fun, List(arg))
          if fun.symbol == sentinelMissing =>
            handleSentinel(apply, arg, stack.toList)
          case apply: tpd.Apply =>
            stack += apply
            try traverseChildren(apply)
            finally stack.remove(stack.size - 1)
          case _ =>
            traverseChildren(tree)

    traverser.traverse(unit.tpdTree)

  // Process a sentinel call: render the chain that surrounds it and emit
  // `report.error` at the appropriate position.
  private def handleSentinel(sentinel: tpd.Apply, arg: tpd.Tree, ancestors: List[tpd.Apply])
                            (using Context): Unit =
    val macroText = extractStringLiteral(arg).getOrElse("")
    val sentinelType: Type = sentinel.tpe.widen

    // Collect the suffix of `ancestors` whose function symbols are `given`s
    // — that is the implicit chain the typer wove around the sentinel. The
    // outermost link's *result type* is the type the user originally
    // summoned, because `summon[T]` is `inline` and has been folded away by
    // the time this phase runs.
    val chain: List[tpd.Apply] = collectChain(ancestors)

    if chain.isEmpty then
      // No implicit chain — direct `summon[X]` (X had no candidate but the
      // catch-all). Relay the macro's already-comprehensive text at the
      // sentinel's position.
      report.error(macroText, sentinel.sourcePos)
    else
      val outerType: Type = chain.head.tpe.widen
      val pos = chain.head.sourcePos
      val rendered = renderChain(outerType, chain, sentinelType)
      report.error(rendered, pos)

  // Walk the ancestor stack from innermost outwards, collecting consecutive
  // `given`-method Applies (the implicit chain). Returns the chain in
  // outermost-first order.
  private def collectChain(ancestors: List[tpd.Apply])(using Context): List[tpd.Apply] =
    ancestors.reverse.takeWhile(_.fun.symbol.is(Given)).reverse

  // Plain-text rendering of a chain. Outermost type at the top, each
  // candidate in the chain indented one step deeper than the previous,
  // ending with the missing type the sentinel stood in for. Deliberately
  // dependency-free: uses `Type.show` rather than stenography, plain
  // characters rather than escapade ANSI.
  private def renderChain(outerType: Type, chain: List[tpd.Apply], missing: Type)
                         (using Context): String =
    val builder = new StringBuilder
    builder.append("contextual value not found\n\n")
    builder.append(s" ■ resolving ${outerType.show}\n")
    val indent = StringBuilder("  ")
    chain.foreach: link =>
      builder.append(s"$indent ▪ candidate ${link.fun.symbol.name.show}\n")
      indent.append("  ")
    builder.append(s"$indent ✗ requires ${missing.show}\n")
    builder.toString

  private def extractStringLiteral(tree: tpd.Tree): Option[String] = tree match
    case tpd.Literal(Constant(text: String)) => Some(text)
    case tpd.Inlined(_, _, body)             => extractStringLiteral(body)
    case tpd.Block(_, expr)                  => extractStringLiteral(expr)
    case tpd.Typed(expr, _)                  => extractStringLiteral(expr)
    case _                                   => None
