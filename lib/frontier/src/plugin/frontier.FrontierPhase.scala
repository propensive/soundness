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

import dotty.tools.dotc.core.TypeError

class FrontierPhase() extends PluginPhase:
  val phaseName: String                = "frontier"
  // Run AFTER `pickleQuotes` (which comes after `splicing`) so quoted
  // expression bodies — e.g. the literal `'{Sentinel.missing[T](...)}`
  // inside the macro's *own source* — are pickled into opaque blobs
  // before we walk the tree. Without that, the plugin matches the Apply
  // nodes inside those quotes when compiling `frontier.core` itself and
  // treats them as real sentinel calls. After pickleQuotes, only
  // expanded user-site Apply nodes remain.
  override val runsAfter: Set[String]  = Set("pickleQuotes")
  override val runsBefore: Set[String] = Set("crossVersionChecks")

  // Walk each unit's typed tree for `frontier.Sentinel.missing[T](pretty,
  // tree)` calls emitted by the catch-all macro in place of missing
  // implicits. For each sentinel, walk up through `given`-method ancestors
  // (the implicit chain the typer wove) and render a full chain at the
  // outermost user-source call. For direct `summon[X]` (no chain) relay
  // the macro's pretty text unchanged.
  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] =
    // The plugin is no-op when `frontier.Sentinel.missing` isn't on the
    // compile classpath — i.e., when the compilation unit doesn't depend on
    // frontier.plugin. (We're still registered as a phase on every unit
    // because the plugin is on `-Xplugin:`, but units that don't transitively
    // depend on frontier won't have any sentinel calls to find.)
    val sentinelMissingOpt: Option[Symbol] =
      try Some(Symbols.requiredModule("frontier.Sentinel").requiredMethod("missing"))
      catch case _: TypeError => None

    sentinelMissingOpt.foreach: sentinelMissing =>
      units.foreach { unit => processUnit(unit, sentinelMissing) }
    units

  private def processUnit(unit: CompilationUnit, sentinelMissing: Symbol)
                         (using Context): Unit =
    val stack: mutable.ArrayBuffer[tpd.Apply] = mutable.ArrayBuffer.empty

    val traverser = new tpd.TreeTraverser:
      def traverse(tree: tpd.Tree)(using Context): Unit =
        tree match
          case apply @ tpd.Apply(fun, List(prettyArg, treeArg))
          if fun.symbol == sentinelMissing =>
            handleSentinel(apply, prettyArg, treeArg, stack.toList)
          case apply: tpd.Apply =>
            stack += apply
            try traverseChildren(apply)
            finally stack.remove(stack.size - 1)
          case _ =>
            traverseChildren(tree)

    traverser.traverse(unit.tpdTree)

  // Process a sentinel call: render the chain that surrounds it and emit
  // `report.error` at the appropriate position. The macro packs two
  // strings into the sentinel — the already-rendered pretty diagnostic
  // (used verbatim for direct `summon[X]`), and a tab-separated
  // serialisation of the seek tree (used to surface alternatives even
  // when the sentinel is buried inside an implicit chain).
  private def handleSentinel
              (sentinel: tpd.Apply,
               prettyArg: tpd.Tree,
               treeArg: tpd.Tree,
               ancestors: List[tpd.Apply])
              (using Context)
              : Unit =
    val macroText = extractStringLiteral(prettyArg).getOrElse("")
    val serialisedTree = extractStringLiteral(treeArg).getOrElse("")
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
      val rendered = renderChain(outerType, chain, sentinelType, serialisedTree)
      report.error(rendered, pos)

  // Plain-text rendering of the chain the typer actually took, with the
  // deepest type's seek tree (parsed from the macro's pickled result)
  // appended under the final `✗ requires` line. The chain comes from
  // walking the typed tree; the deepest-type alternatives come from the
  // macro's seek (which ran during typer with full local scope).
  private def renderChain(outerType: Type,
                          chain: List[tpd.Apply],
                          missing: Type,
                          serialisedTree: String)
                         (using Context): String =
    val builder = new StringBuilder
    builder.append("contextual value not found\n\n")
    builder.append(s" ■ resolving ${outerType.show}\n")
    val indent = new StringBuilder("  ")
    chain.foreach: link =>
      builder.append(s"$indent ▪ candidate ${link.fun.symbol.name.show}\n")
      indent.append("  ")
    builder.append(s"$indent ✗ requires ${missing.show}\n")

    // Append the deepest-type's alternative candidates (proposals and
    // tried candidates from the macro's seek) at one extra indent under
    // the `requires` line, skipping the root Missing node since it
    // duplicates `requires`.
    val deeperIndent = indent.toString + "  "
    Diagnose.parse(serialisedTree).foreach: root =>
      root.children.foreach: child =>
        builder.append(Diagnose.render(child, deeperIndent))

    builder.toString

  // Walk the ancestor stack from innermost outwards, collecting consecutive
  // `given`-method Applies (the implicit chain). Returns the chain in
  // outermost-first order.
  private def collectChain(ancestors: List[tpd.Apply])(using Context): List[tpd.Apply] =
    ancestors.reverse.takeWhile(_.fun.symbol.is(Given)).reverse

  private def extractStringLiteral(tree: tpd.Tree): Option[String] = tree match
    case tpd.Literal(Constant(text: String)) => Some(text)
    case tpd.Inlined(_, _, body)             => extractStringLiteral(body)
    case tpd.Block(_, expr)                  => extractStringLiteral(expr)
    case tpd.Typed(expr, _)                  => extractStringLiteral(expr)
    case _                                   => None
