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
package beneficence

import scala.collection.mutable

import dotty.tools.dotc.*, ast.tpd, core.*, Contexts.*, Flags.*, plugins.*

class GivensPhase() extends PluginPhase:
  val phaseName: String                = "beneficenceGivens"
  override val runsAfter: Set[String]  = Set("typer")
  override val runsBefore: Set[String] = Set("pickler")

  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] =
    val collected: mutable.LinkedHashMap[String, mutable.Buffer[Entry]] =
      mutable.LinkedHashMap.empty

    val sources: mutable.LinkedHashSet[String] = mutable.LinkedHashSet.empty

    units.foreach: unit =>
      sources += unit.source.file.path
      collectGivens(unit, collected)

    GivensWriter.merge(collected, sources.toSet)
    units

  private def collectGivens
                ( unit:      CompilationUnit,
                  collected: mutable.LinkedHashMap[String, mutable.Buffer[Entry]] )
                (using Context)
  :     Unit =

    val sourceFile = unit.source.file.path

    val traverser = new tpd.TreeTraverser:
      def traverse(tree: tpd.Tree)(using Context): Unit =
        tree match
          case d: tpd.ValDef if eligible(d.symbol) => record(d.symbol, d.tpt.tpe)
          case d: tpd.DefDef if eligible(d.symbol) => record(d.symbol, d.tpt.tpe)
          case _                                   => ()
        traverseChildren(tree)

      private def eligible(symbol: Symbols.Symbol)(using Context): Boolean =
        symbol.flags.is(Given) && isStablyAccessible(symbol)

      // A given is stably accessible if every enclosing scope on the path from
      // the root is a package or a module (object). Givens nested inside a
      // non-module class, a trait, a method body, or a refinement aren't usable
      // via a plain `import` — listing them would just be noise.
      private def isStablyAccessible(symbol: Symbols.Symbol)(using Context): Boolean =
        var owner = symbol.owner
        var stable = true
        while stable && owner.exists && owner != Symbols.defn.RootClass do
          if owner.is(Method) then stable = false
          else if owner.isClass && !owner.is(Module) && !owner.is(Package) then stable = false
          else owner = owner.owner
        stable

      private def record(symbol: Symbols.Symbol, tpe: Types.Type)(using Context): Unit =
        val typeclassSymbol = typeclassOf(tpe, symbol)
        if typeclassSymbol.exists then
          val typeclassFqn = sourcePath(typeclassSymbol)
          val givenFqn     = sourcePath(symbol)
          val buffer       = collected.getOrElseUpdate(typeclassFqn, mutable.Buffer())
          buffer += Entry(givenFqn, sourceFile)

      private def typeclassOf(tpe: Types.Type, valSymbol: Symbols.Symbol)(using Context)
      :     Symbols.Symbol =

        // For a "normal" given like `given foo: Show[Int] = ...` baseClasses leads with
        // the typeclass class. For a modular `given X: T:` (with a refining body), the
        // typer creates a synthesized class named after the val (a `module class X$`
        // for value-style givens, or a plain `class X` for parameterised ones). In both
        // shapes, stripping the trailing `$` from the class name and comparing with the
        // val's name detects the synth class so we can skip past it to T.
        val valName = valSymbol.name.toString

        tpe.baseClasses.iterator.filter: cls =>
          cls.exists
          && cls != Symbols.defn.ObjectClass
          && cls != Symbols.defn.AnyClass
          && cls != Symbols.defn.MatchableClass
          && stripDollar(cls.name.toString) != valName
        .nextOption().getOrElse(tpe.dealias.classSymbol)

      private def stripDollar(name: String): String =
        if name.endsWith("$") then name.substring(0, name.length - 1).nn else name

      private def sourcePath(symbol: Symbols.Symbol)(using Context): String =
        val raw: String = symbol.fullName.toString.replace("$.", ".").nn
        if raw.endsWith("$") then raw.substring(0, raw.length - 1).nn else raw

    traverser.traverse(unit.tpdTree)
