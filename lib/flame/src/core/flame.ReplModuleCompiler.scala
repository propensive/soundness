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
package flame

import language.adhocExtensions

import dotty.tools.dotc as dtd
import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.CompilationUnitInfo
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.Scopes.newScope
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.quoted.PickledQuotes
import dotty.tools.dotc.quoted.QuotesCache
import dotty.tools.dotc.reporting.Diagnostic
import dotty.tools.dotc.reporting.HideNonSensicalMessages
import dotty.tools.dotc.reporting.Reporter
import dotty.tools.dotc.reporting.UniqueMessagePositions
import dotty.tools.dotc.transform.Inlining
import dotty.tools.dotc.transform.PickleQuotes
import dotty.tools.dotc.transform.Pickler
import dotty.tools.dotc.transform.PostTyper
import dotty.tools.dotc.transform.SetRootTree
import dotty.tools.dotc.transform.Splicing
import dotty.tools.dotc.transform.Staging
import dotty.tools.dotc.util.NoSource
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.Spans.Span

import ambience.*
import anticipation.*
import gossamer.*
import hellenism.*

// A compilation unit carrying a pickled (TASTy) block of member definitions to
// be lifted into a named top-level object (see `ReplModuleCompiler`). The pickled
// form is plain data, so it crosses freely from macro-expansion time to runtime —
// sidestepping the staging-level restriction on the `Expr` surface API.
class ModuleUnit(val moduleName: String, val tasty: List[String])
extends CompilationUnit(NoSource, null)

object ReplModuleCompiler:
  def compile
    ( classpath: LocalClasspath )
    ( moduleName: Text, out: Text )
    ( pickled: List[String] )
    ( using System )
  :   List[Text] =

    val messages = scala.collection.mutable.ListBuffer[Text]()

    object reporter extends Reporter, UniqueMessagePositions, HideNonSensicalMessages:
      def doReport(diagnostic: Diagnostic)(using Context): Unit =
        messages += diagnostic.toString.tt

    object driver extends dtd.Driver:
      val currentContext =
        val context = QuotesCache.init(initCtx.fresh)
        val arguments: List[Text] = List(t"-d", out, t"-classpath", classpath(), t"")
        setup(arguments.map(_.s).to(Array), context).map(_(1)).get

      def run(): List[Text] =
        given Context = currentContext.fresh.setReporter(reporter)
        ReplModuleCompiler().newRun.compileUnits(ModuleUnit(moduleName.s, pickled) :: Nil)
        messages.to(List)

    driver.run()

// A purpose-built compiler (modelled on `scala.quoted.staging.QuoteCompiler`)
// that takes a closed quoted block of definitions and emits a normal, importable
// top-level `object <moduleName>` containing those definitions as members,
// writing class files (and TASTy) to the configured output directory. Unlike the
// staging compiler — which wraps a single expression in a fixed-named class and
// reads a value back — this produces named bytecode that the REPL's
// text-compiled lines can `export`.
class ReplModuleCompiler extends dtd.Compiler:
  override protected def frontendPhases: List[List[Phase]] =
    List(ModuleFrontend()) :: List(PostTyper()) :: List(SetRootTree()) :: Nil

  override protected def picklerPhases: List[List[Phase]] =
    List(Pickler()) :: List(Inlining()) :: List(Staging()) :: List(Splicing())
    :: List(PickleQuotes()) :: Nil

  class ModuleFrontend extends Phase:
    import tpd.*

    def phaseName: String = "replModuleFrontend"

    def run(using Context): Unit = _root_.dotty.tools.unsupported("run")

    override def runOn(units: List[CompilationUnit])(using ctx: Context): List[CompilationUnit] =
      units.flatMap:
        case unit: ModuleUnit =>
          val unitCtx: Context = ctx.fresh.setPhase(this.start.nn).setCompilationUnit(unit)
          val definitions = unitCtx.definitions
          val span: Span = Span(0)

          // The module's class symbol; its scope (`decls`) is populated with the
          // unpickled members below, making them visible to `export` and codegen.
          val decls = newScope(using unitCtx)

          val moduleSymbol =
            newCompleteModuleSymbol
              ( definitions.RootClass,
                unit.moduleName.toTermName,
                Module | Final,
                Module | Final,
                definitions.ObjectType :: Nil,
                decls,
                coord = span )
              ( using unitCtx )

          val module = moduleSymbol.entered(using unitCtx)
          val moduleClass = module.moduleClass.asClass

          // Unpickle the TASTy block with the module class as owner, so each
          // definition is owned by the module; then enter them as members. The
          // block is closed, so empty type/term holes suffice.
          val ownerCtx = unitCtx.withOwner(moduleClass)
          val noTypes = PickledQuotes.TypeHole.V2(null)
          val noExprs = PickledQuotes.ExprHole.V2(null)
          val tree = PickledQuotes.unpickleTerm(unit.tasty, noTypes, noExprs)(using ownerCtx)

          // Unwrap to the pickled block and re-own its members onto the module
          // class — they were pickled under the macro's owner, and the backend
          // emits each method under its symbol's owner, so this must match the
          // class the members are declared in. Re-owning the whole block at once
          // keeps cross-references between members consistent.
          def unwrap(tree: Tree): Tree = tree match
            case Inlined(_, _, inner) => unwrap(inner)
            case other                => other

          val members: List[Tree] = unwrap(tree) match
            case block: Block =>
              val from =
                block.stats.collectFirst { case definition: MemberDef => definition.symbol.owner }
                . getOrElse(moduleClass)

              block.changeOwner(from, moduleClass).stats

            case _ =>
              Nil

          members.foreach:
            case member: MemberDef => member.symbol.entered(using unitCtx)
            case _                 => ()

          // `ModuleDef` builds the module value, its class and a constructor.
          val moduleDef = ModuleDef(module.asTerm, members)(using unitCtx)
          val rootRef = ref(definitions.RootPackage)(using unitCtx).asInstanceOf[Ident]
          val packageTree = PackageDef(rootRef, moduleDef.trees).withSpan(span)

          val source = SourceFile.virtual("<repl-module>", "")
          val info = CompilationUnitInfo(source.file, tastyInfo = None)

          Some(CompilationUnit(source, packageTree, forceTrees = true, info)(using unitCtx))

        case other =>
          Some(other)
