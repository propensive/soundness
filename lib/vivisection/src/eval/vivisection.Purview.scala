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
package vivisection

import java.util as ju

import scala.collection.immutable as sci
import scala.language.adhocExtensions

import dotty.tools.dotc as dtd
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.core.Types
import dotty.tools.dotc.quoted.QuotesCache
import dotty.tools.dotc.reporting.Reporter
import dotty.tools.dotc.util.SourceFile

import anticipation.*
import gossamer.*
import hellenism.*
import proscenium.*
import rudiments.*
import stenography.*

// Recovers the *static* Scala types of a frame's bindings from the debuggee's compiled program,
// so that a value renders through the instance its declared type selects rather than its erased
// runtime type — most visibly, an opaque type renders through its own `Inspectable` rather than
// its underlying representation's. The compiler's own symbol loader reads the classes' TASTy off
// the classpath; a binding's declared type is then read straight from its symbol. Everything is
// wrapped so a resolution failure degrades to `Unset`, never a crash.
class Purview(classpath: LocalClasspath):
  // A standalone compiler context over the debuggee's classpath, warmed once. Modelled on
  // `delicious.Reifier` and `anthology.ScalacDriver`: `Driver.setup` loads the classpath's symbol
  // table, `NoReporter` silences diagnostics, and an empty virtual source primes a run.
  private lazy val context: Contexts.Context =
    val paths: List[Text] =
      classpath.entries.sweep:
        case Classpath.Entry.Directory(directory) => directory
        case Classpath.Entry.Jar(jar)             => jar

    val entries: Text = paths.join(java.io.File.pathSeparator.nn.tt)

    object driver extends dtd.Driver:
      def context: Contexts.Context =
        val args = ju.ArrayList[String]()
        // `-Yretain-trees` keeps the method-body trees the symbol loader unpickles from TASTy, so
        // a method's local `val`s (which live in its tree, not on its symbol) can be read.
        args.add("-Yretain-trees")
        args.add("-classpath")
        args.add(entries.s)
        args.add("")
        val array = args.toArray(new scala.Array[String | Null](0)).nn
        setup(array.asInstanceOf[scala.Array[String]], initCtx.fresh).map(_(1)).get

    val base = driver.context.fresh.setReporter(Reporter.NoReporter)
    val run = dtd.Compiler().newRun(using base)
    // `.stdlib`: `Compiler.Run#compileSources` is a `dotty.tools.dotc` API, which takes the
    // compiler's own `sci.List`.
    run.compileSources(List(SourceFile.virtual("<purview>", "")).stdlib)

    // Quote unpickling — which `stenography`'s `TypeRepr.of` comparisons trigger — expects the
    // quote-cache context property a macro-expansion context carries; a standalone context must
    // install it explicitly (as `delicious.Reifier` does).
    QuotesCache.init(run.runContext.fresh)

  // The debuggee compiles with `proscenium` imported, so the printer qualifies standard types as
  // `proscenium.Int`/`proscenium.Array`; the synthetic evaluation class compiles under the ordinary
  // `scala` imports, where those are the plain names, so the prefix is stripped to let the type —
  // and its `Inspectable` — resolve there. A domain type like `Port` is unaffected.
  private def normalise(text: Text): Text = text.s.replace("proscenium.", "").nn.tt

  // Every binding a debugger might see at a frame in a method, as (name, declared type) under the
  // standalone context: the method's value parameters (read from its symbol) and the `val`s in its
  // body (read from its unpickled tree). The types are compiler `Type`s, rendered differently by
  // the two public methods below. A later binding of the same name — a shadowing inner `val` —
  // wins, matching what is live at a stop.
  private def bindings(className: Text, methodName: Text)(using Contexts.Context)
  :   sci.List[(Text, Types.Type)] =

    val cls = Symbols.requiredClass(className.s)
    val method = cls.requiredMethod(methodName.s)
    val terms = method.paramSymss.flatten.filter(_.isTerm)
    val parameters = for param <- terms yield (param.name.show.tt, param.info)

    // Walk the method body for its own `val`s (non-parameter, non-synthetic), reading each's
    // declared type from its type tree. `defTree` is the tree the symbol loader unpickled from
    // TASTy; if it is absent (trees not retained) the body simply contributes nothing.
    val locals = scala.collection.mutable.ListBuffer[(Text, Types.Type)]()

    val traverser = new tpd.TreeTraverser:
      def traverse(tree: tpd.Tree)(using Contexts.Context): Unit =
        tree match
          case valDef: tpd.ValDef =>
            val symbol = valDef.symbol

            if !symbol.is(Flags.Param) && !symbol.is(Flags.Synthetic)
            then locals += ((valDef.name.show.tt, valDef.tpt.tpe))

          case _ =>
            ()

        traverseChildren(tree)

    method.defTree match
      case defDef: tpd.DefDef => traverser.traverse(defDef.rhs)
      case _                  => ()

    parameters ++ locals.to(sci.List)

  // The compile-usable source type of each value parameter, keyed by name — the compiler's own
  // printed form. Consumed by the evaluator to type the synthetic class's parameters, so it must
  // be valid, resolvable Scala. Degrades to an empty map on any failure: a debugger reads types
  // opportunistically and must never fail the frame.
  def parameters(className: Text, methodName: Text): Map[Text, Text] =
    try
      given Contexts.Context = context

      val entries =
        for (name, tpe) <- bindings(className, methodName) yield (name, normalise(tpe.show.tt))

      entries.to(Map)

    catch case scala.util.control.NonFatal(_) => Map()

  // The human-facing rendering of each value parameter's declared type, through `stenography` —
  // capture-set-aware and source-accurate — keyed by name. This is what a debugger surfaces to the
  // user as `Variable.static`. Degrades to an empty map on any failure.
  def rendered(className: Text, methodName: Text): Map[Text, Text] =
    try
      given Contexts.Context = context
      val quotes = scala.quoted.runtime.impl.QuotesImpl()

      val entries =
        for (name, tpe) <- bindings(className, methodName) yield
          val repr = tpe.asInstanceOf[quotes.reflect.TypeRepr]
          (name, normalise(Syntax(using quotes)(repr).qualified))

      entries.to(Map)

    catch case scala.util.control.NonFatal(_) => Map()
