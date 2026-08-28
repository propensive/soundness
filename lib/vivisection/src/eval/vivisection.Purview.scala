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
import dotty.tools.dotc.core.Contexts
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.reporting.Reporter
import dotty.tools.dotc.util.SourceFile

import anticipation.*
import hellenism.*
import proscenium.*

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
    val paths: sci.List[Text] =
      classpath.entries.stdlib.flatMap:
        case Classpath.Entry.Directory(directory) => sci.List(directory)
        case Classpath.Entry.Jar(jar)             => sci.List(jar)
        case _                                    => sci.Nil

    val entries: Text = paths.map(_.s).mkString(java.io.File.pathSeparator.nn).tt

    object driver extends dtd.Driver:
      def context: Contexts.Context =
        val args = ju.ArrayList[String]()
        args.add("-classpath")
        args.add(entries.s)
        args.add("")
        val array = args.toArray(new scala.Array[String | Null](0)).nn
        setup(array.asInstanceOf[scala.Array[String]], initCtx.fresh).map(_(1)).get

    val base = driver.context.fresh.setReporter(Reporter.NoReporter)
    val run = dtd.Compiler().newRun(using base)
    run.compileSources(List(SourceFile.virtual("<purview>", "")).stdlib)
    run.runContext

  // The declared source types of a method's value parameters, keyed by parameter name. `Unset`
  // per entry is never produced — a name simply absent means its type could not be recovered, and
  // the caller falls back to the erased runtime type. The whole lookup degrades to an empty map on
  // any failure (an unloadable class, a name that does not resolve): a debugger reads types
  // opportunistically and must never fail the frame.
  def parameters(className: Text, methodName: Text): sci.Map[Text, Text] =
    try
      given Contexts.Context = context
      val cls = Symbols.requiredClass(className.s)
      val method = cls.requiredMethod(methodName.s)

      // The debuggee compiles with `proscenium` imported, so the printer qualifies standard types
      // as `proscenium.Int`/`proscenium.Array`; the synthetic evaluation class compiles under the
      // ordinary `scala` imports, where those are the plain names, so the prefix is stripped to let
      // the type — and its `Inspectable` — resolve there. A domain type like `Port` is unaffected.
      val entries = method.paramSymss.flatten.filter(_.isTerm).map: param =>
        val printed = param.info.show.replace("proscenium.", "").nn.tt
        (param.name.show.tt, printed)

      entries.toMap

    catch case scala.util.control.NonFatal(_) => sci.Map()
