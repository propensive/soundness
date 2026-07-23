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
package delicious

import scala.collection.immutable.ListMap

import dotty.tools.dotc as dtd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.Contexts
import dotty.tools.dotc.core.tasty.DottyUnpickler
import dotty.tools.dotc.quoted.QuotesCache
import dotty.tools.dotc.core.tasty.TreeUnpickler.UnpickleMode
import dotty.tools.dotc.reporting.Reporter
import dotty.tools.dotc.util.SourceFile
import dotty.tools.io.NoAbstractFile

import java.util as ju

import anticipation.*
import gossamer.*
import hellenism.*
import rudiments.*
import stenography.*
import vacuous.*

object Reifier:
  /** Rewrite the placeholder sentinels — string-literal types the pickler
   *  substituted for unpicklable subtrees — into renderings of the recorded
   *  placeholders. Sound because the pickler replaces whole maximal subtrees,
   *  so a sentinel can only appear as a complete type, never in constructor or
   *  prefix position. */
  def substitute(syntax: Syntax, placeholders: List[Placeholder]): Syntax =
    val byId: Map[Int, Placeholder] = placeholders.map { placeholder => placeholder.id -> placeholder }.to(Map)

    def replace(text: Text): Optional[Syntax] =
      if text.s.length >= 2 && text.s.startsWith("\"") && text.s.endsWith("\"") then
        val body = text.s.substring(1, text.s.length - 1).nn.tt

        Placeholder.reference(body).let { id => byId.at(id).let { p => Syntax.Symbolic(p.printed) } }
        . or(Placeholder.escaped(body).let { literal => Syntax.Primitive(t"\"$literal\"") })

      else Unset

    def entries(map: ListMap[Text, Syntax]): ListMap[Text, Syntax] =
      map.map { (key, value) => key -> recur(value) }

    def recur(syntax: Syntax): Syntax = syntax match
      case Syntax.Primitive(text)          => replace(text).or(syntax)
      case Syntax.Simple(_)                => syntax
      case Syntax.Symbolic(_)              => syntax
      case Syntax.Value(_)                 => syntax
      case Syntax.Projection(base, text)   => Syntax.Projection(recur(base), text)
      case Syntax.Infix(left, mid, right)  => Syntax.Infix(recur(left), mid, recur(right))
      case Syntax.Prefix(middle, right)    => Syntax.Prefix(middle, recur(right))
      case Syntax.Suffix(left, suffix)     => Syntax.Suffix(recur(left), suffix)
      case Syntax.Selection(left, right)   => Syntax.Selection(recur(left), right)
      case Syntax.Named(using0, name, s)   => Syntax.Named(using0, name, recur(s))
      case Syntax.Sequence(style, parts)   => Syntax.Sequence(style, parts.map(recur))
      case Syntax.Compound(parts)          => Syntax.Compound(parts.map(recur))
      case Syntax.Match(scrutinee, cases)  => Syntax.Match(recur(scrutinee), cases.map(recur))

      case Syntax.Structural(base, types, terms) =>
        Syntax.Structural(recur(base), entries(types), entries(terms))

      case Syntax.Application(left, elements, infix) =>
        Syntax.Application(recur(left), elements.map(recur), infix)

      case Syntax.Declaration(method, syntaxes, result) =>
        Syntax.Declaration(method, syntaxes.map(recur), recur(result))

    recur(syntax)

/** Turns the TASTy payloads of semantic diagnostics back into stenography
 *  `Syntax` values, resolving them against the given classpath. The pickler
 *  guarantees the payload resolves against the classpath alone: anything that
 *  could not (local symbols, skolems, error types, type variables) was
 *  replaced by a placeholder before pickling. */
class Reifier(classpath: LocalClasspath):
  private lazy val runContext: Contexts.Context =
    val entries: Text =
      classpath.entries.flatMap:
        case ClasspathEntry.Directory(directory) => List(directory)
        case ClasspathEntry.Jar(jar)             => List(jar)
        case _                                   => Nil

      . join(java.io.File.pathSeparator.nn.tt)

    object driver extends dtd.Driver:
      def context: Contexts.Context =
        // The trailing empty argument stops the driver from treating an
        // argument list with no source files as a request to print usage.
        setup(Array("-classpath", entries.s, ""), initCtx.fresh).map(_(1)).get

    val base = driver.context.fresh.setReporter(Reporter.NoReporter)
    val run = dtd.Compiler().newRun(using base)
    run.compileSources(List(SourceFile.virtual("<delicious>", "")))

    // Quote unpickling (which stenography's `TypeRepr.of` comparisons trigger)
    // expects the quote-cache context property that macro-expansion contexts
    // carry; a standalone context must install it explicitly.
    QuotesCache.init(run.runContext.fresh)

  /** The stenography rendering of a type marker's TASTy payload, or `Unset`
   *  if there is no payload or it cannot be unpickled: a diagnostic must never
   *  become a crash, so callers fall back to the compiler-printed text. */
  def syntax(typed: Markup.Typed): Optional[Syntax] =
    typed.tasty.let: tasty =>
      try
        given Contexts.Context = runContext
        val bytes = ju.Base64.getDecoder.nn.decode(tasty.s).nn

        val unpickler =
          DottyUnpickler(NoAbstractFile, bytes, isBestEffortTasty = false, UnpickleMode.TypeTree)

        unpickler.enter(Set.empty)
        val tree = unpickler.tree
        tree.foreachSubTree { _ => () } // force trees and positions

        val quotes = scala.quoted.runtime.impl.QuotesImpl()
        val repr = tree.tpe.asInstanceOf[quotes.reflect.TypeRepr]

        Reifier.substitute(Syntax(using quotes)(repr), typed.placeholders)

      catch case scala.util.control.NonFatal(_) => Unset
