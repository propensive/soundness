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

import dotty.tools.dotc as dtd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.CompilationUnitInfo
import dotty.tools.dotc.core.Contexts
import dotty.tools.dotc.core.tasty.DottyUnpickler
import dotty.tools.dotc.core.tasty.TastyUnpickler
import dotty.tools.dotc.quoted.QuotesCache
import dotty.tools.dotc.core.tasty.TreeUnpickler.UnpickleMode
import dotty.tools.dotc.reporting.Reporter
import dotty.tools.dotc.util.SourceFile
import dotty.tools.io.VirtualFile

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
    val byId: Map[Int, Placeholder] = Map.of(placeholders.stdlib.map { placeholder => placeholder.id -> placeholder }.toMap)

    def replace(text: Text): Optional[Syntax] =
      if text.s.length >= 2 && text.s.startsWith("\"") && text.s.endsWith("\"") then
        val body = text.s.substring(1, text.s.length - 1).nn.tt

        Placeholder.reference(body).let { id => byId(id).let { p => Syntax.Symbolic(p.printed) } }
        . or(Placeholder.escaped(body).let { literal => Syntax.Primitive(t"\"$literal\"") })

      else Unset

    def entries(map: Ledger[Text, Syntax]): Ledger[Text, Syntax] = map.map(recur)

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
        case Classpath.Entry.Directory(directory) => List(directory)
        case Classpath.Entry.Jar(jar)             => List(jar)
        case _                                   => Nil

      . join(java.io.File.pathSeparator.nn.tt)

    object driver extends dtd.Driver:
      def context: Contexts.Context =
        // The trailing empty argument stops the driver from treating an
        // argument list with no source files as a request to print usage.
        // As in `Scalac`: the argument array crosses in through a Java-side copy.
        val args = java.util.ArrayList[String]()
        args.add("-classpath"); args.add(entries.s); args.add("")
        setup(args.toArray(new scala.Array[String | Null](0)).nn.asInstanceOf[scala.Array[String]], initCtx.fresh)
        . map(_(1)).get

    val base = driver.context.fresh.setReporter(Reporter.NoReporter)
    val run = dtd.Compiler().newRun(using base)
    run.compileSources(List(SourceFile.virtual("<delicious>", "")).stdlib)

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
        // The decode is inlined at the argument: the Java decoder's fluid result adapts to
        // the unpickler's pure formal, where a named array value would charge its read
        // capability.
        // `DottyUnpickler`'s constructor differs between the compiler streams (one reads the
        // bytes from the file, the other takes them separately), so the same unpickling is
        // spelled out through the section unpicklers, whose surface both streams share.
        val unpickler =
          TastyUnpickler
            ( ju.Base64.getDecoder.nn.decode(tasty.s).nn.asInstanceOf[scala.Array[Byte]],
              false )

        // `TreeUnpickler`'s constructor reads `compilationUnitInfo.tastyInfo.get.attributes`
        // UNCONDITIONALLY, so the `TastyInfo` must be present or every payload dies on `None.get` —
        // inside `unpickle` below, where the `catch` turns it into a silent `Unset`. The one-argument
        // `CompilationUnitInfo` supplies `None`, so `DottyUnpickler`'s own header/attributes step is
        // reproduced here too. Its VALUES are inert for a diagnostic payload — the pickler writes no
        // attributes section, so this is `Attributes.empty` and every flag is false, exactly what
        // `DottyUnpickler` would compute — but the `Some` wrapper is load-bearing.
        val attributes =
          unpickler.unpickle(DottyUnpickler.AttributesSectionUnpickler()).map(_.attributes)
          . getOrElse(dtd.core.tasty.Attributes.empty)

        val header = unpickler.header

        val version =
          dotty.tools.tasty.TastyVersion
            (header.majorVersion, header.minorVersion, header.experimentalVersion)

        // The file exists only to give the compilation unit an associated name: its
        // contents are never read (the unpickler above already has the bytes), but the
        // decode is repeated inline because only a fluid Java result adapts to the pure
        // formal, as above.
        val info =
          CompilationUnitInfo
            ( VirtualFile
                ( "<delicious>",
                  ju.Base64.getDecoder.nn.decode(tasty.s).nn.asInstanceOf[scala.Array[Byte]] ),
              // Scala 3.10 makes this parameter a lazy loader (`tastyInfoLoader:
              // () => Option[TastyInfo]`); the `Some` wrapper remains load-bearing.
              () => Some(dtd.core.TastyInfo(version, attributes)) )
          . nn
        val positions = unpickler.unpickle(DottyUnpickler.PositionsSectionUnpickler())
        val comments = unpickler.unpickle(DottyUnpickler.CommentsSectionUnpickler())

        val treeUnpickler =
          unpickler.unpickle(DottyUnpickler.TreeSectionUnpickler(info, positions, comments, false))
          . get

        treeUnpickler.enter(scala.collection.immutable.Set.empty)
        val tree = treeUnpickler.unpickle(UnpickleMode.TypeTree).head
        tree.foreachSubTree { _ => () } // force trees and positions

        val quotes = scala.quoted.runtime.impl.QuotesImpl()
        val repr = tree.tpe.asInstanceOf[quotes.reflect.TypeRepr]

        Reifier.substitute(Syntax(using quotes)(repr), typed.placeholders)

      catch case scala.util.control.NonFatal(_) => Unset
