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
package stenography

import scala.collection.immutable as sci
import scala.util.control.NonFatal

import anticipation.*
import vacuous.*

object Imports:
  val empty: Imports = Imports(sci.Set(), sci.Set())

  // The designators reachable by their leaf name through the `export` aliases declared in
  // `scope`, a package or object such as `soundness`: each alias's target type, and its
  // companion. This is the non-macro counterpart of the harvest `internal.name` performs for
  // every wildcard import at a macro expansion site, for callers which render types outside
  // any macro (a REPL abbreviating a diagnostic against the session's imports) and so must
  // build their `Imports` by hand. Rendering a target goes through `Syntax`, which needs the
  // quote cache installed on the context — a macro's context has it; a standalone one must
  // install it, as `delicious.Reifier` does. A scope which does not resolve, or whose
  // declarations cannot be read, contributes nothing: a rendering must never throw.
  def exports(scope: Designator)(using context: dotty.tools.dotc.core.Contexts.Context)
  :   sci.Set[Designator] =

    import dotty.tools.dotc.core.Denotations
    import dotty.tools.dotc.core.Names.termName

    def path(designator: Designator): String = designator.parent.lay(designator.name.s): parent =>
      s"${path(parent)}.${designator.name}"

    try
      val denotation = Denotations.staticRef(termName(path(scope)), generateStubs = false)
      if !denotation.exists then sci.Set() else
        given quotes: scala.quoted.Quotes = scala.quoted.runtime.impl.QuotesImpl()
        given Bindings = Bindings()
        stenography.internal.scopeInfo(denotation.symbol)(0).toSet
    catch case NonFatal(_) => sci.Set()

  // `Imports(designators, direct)`, with `direct` extended by the `exports` of every one of
  // `designators`, so that a type reached through a wildcard-imported prelude renders as the
  // user would write it.
  // The infix type aliases declared in `scope` which each refine a single type member, by that
  // member's name: what lets `Foo { type Form = Bar }` be written `Foo in Bar` where `scope`
  // is wildcard-imported. Where two aliases claim a member, the one first by name wins, as in
  // a macro's harvest.
  def infixAliases(scope: Designator)(using context: dotty.tools.dotc.core.Contexts.Context)
  :   sci.Map[String, Text] =

    import dotty.tools.dotc.core.Denotations
    import dotty.tools.dotc.core.Names.termName

    def path(designator: Designator): String = designator.parent.lay(designator.name.s): parent =>
      s"${path(parent)}.${designator.name}"

    try
      val denotation = Denotations.staticRef(termName(path(scope)), generateStubs = false)
      if !denotation.exists then sci.Map() else
        given quotes: scala.quoted.Quotes = scala.quoted.runtime.impl.QuotesImpl()
        given Bindings = Bindings()
        stenography.internal.scopeInfo(denotation.symbol)(1).groupBy(_(0)).view.mapValues: candidates =>
          candidates.map(_(1).s).min.tt
        . toMap
    catch case NonFatal(_) => sci.Map()

  def resolve(designators: sci.Set[Designator], direct: sci.Set[Designator])
       (using dotty.tools.dotc.core.Contexts.Context)
  :   Imports =

    val aliases: sci.Map[String, Text] =
      designators.toList.flatMap(infixAliases(_).toList).groupBy(_(0)).view.mapValues: candidates =>
        candidates.map(_(1).s).min.tt
      . toMap

    Imports(designators, direct ++ designators.flatMap(exports), aliases)

// `aliases` maps a refined member's name to the infix type alias in scope which refines it,
// for `Syntax.text` to prefer; empty unless resolved against a compiler context.
case class Imports
  ( designators: sci.Set[Designator],
    direct:      sci.Set[Designator],
    aliases:     sci.Map[String, Text] = sci.Map() ):

  def has(designator: Designator): Boolean = designators.contains(designator)
  def hasDirect(designator: Designator): Boolean = direct.contains(designator)
