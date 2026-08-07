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
import scala.collection.immutable.{List, Nil, ::}

import scala.quoted.*

import anticipation.*
import gigantism.*
import vacuous.*

object internal:
  import dotty.tools.dotc.*

  def designator[designator <: AnyKind: Type]: Macro[Text] = Expr(name[designator])

  def name[designator <: AnyKind: Type](using Quotes): Text =
    import quotes.reflect.*
    name(TypeRepr.of[designator])

  def name(using Quotes)(typeRepr: quotes.reflect.TypeRepr): Text =
    // Whether `A => B` desugars to `ImpureFunctionN` at the use site, and hence whether a bare
    // `FunctionN` should be rendered `->` or `=>`.
    val pureFuns: Boolean = quotes.absolve match
      case quotes: runtime.impl.QuotesImpl =>
        try config.Feature.pureFunsEnabled(using quotes.ctx) catch case _: Exception => true

      case _ => true

    // The bindings the scan below runs with. They carry no infix aliases: the scan renders
    // nothing but bare `TypeRef`s, for which the aliases it is in the middle of discovering
    // could not matter. The type itself is rendered with alias-carrying bindings at the end.
    given Bindings = Bindings(pureFuns)

    val outer: List[Designator] = quotes.absolve match
      case quotes: runtime.impl.QuotesImpl =>
        given context: core.Contexts.Context = quotes.ctx

        context.compilationUnit.tpdTree.absolve match
          case ast.tpd.PackageDef(root, statements) =>
            Designator(root.show) :: statements.collect:
              case ast.tpd.Import(name, _) => Designator(name.show)

          case _ =>
            Nil

      case _ => Nil

    val imports: sci.Set[Designator] = metaprogramming.imports.map(_.term).map(Syntax.term(_)).toSet

    // Drill into every scope whose members are reachable unqualified from here — each wildcard
    // import that's in scope (including the REPL-accumulated ones across earlier lines,
    // captured by `metaprogramming.imports`) and this unit's own package — and harvest two
    // things from a single pass over its declarations.
    //
    // The `direct` set holds type aliases carrying the `Exported` flag. Those aliases' *target*
    // types are reachable via just their leaf in the current scope, so we render them that way.
    //
    // The `aliases` map holds the infix type aliases which refine a single type member, keyed
    // by that member's name, so `Foo { type Form = Bar }` can be written back as `Foo in Bar`.
    // Scanning scopes rather than the whole classpath is what keeps the rendering honest: an
    // alias is only worth preferring where it is in scope to be written.
    val (direct, aliases) = quotes.absolve match
      case quotes: runtime.impl.QuotesImpl =>
        given context: core.Contexts.Context = quotes.ctx

        val imported = metaprogramming.imports.filter(_.wildcard).map: imp =>
          imp.term.asInstanceOf[core.Types.Type].termSymbol(using context)

        // This unit's own package, so that a module's aliases apply within the module which
        // declares them, without it having to import itself.
        val own = context.compilationUnit.tpdTree.absolve match
          case ast.tpd.PackageDef(root, _) => List(root.symbol)
          case _                           => Nil

        val harvested = (imported ++ own).filter(_.exists).map(scopeInfo)

        // Where two aliases claim the same member, choose by name, so that the rendering does
        // not depend on the order the scopes happened to be walked in.
        val aliases = harvested.flatMap(_(1)).groupBy(_(0)).view.mapValues: candidates =>
          candidates.map(_(1).s).min.tt

        (harvested.flatMap(_(0)).toSet, aliases.toMap)

      case _ =>
        (sci.Set.empty[Designator], sci.Map.empty[String, Text])

    given Imports =
      Imports(sci.Set(Designator("scala"), Designator("scala.Predef")) ++ imports ++ outer, direct)

    Syntax(using quotes)(using Bindings(pureFuns, aliases))(typeRepr).text

  // Everything worth knowing about one scope whose members are reachable unqualified: the
  // targets of its `Exported` aliases, and the infix type aliases it declares which refine a
  // single type member.
  private def scopeInfo(using Quotes, dotty.tools.dotc.core.Contexts.Context)
    ( rootSym: dotty.tools.dotc.core.Symbols.Symbol )
  :   (List[Designator], List[(String, Text)]) =

    import dotty.tools.dotc.core.Flags

    // Top-level definitions in a package are stored inside synthetic
    // `<filename>$package` classes; export forwarders for types live there.
    val directDecls = rootSym.info.decls.toList
    val packageClasses = directDecls.filter(_.name.toString.endsWith("$package"))
    val nestedDecls = packageClasses.flatMap(_.info.decls.toList)

    // A type alias can never be a *direct* member of a package — a top-level one lives in the
    // `$package` class above — so a package's own declarations cannot hold an infix alias and
    // are skipped. That is not just an optimisation: `java.lang` is a root import, and reading
    // the `Infix` flag off each of its members would complete every class in it, which on a JDK
    // whose classfiles this compiler cannot fully read fails outright.
    val aliasDecls = if rootSym.is(Flags.Package) then nestedDecls else directDecls ++ nestedDecls

    val exported = (directDecls ++ nestedDecls).filter(_.is(Flags.Exported))

    (exported.flatMap(exportedTarget), aliasDecls.filter(_.is(Flags.Infix)).flatMap(refiningAlias))


  private def exportedTarget(using Quotes, dotty.tools.dotc.core.Contexts.Context)
    ( decl: dotty.tools.dotc.core.Symbols.Symbol )
  :   List[Designator] =

    import dotty.tools.dotc.core.Types
    import quotes.reflect.TypeRepr

    decl.info match
      // Only simple (non-polymorphic) alias targets can shorten to a name. Polymorphic
      // re-exports (tuple types, `Some`, `<:<`, …) carry `TypeParamRef`s that `Syntax`
      // cannot render — matching only `TypeRef` skips them instead of crashing. This path
      // is now exercised by proscenium's prelude re-exports of the primitives.
      case alias: Types.TypeAlias => alias.alias match
        case ref: Types.TypeRef =>
          Syntax(ref.asInstanceOf[TypeRepr]) match
            // Add both forms so the same import path can shorten references
            // to either the type itself or its companion (e.g. `Textual` and
            // `Textual.foo` both resolve via `import soundness.*`).
            case Syntax.Simple(designator) => List(designator, designator.companionObject)
            case _                         => Nil

        case _ =>
          Nil

      case _ =>
        Nil


  // Recognises an alias of the shape
  //
  //     infix type in [refined, form] = refined { type Form = form }
  //
  // and pairs the refined member's name with the operator's, `"Form" -> "in"`. The two type
  // parameters have to appear in exactly those positions: the first as the refinement's parent
  // and the second as the sole member's alias. Anything else — a fixed parent, a second member,
  // an abstract member, a different arity — has no two-operand infix form and is skipped, which
  // is what excludes `transcribes` and every infix alias that is not a refinement at all.
  private def refiningAlias(using Quotes, dotty.tools.dotc.core.Contexts.Context)
    ( decl: dotty.tools.dotc.core.Symbols.Symbol )
  :   List[(String, Text)] =

    import dotty.tools.dotc.core.Types

    def parameter(repr: Types.Type, binder: Types.HKTypeLambda, index: Int): Boolean =
      repr match
        case ref: Types.TypeParamRef => ref.binder == binder && ref.paramNum == index
        case _                       => false

    val lambda: Optional[Types.HKTypeLambda] = decl.info match
      case alias: Types.TypeAlias => alias.alias match
        case lambda: Types.HKTypeLambda => lambda
        case _                          => Unset

      case lambda: Types.HKTypeLambda => lambda
      case _                          => Unset

    lambda.lay(Nil): lambda =>
      if lambda.paramNames.length != 2 then Nil else
        // An `export`ed alias forwards to the original as an applied type, so dealiasing the
        // body is what lets an alias reached through a prelude be recognised too.
        lambda.resType.dealias match
          case Types.RefinedType(parent, name, Types.TypeAlias(rhs))
          if parameter(parent, lambda, 0) && parameter(rhs, lambda, 1) =>
            List(name.toString -> decl.name.toString.tt)

          case _ =>
            Nil
