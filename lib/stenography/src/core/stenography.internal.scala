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
┃    Soundness, version 0.63.0.                                                                    ┃
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

object internal:
  import dotty.tools.dotc.*

  def typename[typename <: AnyKind: Type]: Macro[Text] = Expr(name[typename])

  def name[typename <: AnyKind: Type](using Quotes): Text =
    import quotes.reflect.*
    name(TypeRepr.of[typename])

  def name(using Quotes)(typeRepr: quotes.reflect.TypeRepr): Text =
    given Bindings = Bindings()

    val outer: List[Typename] = quotes.absolve match
      case quotes: runtime.impl.QuotesImpl =>
        given context: core.Contexts.Context = quotes.ctx

        context.compilationUnit.tpdTree.absolve match
          case ast.tpd.PackageDef(root, statements) =>
            Typename(root.show) :: statements.collect:
              case ast.tpd.Import(name, _) => Typename(name.show)

          case _ =>
            Nil

      case _ => Nil

    val imports: sci.Set[Typename] = metaprogramming.imports.map(_.term).map(Syntax.term(_)).toSet

    // Build the `direct` set by drilling into every wildcard import that's in
    // scope (including REPL-accumulated imports across earlier lines, captured
    // by `metaprogramming.imports`) and collecting type aliases carrying the
    // `Exported` flag. Those aliases' *target* types are reachable via just
    // their leaf in the current scope, so we render them that way.
    val direct: sci.Set[Typename] = quotes.absolve match
      case quotes: runtime.impl.QuotesImpl =>
        given context: core.Contexts.Context = quotes.ctx

        metaprogramming.imports.filter(_.wildcard).flatMap: imp =>
          val rootSym = imp.term.asInstanceOf[core.Types.Type].termSymbol(using context)

          if !rootSym.exists then Nil
          else exportedTargets(rootSym)

        .toSet

      case _ => sci.Set.empty[Typename]

    given Imports =
      Imports(sci.Set(Typename("scala"), Typename("scala.Predef")) ++ imports ++ outer, direct)

    Syntax(typeRepr).text

  private def exportedTargets(using Quotes, dotty.tools.dotc.core.Contexts.Context)
    ( rootSym: dotty.tools.dotc.core.Symbols.Symbol )
  :   List[Typename] =

    import dotty.tools.dotc.core.{Flags, Types}
    import quotes.reflect.TypeRepr

    // Top-level definitions in a package are stored inside synthetic
    // `<filename>$package` classes; export forwarders for types live there.
    val directDecls = rootSym.info.decls.toList
    val packageClasses = directDecls.filter(_.name.toString.endsWith("$package"))
    val nestedDecls = packageClasses.flatMap(_.info.decls.toList)

    (directDecls ++ nestedDecls).filter(_.is(Flags.Exported)).flatMap: decl =>
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
              case Syntax.Simple(typename) => List(typename, typename.companionObject)
              case _                       => Nil

          case _ =>
            Nil

        case _ =>
          Nil
