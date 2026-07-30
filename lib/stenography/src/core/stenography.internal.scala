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

import scala.quoted.*

import anticipation.*
import gigantism.*

object internal:
  import dotty.tools.dotc.*

  def designator[designator <: AnyKind: Type]: Macro[Text] = Expr(name[designator])

  def name[designator <: AnyKind: Type](using Quotes): Text =
    import quotes.reflect.*
    name(TypeRepr.of[designator])

  def name(using Quotes)(typeRepr: quotes.reflect.TypeRepr): Text =
    given Bindings = Bindings()

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

    val imports: Set[Designator] = metaprogramming.imports.map(_.term).map(Syntax.term(_)).to(Set)

    // Build the `direct` set by drilling into every wildcard import that's in
    // scope (including REPL-accumulated imports across earlier lines, captured
    // by `metaprogramming.imports`) and collecting type aliases carrying the
    // `Exported` flag. Those aliases' *target* types are reachable via just
    // their leaf in the current scope, so we render them that way.
    val direct: Set[Designator] = quotes.absolve match
      case quotes: runtime.impl.QuotesImpl =>
        given context: core.Contexts.Context = quotes.ctx

        metaprogramming.imports.filter(_.wildcard).flatMap: imp =>
          val rootSym = imp.term.asInstanceOf[core.Types.Type].termSymbol(using context)

          if !rootSym.exists then Nil else exportedTargets(rootSym)

        .toSet

      case _ => Set.empty[Designator]

    given Imports =
      Imports(Set(Designator("scala"), Designator("scala.Predef")) ++ imports ++ outer, direct)

    Syntax(typeRepr).text

  private def exportedTargets(using Quotes, dotty.tools.dotc.core.Contexts.Context)
    ( rootSym: dotty.tools.dotc.core.Symbols.Symbol )
  :   List[Designator] =

    import dotty.tools.dotc.core.{Flags, Types}
    import quotes.reflect.TypeRepr

    // Top-level definitions in a package are stored inside synthetic
    // `<filename>$package` classes; export forwarders for types live there.
    val directDecls = rootSym.info.decls.toList
    val packageClasses = directDecls.filter(_.name.toString.endsWith("$package"))
    val nestedDecls = packageClasses.flatMap(_.info.decls.toList)

    (directDecls ++ nestedDecls).filter(_.is(Flags.Exported)).flatMap: decl =>
      decl.info match
        case alias: Types.TypeAlias =>
          Syntax(alias.alias.asInstanceOf[TypeRepr]) match
            // Add both forms so the same import path can shorten references
            // to either the type itself or its companion (e.g. `Textual` and
            // `Textual.foo` both resolve via `import soundness.*`).
            case Syntax.Simple(designator) => List(designator, designator.companionObject)
            case _                         => Nil

        case _ =>
          Nil
