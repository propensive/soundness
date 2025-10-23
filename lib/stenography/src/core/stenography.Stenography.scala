                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                               ╭───╮                              ┃
┃                                                               │   │                              ┃
┃                                                               │   │                              ┃
┃                     ╭─────────╮ ╭───╮╌────╮╌────╮ ╭─────────╮ │   │ ╭───╮                        ┃
┃                     ╰─────╮   │ │   ╭─╮   ╭─╮   │ │   ╭─╮   │ │   │╌╯   │                        ┃
┃                     ╭─────╯   │ │   │ │   │ │   │ │   │ │   │ │        ╌╯                        ┃
┃                     │   ╭─╮   │ │   │ │   │ │   │ │   │ │   │ │   ╭─╮   │                        ┃
┃                     │   ╰─╯   │ │   │ │   │ │   │ │   ╰─╯   │ │   │ │   │                        ┃
┃                     ╰─────────╯ ╰───╯ ╰───╯ ╰───╯ ╰─────────╯ ╰───╯ ╰───╯                        ┃
┃                                                                                                  ┃
┃    Amok, prerelease version                                                                      ┃
┃    © Copyright 2023-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://github.com/propensive/amok/                                                       ┃
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
import spectacular.*

object Stenography:
  import dotty.tools.dotc.*

  def name[typename <: AnyKind: Type](using Quotes): Expr[Text] =
    import quotes.reflect.*
    val outer = quotes match
      case quotes: runtime.impl.QuotesImpl =>
        given ctx: core.Contexts.Context = quotes.ctx
        ctx.compilationUnit.tpdTree match
          case ast.tpd.PackageDef(_, statements) =>
            statements.collect:
              case ast.tpd.Import(name, List(SimpleSelector("_"))) => Typename(name.show)

    def owners(symbol: Symbol): List[Tree] =
      val parent =
        if symbol.maybeOwner.isPackageDef then
          symbol.maybeOwner.companionModule
        else symbol.maybeOwner
      try symbol.tree :: owners(parent) catch case _ =>
        Nil

    val imports = owners(Symbol.spliceOwner).flatMap:
      case tree@DefDef(_, _, _, Some(Block(entries, _))) => entries.collect:
        case Import(name, List(SimpleSelector("_"))) => Typename(name.show)

      case tree@ValDef(_, _, _) =>
        Nil

      case tree: ClassDef =>
        tree.body.collect:
          case Import(name, List(SimpleSelector("_"))) => Typename(name.show)

      case tree =>
        Nil

    given Scope = Scope(Set(Typename("scala"), Typename("scala.Predef")) ++ imports ++ outer)

    Expr(Syntax(TypeRepr.of[typename]).show)
