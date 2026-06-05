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
package anthology

import scala.collection.mutable as scm
import scala.quoted.*

import ambience.*
import hellenism.*

// Macro support for `Repl.apply(inline body)`: it reads the inline binding
// block's AST and lifts each statement into the REPL context. Imports/exports
// and definitions are lifted as source text; `val`/`var` bindings capture the
// runtime value of their right-hand side (evaluated in the host scope) into
// `ReplBridge`, exposing it in the REPL via a typed accessor.
object ReplMacro:
  def bound[version <: Scalac.Versions: Type]
    ( body:        Expr[Unit],
      scalac:      Expr[Scalac[version]],
      classloader: Expr[Classloader],
      temporary:   Expr[TemporaryDirectory] )
    ( using Quotes )
  :   Expr[Repl[version]] =

    import quotes.reflect.*

    def statements(term: Term): List[Statement] = term match
      case Inlined(_, _, inner)    => statements(inner)
      case Literal(UnitConstant()) => Nil
      case Block(stats, last)      => stats ++ statements(last)
      case other                   => List(other)

    def sourceText(tree: Tree): String = tree.pos.sourceCode.getOrElse(tree.show)

    val imports:     scm.ListBuffer[String]                 = scm.ListBuffer()
    val definitions: scm.ListBuffer[String]                 = scm.ListBuffer()
    val bindings:    scm.ListBuffer[(Boolean, String, String)] = scm.ListBuffer()
    val captures:    scm.ListBuffer[(String, Term)]         = scm.ListBuffer()

    statements(body.asTerm).foreach:
      case statement: Import => imports += sourceText(statement)
      case statement: Export => imports += sourceText(statement)

      case statement: ValDef if !statement.symbol.flags.is(Flags.Given) =>
        val mutable: Boolean = statement.symbol.flags.is(Flags.Mutable)
        bindings += ((mutable, statement.name, statement.tpt.tpe.show))

        statement.rhs.foreach: rhs =>
          captures += ((statement.name, rhs))

      case statement: Definition => definitions += sourceText(statement)
      case _                     => ()

    val importsExpr: Expr[List[String]] = Expr(imports.to(List))
    val definitionsExpr: Expr[List[String]] = Expr(definitions.to(List))

    val bindingsExpr: Expr[List[Repl.Binding]] = Expr.ofList:
      bindings.to(List).map: (mutable, name, typeName) =>
        '{Repl.Binding(${Expr(mutable)}, ${Expr(name)}, ${Expr(typeName)})}

    val preludeExpr: Expr[Repl.Prelude] =
      '{Repl.Prelude($importsExpr, $definitionsExpr, $bindingsExpr)}

    ' {
        val repl: Repl[version] =
          Repl.make[version]($preludeExpr)(using $scalac, $classloader, $temporary)

        $ {
            val puts: List[Expr[Unit]] = captures.to(List).map: (name, rhs) =>
              '{ReplBridge.put(repl.session, ${Expr(name)}, ${rhs.asExprOf[Any]})}

            Expr.block(puts, 'repl)
          }
      }
