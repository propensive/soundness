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

    val stats: List[Statement] = statements(body.asTerm)

    // Every symbol the block itself defines; a reference to one of these is not
    // "free" and needs no capturing.
    val blockSymbols: scm.Set[Symbol] = scm.Set()

    val definedSymbols = new TreeAccumulator[Unit]:
      def foldTree(unit: Unit, tree: Tree)(owner: Symbol): Unit =
        tree match
          case definition: Definition => blockSymbols += definition.symbol
          case _                      => ()

        foldOverTree(unit, tree)(owner)

    stats.foreach(definedSymbols.foldTree((), _)(Symbol.spliceOwner))

    val imports:     scm.ListBuffer[String]                    = scm.ListBuffer()
    val definitions: scm.ListBuffer[String]                    = scm.ListBuffer()
    val bindings:    scm.ListBuffer[(Boolean, String, String)] = scm.ListBuffer()
    val captures:    scm.ListBuffer[(String, Term)]            = scm.ListBuffer()
    val lifted:      scm.ListBuffer[Tree]                      = scm.ListBuffer()

    stats.foreach:
      case statement: Import => imports += sourceText(statement)
      case statement: Export => imports += sourceText(statement)

      case statement: ValDef if !statement.symbol.flags.is(Flags.Given) =>
        val mutable: Boolean = statement.symbol.flags.is(Flags.Mutable)
        bindings += ((mutable, statement.name, statement.tpt.tpe.show))

        statement.rhs.foreach: rhs =>
          captures += ((statement.name, rhs))

      case statement: Definition =>
        definitions += sourceText(statement)
        lifted += statement

      case _ =>
        ()

    // A lifted definition (`def`/`class`/…) is recompiled inside the REPL, so a
    // value it refers to from the enclosing scope must be captured too, or it
    // would fail to resolve. Collect simple-name references to a val/var/param
    // the block does not itself define and that is not part of the REPL's own
    // default scope (`scala`/`java`), and capture each as a binding (keyed by
    // name, so the lifted source resolves it in the REPL). This covers enclosing
    // method locals and parameters as well as fields of an enclosing object —
    // e.g. a value defined earlier in an enclosing REPL session.
    val bound: scm.Set[String] = scm.Set.from(bindings.map(_._2))
    val free:  scm.LinkedHashMap[String, (String, Term)] = scm.LinkedHashMap()

    val freeReferences = new TreeAccumulator[Unit]:
      def foldTree(unit: Unit, tree: Tree)(owner: Symbol): Unit =
        tree match
          case ident: Ident if ident.symbol.exists && ident.symbol.isValDef =>
            val symbol:    Symbol = ident.symbol
            val ownerName: String = symbol.maybeOwner.fullName

            val predefined =
              ownerName == "scala" || ownerName.startsWith("scala.")
              || ownerName == "java" || ownerName.startsWith("java.")

            if !predefined && !blockSymbols.contains(symbol) && !bound.contains(ident.name)
            then free.getOrElseUpdate(ident.name, (ident.tpe.widen.show, Ref(symbol)))

          case _ =>
            ()

        foldOverTree(unit, tree)(owner)

    lifted.foreach(freeReferences.foldTree((), _)(Symbol.spliceOwner))

    free.foreach: (name, info) =>
      bindings += ((false, name, info(0)))
      captures += ((name, info(1)))

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
