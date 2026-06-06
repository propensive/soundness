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

import java.util as ju

import scala.collection.mutable as scm
import scala.quoted.*
import scala.quoted.runtime.impl.QuotesImpl

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.quoted.PickledQuotes

import ambience.*
import hellenism.*

// Macro support for `Repl.apply(inline body)`: it reads the inline binding
// block's AST and lifts each statement into the REPL context. Imports/exports
// are lifted as source text (re-injected into every line). Definitions and the
// accessors for captured `val`/`var` bindings are assembled into a single typed
// block, pickled to TASTy and carried (as data) to the REPL, where they are
// recompiled into a seed object with full type fidelity (`Repl.Prelude`). A
// captured value is exposed through a `def` accessor backed by `ReplBridge`, and
// free references inside lifted definitions are redirected to those accessors.
object ReplMacro:
  def bound[version <: Scalac.Versions: Type]
    ( body:        Expr[Unit],
      scalac:      Expr[Scalac[version]],
      classloader: Expr[Classloader],
      temporary:   Expr[TemporaryDirectory] )
    ( using Quotes )
  :   Expr[Repl[version]] =

    import quotes.reflect.*

    val owner: Symbol = Symbol.spliceOwner

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

    stats.foreach(definedSymbols.foldTree((), _)(owner))

    // A captured binding lifted into the seed object: `value` is the host-side
    // expression supplying its value; `symbol` is what the block (and host) use
    // to refer to it, used both to redirect lifted references to the accessor and
    // — when `writeBack` is set (a free `var`) — to assign back to the host.
    case class Bind
      ( name:      String,
        tpe:       TypeRepr,
        mutable:   Boolean,
        value:     Term,
        symbol:    Symbol,
        writeBack: Boolean )

    val imports: scm.ListBuffer[String]    = scm.ListBuffer()
    val binds:   scm.ListBuffer[Bind]      = scm.ListBuffer()
    val lifted:  scm.ListBuffer[Statement] = scm.ListBuffer()

    stats.foreach:
      case statement: Import => imports += sourceText(statement)
      case statement: Export => imports += sourceText(statement)

      case statement: ValDef if !statement.symbol.flags.is(Flags.Given) =>
        statement.rhs.foreach: rhs =>
          val mutable: Boolean = statement.symbol.flags.is(Flags.Mutable)
          binds += Bind(statement.name, statement.tpt.tpe, mutable, rhs, statement.symbol, false)

      case statement: Definition =>
        lifted += statement

      case _ =>
        ()

    // A lifted definition is recompiled in the seed, so a value it refers to from
    // the enclosing scope must be captured too, or it would not resolve. Collect
    // simple-name references to a val/var/param the block does not itself define
    // and that is not part of the REPL's default scope (`scala`/`java`). Such a
    // reference is captured live (`writeBack` for a `var`, so an assignment in a
    // lifted definition flows back to the host).
    val bound: scm.Set[String] = scm.Set.from(binds.map(_.name))
    val free:  scm.LinkedHashMap[String, (TypeRepr, Boolean, Symbol)] = scm.LinkedHashMap()

    val freeReferences = new TreeAccumulator[Unit]:
      def foldTree(unit: Unit, tree: Tree)(owner: Symbol): Unit =
        tree match
          case ident: Ident if ident.symbol.exists && ident.symbol.isValDef =>
            val symbol:    Symbol  = ident.symbol
            val ownerName: String  = symbol.maybeOwner.fullName
            val mutable:   Boolean = symbol.flags.is(Flags.Mutable)

            val predefined =
              ownerName == "scala" || ownerName.startsWith("scala.")
              || ownerName == "java" || ownerName.startsWith("java.")

            if !predefined && !blockSymbols.contains(symbol) && !bound.contains(ident.name)
            then free.getOrElseUpdate(ident.name, (ident.tpe.widen, mutable, symbol))

          case _ =>
            ()

        foldOverTree(unit, tree)(owner)

    lifted.foreach(freeReferences.foldTree((), _)(owner))

    free.foreach: (name, info) =>
      binds += Bind(name, info(0), info(1), Ref(info(2)), info(2), info(1))

    // Build accessors and the maps that redirect captured references to them.
    val getters: scm.Map[Symbol, Symbol] = scm.Map()
    val setters: scm.Map[Symbol, Symbol] = scm.Map()

    def accessorsFor(bind: Bind): List[Statement] =
      val getterSymbol = Symbol.newMethod(owner, bind.name, ByNameType(bind.tpe))
      getters(bind.symbol) = getterSymbol

      val getter = (bind.tpe.asType: @unchecked) match
        case '[t] =>
          DefDef(getterSymbol, _ => Some('{ReplBridge.fetchLive[t](${Expr(bind.name)})}.asTerm))

      if !(bind.mutable && bind.writeBack) then List(getter) else
        val setterType =
          MethodType(List("value"))(_ => List(bind.tpe), _ => TypeRepr.of[Unit])

        val setterSymbol = Symbol.newMethod(owner, bind.name+"_=", setterType)
        setters(bind.symbol) = setterSymbol

        val setter = DefDef(setterSymbol, params =>
          val value: Term = params.head.head.asInstanceOf[Term]
          val boxed: Expr[Object] = '{${value.asExprOf[Any]}.asInstanceOf[Object]}
          Some('{ReplBridge.updateLive(${Expr(bind.name)}, $boxed)}.asTerm))

        List(getter, setter)

    val accessors: List[Statement] = binds.to(List).flatMap(accessorsFor)

    // Redirect each captured reference in a lifted definition to its accessor: a
    // read becomes a call to the getter, an assignment a call to the setter.
    val redirect = new TreeMap:
      override def transformTerm(tree: Term)(owner: Symbol): Term = tree match
        case Assign(lhs, rhs) if lhs.symbol.exists && setters.contains(lhs.symbol) =>
          Apply(Ref(setters(lhs.symbol)), List(transformTerm(rhs)(owner)))

        case ident: Ident if ident.symbol.exists && getters.contains(ident.symbol) =>
          Ref(getters(ident.symbol))

        case other =>
          super.transformTerm(other)(owner)

    val redirected: List[Statement] = lifted.to(List).map(redirect.transformStatement(_)(owner))

    // The quote-built accessor bodies carry `Inlined` nodes whose `call`
    // references this (`@experimental`) object; strip them so the recompiled seed
    // refers only to `ReplBridge` and stdlib symbols.
    val stripper = new TreeMap:
      override def transformTerm(tree: Term)(owner: Symbol): Term = tree match
        case Inlined(_, Nil, expansion) => transformTerm(expansion)(owner)
        case other                      => super.transformTerm(other)(owner)

    val seedStatements: List[Statement] = accessors ++ redirected

    val pickled: List[String] =
      if seedStatements.isEmpty then Nil else
        // Re-own the whole block to one owner so the lifted definitions (owned by
        // the call site) and the accessors (owned by the splice) pickle under a
        // single owner; the seed compiler then re-roots them onto the module.
        val block = Block(seedStatements, '{()}.asTerm).changeOwner(owner)
        val stripped = stripper.transformTerm(block)(owner)
        val tree = Inlined(None, Nil, stripped).asInstanceOf[tpd.Tree]
        PickledQuotes.pickleQuote(tree)(using quotes.asInstanceOf[QuotesImpl].ctx)

    val importsExpr: Expr[List[String]] = Expr(imports.to(List))
    val preludeExpr: Expr[Repl.Prelude] = '{Repl.Prelude($importsExpr, ${Expr(pickled)})}

    ' {
        val repl: Repl[version] =
          Repl.make[version]($preludeExpr)(using $scalac, $classloader, $temporary)

        $ {
            // Each binding's value is registered live, keyed by session and name;
            // a free `var` also registers a consumer that assigns back to the host.
            val puts: List[Expr[Unit]] = binds.to(List).flatMap: bind =>
              val read: Expr[Object] = '{${bind.value.asExprOf[Any]}.asInstanceOf[Object]}

              val supplier: Expr[ju.function.Supplier[Object]] =
                '{new ju.function.Supplier[Object] { def get(): Object = $read }}

              val put: Expr[Unit] =
                '{ReplBridge.putSupplier(repl.session, ${Expr(bind.name)}, $supplier)}

              if !(bind.mutable && bind.writeBack) then List(put) else
                val methodType =
                  MethodType(List("value"))(_ => List(TypeRepr.of[Object]), _ => TypeRepr.of[Unit])

                val setter: Expr[Object => Unit] =
                  Lambda(Symbol.spliceOwner, methodType, (_, params) =>
                    val value: Term = params.head.asInstanceOf[Term]

                    (bind.symbol.info.widen.asType: @unchecked) match
                      case '[t] =>
                        val cast = '{${value.asExprOf[Object]}.asInstanceOf[t]}.asTerm
                        Assign(Ref(bind.symbol), cast)
                  ).asExprOf[Object => Unit]

                // `accept`'s parameter is typed `Object | Null` so the override
                // matches Java's `Consumer` under explicit-nulls.
                val consumer: Expr[ju.function.Consumer[Object]] =
                  ' { val function = $setter

                      new ju.function.Consumer[Object]:
                        def accept(value: Object | Null): Unit =
                          function(value.asInstanceOf[Object]) }

                val putSetter: Expr[Unit] =
                  '{ReplBridge.putSetter(repl.session, ${Expr(bind.name)}, $consumer)}

                List(put, putSetter)

            Expr.block(puts, 'repl)
          }
      }
