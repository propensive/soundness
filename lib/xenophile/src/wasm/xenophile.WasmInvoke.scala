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
package xenophile

import scala.quoted.*

import anticipation.*
import distillate.*
import fulminate.*
import prepositional.*
import rudiments.*
import vacuous.*

// The terminal materializer for the WIT ecosystem: turns a fully-applied `Foreign` invocation into
// a real Wasm Component Model import call (`scala.scalajs.wit.witImportCall`, lowered by the
// scala-wasm compiler). Unlike querencia's runtime `Javascript.serialize`, this runs at compile
// time, so it is a macro that deconstructs the inline navigation (the `invoke` extension in the
// module's package object must be applied directly to an inline chain — not to a `val`).
object WasmInvoke:
  def invoke[result: Type](self: Expr[Foreign])(using quotes: Quotes): Expr[result] =
    import quotes.reflect.*

    // The receiver carries the source language (`Origin`) in its refined type; the WIT function it
    // was reached through is recovered from the `Foreign.Expression` the navigation built.
    val (_, origin) = Xenophile.receiver(self)

    // The navigation expands to `Foreign.make(<AST>).asInstanceOf[…]`, and reaching this macro
    // through further `inline` definitions (e.g. an inline given publishing a deferred import)
    // nests it in `Inlined`/`Typed` layers that `underlyingArgument` cannot fold away. So the AST
    // is recovered by term-level stripping, as in `JsInvoke`, rather than quote-pattern matching.
    def strip(term: Term): Term = term match
      case Inlined(_, _, body)                        => strip(body)
      case Typed(expr, _)                             => strip(expr)
      case Block(Nil, expr)                           => strip(expr)
      case TypeApply(Select(expr, "asInstanceOf"), _) => strip(expr)
      case _                                          => term

    // `.tt` desugars to `tt("…")`; recover the string from the operand (or a bare string literal).
    def stringOf(term: Term): Text = strip(term).absolve match
      case Literal(StringConstant(string)) => string.tt

    def literal(term: Term): Text = strip(term).absolve match
      case Apply(Ident("tt"), List(argument)) => stringOf(argument)

    def notCall: Nothing =
      halt(m"xenophile: `invoke` expects a foreign function invocation, `interface.function(args)`")

    val expression = strip(self.asTerm.underlyingArgument).absolve match
      case Apply(Select(_, "make"), List(argument)) => strip(argument)
      case _                                        => notCall

    // Either an applied call — `Expression.Apply(select, args)`, whose companion `apply` takes two
    // arguments — or the bare selection of a zero-parameter WIT function (`Expression.Select`,
    // whose companion `apply` takes three): `interface.function.invoke[R]`. The latter is
    // preferred inside `inline` definitions, where re-inlining an empty-varargs application trips
    // path-dependent type avoidance.
    val selectNode = expression match
      case Apply(Select(_, "apply"), List(node, _)) => strip(node)
      case _                                        => expression

    val (owner, function) = selectNode.absolve match
      case Apply(Select(_, "apply"), List(_, member, owner)) => (literal(owner), literal(member))
      case _                                                 => notCall

    // Look up the function's module id (e.g. `wasi:random/random@0.2.0`) from the definitions.
    val members = Xenophile.definitions(origin, Xenophile.locusOf(origin)).at(owner).or:
      halt(m"xenophile: the foreign type $owner is not defined")

    val prototype = members.at(function).or:
      halt(m"xenophile: the foreign type $owner has no member $function")

    val module = prototype.module.or:
      halt(m"xenophile: $owner.$function is not addressable as a WIT import (it has no module id)")

    // The result crosses the boundary via its `Decodable in Wasm` codec; its `Carrier` type is what
    // the import returns and what `witImportCall` is told (as `classOf[Carrier]`).
    val decodable = Expr.summon[result is Decodable in Wasm].getOrElse:
      halt(m"xenophile: no way to read a ${TypeRepr.of[result].show} from a WIT boundary")

    val carrier = carrierType(decodable)

    // Emit `decodable.decoded(Wasm(witImportCall(module, function, classOf[Carrier])))`. The import
    // call is built by looking up `witImportCall` in the *downstream* classpath (the scala-wasm
    // library), so this macro needs no dependency on it.
    val witImportCall = Symbol.requiredMethod("scala.scalajs.wit.witImportCall")

    // A raw class literal (not a quoted `classOf[T]`, which arrives at the backend wrapped in
    // adaptation nodes that its literal-`classOf` check rejects).
    val classOfCarrier = Literal(ClassOfConstant(carrier))

    // The `sigAndArgs` argument must be a `Repeated` ascribed with the tree-level repeated type
    // (`scala.<repeated>[Any]` — not `Seq[Any]`, which reads as a single sequence value and gets
    // re-wrapped as one vararg element).
    val repeatedAny = defn.RepeatedParamClass.typeRef.appliedTo(TypeRepr.of[Any])

    val varargs =
      Typed(Repeated(List(classOfCarrier), TypeTree.of[Any]), Inferred(repeatedAny))

    val callTerm =
      Apply
        ( Ref(witImportCall),
          List(Expr(module.s).asTerm, Expr(function.s).asTerm, varargs) )

    val call = callTerm.asExprOf[Any]

    '{$decodable.decoded(Wasm($call))}

  // The `Carrier` type of a summoned `Encodable`/`Decodable in Wasm` codec, read from the `Carrier`
  // refinement member of the codec's (precise) type.
  private def carrierType(using quotes: Quotes)(codec: Expr[Any]): quotes.reflect.TypeRepr =
    import quotes.reflect.*

    def find(repr: TypeRepr): Optional[TypeRepr] = repr.dealias match
      case Refinement(_, "Carrier", TypeBounds(_, hi)) => hi
      case Refinement(parent, _, _)                    => find(parent)
      case AndType(left, right)                        => find(left).or(find(right))
      case _                                           => Unset

    find(codec.asTerm.tpe.widen).or:
      halt(m"xenophile: the WIT codec does not expose a `Carrier` type")
