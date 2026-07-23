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
package xenophile

import scala.collection.immutable.{List, Nil, ::}
import scala.quoted.*

import anticipation.*
import distillate.*
import fulminate.*
import prepositional.*

// The terminal materializer for JavaScript ecosystems (WebIDL, TypeScript): turns a fully-applied
// `Foreign` navigation into a real Scala.js dynamic call — `js.Dynamic.global.<owner>.<method>()` —
// decoded through the value's `Decodable in Js` codec. It is the JS analogue of `WasmInvoke`:
// where that emits a Wasm Component Model import (`witImportCall`), this emits the Scala.js dynamic
// interop nodes (`selectDynamic`/`applyDynamic` on `js.Dynamic`). Both are looked up in the
// *downstream* classpath by name, so this module needs no dependency on `scalajs-library` and still
// compiles for the JVM (the macro runs there); the call is lowered by Scala.js at link time.
//
// Like `WasmInvoke`, this v1 materializes nullary method calls (`receiver.method()`); argument
// marshalling through `Encodable in Js` is a follow-up, tracked alongside the `Wasm` form's.
object JsInvoke:
  def invoke[result: Type](self: Expr[Foreign])(using quotes: Quotes): Expr[result] =
    import quotes.reflect.*

    // The `js.Dynamic` interop entry points, resolved from the downstream Scala.js library. Named
    // rather than referenced, so this module compiles without depending on `scalajs-library`.
    val globalMethod = Symbol.requiredMethod("scala.scalajs.js.Dynamic.global")
    val dynamic = Symbol.requiredClass("scala.scalajs.js.Dynamic")
    val selectDynamic = dynamic.methodMember("selectDynamic").head
    val applyDynamic = dynamic.methodMember("applyDynamic").head
    // A concrete `js.Any` tree for the empty varargs element (an inferred one fails to resolve).
    val jsAnyTree = TypeTree.ref(Symbol.requiredClass("scala.scalajs.js.Any"))

    // The navigation the `invoke` is applied to expands to `Foreign.make(<AST>).asInstanceOf[…]`,
    // itself wrapped in `Inlined`/`Typed` layers (the inline `applyDynamic` leaves a `Foreign_this`
    // binding, so `underlyingArgument` cannot fold the `Inlined` away). Rather than quote-match
    // through those layers, we peel them at tree level and read the `Foreign.Expression` AST the
    // navigation macros built directly. (`.tt` desugars to a plain `tt("…")` application.)
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
      case other                              => stringOf(other)

    // `receiver.selectDynamic("name")`, yielding another `js.Dynamic`.
    def select(receiver: Term, name: Text): Term =
      Apply(Select(receiver, selectDynamic), List(Literal(StringConstant(name.s))))

    // `receiver.applyDynamic("method")()` — a nullary JS method call. The empty `js.Any*` argument
    // must be a `Repeated` (concrete element type) ascribed with the method's own repeated param
    // type (`js.Any*`, not `Seq[js.Any]`, which reads as a single sequence value).
    val varargType = applyDynamic.paramSymss.last.head.info

    def applyNullary(receiver: Term, method: Text): Term =
      val arguments = varargType.asType.absolve match
        case '[repeated] => Typed(Repeated(Nil, jsAnyTree), TypeTree.of[repeated])

      Apply
        ( Apply(Select(receiver, applyDynamic), List(Literal(StringConstant(method.s)))),
          List(arguments) )

    def notCall: Nothing = halt(m"xenophile: `invoke` expects a call, `receiver.method()`")

    // Peel to the `Foreign.make(<AST>)` application; take its argument — the expression AST.
    val expression = strip(self.asTerm.underlyingArgument).absolve match
      case Apply(Select(_, "make"), List(argument)) => strip(argument)
      case _                                        => notCall

    // Like `WasmInvoke`, the `owner` recorded on the `Select` names the type the method belongs to;
    // for a global JavaScript object (`Math`, `crypto`, …) that is the global property to read, so
    // the call is `global.<owner>.<method>()`. AST: `Apply(Select(_, member, owner), _)`
    // (`Apply.apply` has two arguments; the nested `Select.apply` has three).
    val selectNode = expression match
      case Apply(Select(_, "apply"), List(node, _)) => strip(node)
      case _                                        => notCall

    val (owner, function) = selectNode.absolve match
      case Apply(Select(_, "apply"), List(_, member, owner)) => (literal(owner), literal(member))
      case _                                                 => notCall

    val call = applyNullary(select(Ref(globalMethod), owner), function)

    // The result crosses the boundary via its `Decodable in Js` codec, which reads the `js.Dynamic`
    // return value at its `Carrier` type (via `Js.as`) and lifts it to the Scala type.
    val decodable = Expr.summon[result is Decodable in Js].getOrElse:
      halt(m"xenophile: no way to read a ${TypeRepr.of[result].show} from a JavaScript boundary")

    '{$decodable.decoded(Js(${call.asExprOf[Any]}))}
