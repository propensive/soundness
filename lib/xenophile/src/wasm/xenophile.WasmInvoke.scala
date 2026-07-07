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
import gossamer.*
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

    // The requested result type determines both the WIT carrier the import returns (told to
    // `witImportCall` as `classOf[Carrier]`) and how that carrier is decoded. `deriveResult`
    // computes the pair; the carrier for a `list<tuple<…>>` mentions the scala-wasm `Tuple2` class,
    // which is resolved (and only ever appears) in this downstream-expanded code.
    val (carrier, decode) = deriveResult[result]

    // Emit `<decode>(witImportCall(module, function, classOf[Carrier]))`. The import call is built
    // by looking up `witImportCall` in the *downstream* classpath (the scala-wasm library), so this
    // macro needs no dependency on it.
    val witImportCall = Symbol.requiredMethod("scala.scalajs.wit.witImportCall")

    // A raw class literal (not a quoted `classOf[T]`, which arrives at the backend wrapped in
    // adaptation nodes that its literal-`classOf` check rejects).
    val classOfCarrier = Literal(ClassOfConstant(carrier))

    // The WIT result type as text, straight from the `.wit` definition, so the backend has the full
    // shape (e.g. `list<tuple<string, string>>`) that `classOf` cannot carry: `classOf` erases
    // nested type arguments, collapsing a `tuple<string, string>` to a fieldless tuple. `classOf`
    // still supplies the (erasure-safe) IR result type.
    val witType = Literal(StringConstant(prototype.result.text.s))

    // The `sigAndArgs` argument must be a `Repeated` ascribed with the tree-level repeated type
    // (`scala.<repeated>[Any]` — not `Seq[Any]`, which reads as a single sequence value and gets
    // re-wrapped as one vararg element).
    val repeatedAny = defn.RepeatedParamClass.typeRef.appliedTo(TypeRepr.of[Any])

    val varargs =
      Typed(Repeated(List(classOfCarrier, witType), TypeTree.of[Any]), Inferred(repeatedAny))

    val callTerm =
      Apply
        ( Ref(witImportCall),
          List(Expr(module.s).asTerm, Expr(function.s).asTerm, varargs) )

    decode(callTerm.asExprOf[Any]).asExprOf[result]

  // The WIT carrier a result type crosses the boundary as, paired with a function decoding that
  // carrier (an `Any`, the erased `witImportCall` result) back to the result. A leaf type uses its
  // `Decodable in Wasm` codec directly; a `List[(A, B)]` is a `list<tuple<Ac, Bc>>`, carried as an
  // `Array[scala.scalajs.wit.Tuple2[Ac, Bc]]` and decoded element-wise.
  private def deriveResult[result: Type](using quotes: Quotes)
  :   (quotes.reflect.TypeRepr, Expr[Any] => Expr[Any]) =

    import quotes.reflect.*

    def leaf(tpe: TypeRepr): (TypeRepr, Expr[Any] => Expr[Any]) =
      tpe.asType.absolve match
        case '[leafType] =>
          val decodable = Expr.summon[leafType is Decodable in Wasm].getOrElse:
            halt(m"xenophile: no way to read a ${tpe.show} from a WIT boundary")

          (carrierType(decodable), call => '{$decodable.decoded(Wasm($call))})

    val listClass = Symbol.requiredClass("scala.collection.immutable.List")

    // A `list<tuple<Ac, Bc>>` (Scala `List[(A, B)]`), if that is the shape of the result.
    def listOfPairs(leftType: TypeRepr, rightType: TypeRepr): (TypeRepr, Expr[Any] => Expr[Any]) =
      leftType.asType.absolve match case '[left] => rightType.asType.absolve match
        case '[right] =>
          val (leftCarrier, leftDecode) = leaf(leftType)
          val (rightCarrier, rightDecode) = leaf(rightType)

          // The carrier element is scala-wasm's `Tuple2`, whose name is resolved here (downstream),
          // so it never appears in `xenophile.wasm`'s own (scala-wasm-free) compilation.
          val witTuple2 = Symbol.requiredClass("scala.scalajs.wit.Tuple2")
          val tupleCarrier = witTuple2.typeRef.appliedTo(List(leftCarrier, rightCarrier))
          val arrayCarrier = defn.ArrayClass.typeRef.appliedTo(tupleCarrier)
          val pairType = TypeRepr.of[(left, right)]

          val decode: Expr[Any] => Expr[Any] = call =>
            // Each element is a `scala.scalajs.wit.Tuple2`; read `_1`/`_2`, decode them, and build
            // a Scala `(left, right)`. Done through a reflective `map` because the element type is
            // not nameable in this module.
            val method = MethodType(List("tuple"))(_ => List(TypeRepr.of[Any]), _ => pairType)

            val mapper = Lambda(Symbol.spliceOwner, method, (_, params) =>
              val tuple = TypeApply(Select.unique(params.head.asInstanceOf[Term], "asInstanceOf"),
                  List(Inferred(tupleCarrier)))

              val leftValue = leftDecode(Select.unique(tuple, "_1").asExprOf[Any])
              val rightValue = rightDecode(Select.unique(tuple, "_2").asExprOf[Any])
              '{(${leftValue.asExprOf[left]}, ${rightValue.asExprOf[right]})}.asTerm)

            val wrapped =
              '{scala.collection.immutable.ArraySeq.unsafeWrapArray($call.asInstanceOf[Array[Any]])}

            val mapped = Select.overloaded(wrapped.asTerm, "map", List(pairType), List(mapper))
            Select.unique(mapped, "toList").asExprOf[Any]

          (arrayCarrier, decode)

    TypeRepr.of[result].dealias match
      case AppliedType(list, List(element)) if list.typeSymbol == listClass =>
        element.dealias match
          case AppliedType(tuple, List(leftType, rightType))
          if tuple.typeSymbol == defn.TupleClass(2) =>
            listOfPairs(leftType, rightType)

          case _ =>
            leaf(TypeRepr.of[result])

      case _ =>
        leaf(TypeRepr.of[result])

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
