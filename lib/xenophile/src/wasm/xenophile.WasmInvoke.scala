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

    // The elements of the `Expr.ofList`-built argument list of an `Expression.Apply`.
    def argumentList(term: Term): List[Term] = strip(term) match
      case Apply(_, List(varargs)) => strip(varargs) match
        case Repeated(elements, _) => elements.map(strip)
        case _                     => Nil

      case _ =>
        Nil

    // The Scala value wrapped by the `Foreign.converter` `Conversion` in a navigation operand (a
    // converted argument, or a `WitHandle` receiver): the argument of the conversion's `apply`.
    def convertedValue(term: Term): Term =
      var found: Optional[Term] = Unset

      val traverser = new TreeTraverser:
        override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match
          case Apply(Select(qualifier, "apply"), List(value))
          if qualifier.tpe <:< TypeRepr.of[Conversion[Nothing, Foreign]] =>
            if found.absent then found = value

          case _ =>
            traverseTreeChildren(tree)(owner)

      traverser.traverseTree(term)(Symbol.spliceOwner)

      found.or:
        halt(m"xenophile: a foreign operand must be a Scala value with an `Interoperable` instance")

    // Either an applied call — `Expression.Apply(select, args)`, whose companion `apply` takes two
    // arguments — or the bare selection of a zero-parameter WIT function (`Expression.Select`,
    // whose companion `apply` takes three): `interface.function.invoke[R]`. The latter is
    // preferred inside `inline` definitions, where re-inlining an empty-varargs application trips
    // path-dependent type avoidance.
    val (selectNode, argumentTerms) = expression match
      case Apply(Select(_, "apply"), List(node, arguments)) =>
        (strip(node), argumentList(arguments))

      case _ =>
        (expression, Nil)

    val (receiverNode, owner, function) = selectNode.absolve match
      case Apply(Select(_, "apply"), List(receiver, member, owner)) =>
        (strip(receiver), literal(owner), literal(member))

      case _ =>
        notCall

    // Look up the function's module id (e.g. `wasi:random/random@0.2.0`) from the definitions.
    val allDefinitions = Xenophile.definitions(origin, Xenophile.locusOf(origin))

    val members = allDefinitions.at(owner).or:
      halt(m"xenophile: the foreign type $owner is not defined")

    val prototype = members.at(function).or:
      halt(m"xenophile: the foreign type $owner has no member $function")

    val module = prototype.module.or:
      halt(m"xenophile: $owner.$function is not addressable as a WIT import (it has no module id)")

    // The module in which a named WIT type is defined: for a resource, taken from its methods; for
    // other named types (variants, records), the referencing function's own module — WASI types
    // are used within the interface that declares them.
    def definingModule(name: Text): Optional[Text] =
      allDefinitions.at(name).let(_.values.to(List).prim.let(_.module).or(Unset)).or(module)

    def facadeOf(name: Text): Symbol =
      facadeClass(definingModule(name).or(halt(m"xenophile: $name has no defining module")), name)

    // The requested result type determines both the WIT carrier the import returns (told to
    // `witImportCall` as `classOf[Carrier]`) and how that carrier is decoded. `deriveResult`
    // computes the pair; the carrier for a `list<tuple<…>>` mentions the scala-wasm `Tuple2` class,
    // which is resolved (and only ever appears) in this downstream-expanded code. Two result shapes
    // are handled here rather than in `deriveResult`, because they need the WIT prototype:
    //
    //  -  A `WitHandle of topic` result is a WIT resource: its carrier is the resource's facade
    //     class, and the raw handle is wrapped in a `WitHandle`.
    //  -  A `Unit` result against a WIT `result<…>` function crosses as a `scala.scalajs.wit.Result`
    //     and is checked: an `Err` raises an exception, an `Ok` becomes `()`.
    val witHandleTopic: Optional[TypeRepr] = TypeRepr.of[result].dealias match
      case Refinement(base, "Topic", TypeBounds(_, topic)) if base =:= TypeRepr.of[WitHandle] =>
        topic

      case _ =>
        Unset

    val (carrier, decode) = witHandleTopic.absolve match
      case topic: TypeRepr =>
        prototype.result.absolve match
          case Foreign.Type.Named(name) =>
            val facade = facadeOf(name)

            val decode: Expr[Any] => Expr[Any] = call =>
              val handle = '{new WitHandle($call)}.asTerm
              TypeApply(Select.unique(handle, "asInstanceOf"), List(TypeTree.of[result]))
                .asExprOf[Any]

            (facade.typeRef, decode)

          case other =>
            halt(m"xenophile: $owner.$function does not return a WIT resource")

      case _ =>
        if TypeRepr.of[result] =:= TypeRepr.of[Unit] then prototype.result match
          case Foreign.Type.Applied(constructor, _) if constructor.s == "result" =>
            val resultClass = Symbol.requiredClass("scala.scalajs.wit.Result")
            val okClass = Symbol.requiredClass("scala.scalajs.wit.Ok")
            val okType = AppliedType(okClass.typeRef, List(TypeBounds.empty))

            val decode: Expr[Any] => Expr[Any] = call =>
              '{  val outcome = $call

                  if !${ TypeApply(Select.unique('outcome.asTerm, "isInstanceOf"),
                      List(Inferred(okType))).asExprOf[Boolean] }
                  then throw new RuntimeException("WIT import returned an error: " + outcome)  }

            (resultClass.typeRef, decode)

          case _ =>
            halt(m"xenophile: `invoke[Unit]` requires a WIT `result<…>` function")

        else deriveResult[result]

    // Emit `<decode>(witImportCall(module, function, classOf[Carrier]))`. The import call is built
    // by looking up `witImportCall` in the *downstream* classpath (the scala-wasm library), so this
    // macro needs no dependency on it.
    val witImportCall = Symbol.requiredMethod("scala.scalajs.wit.witImportCall")

    // A raw class literal (not a quoted `classOf[T]`, which arrives at the backend wrapped in
    // adaptation nodes that its literal-`classOf` check rejects).
    val classOfCarrier = Literal(ClassOfConstant(carrier))

    // The `sigAndArgs` argument must be a `Repeated` ascribed with the tree-level repeated type
    // (`scala.<repeated>[Any]` — not `Seq[Any]`, which reads as a single sequence value and gets
    // re-wrapped as one vararg element).
    val repeatedAny = defn.RepeatedParamClass.typeRef.appliedTo(TypeRepr.of[Any])

    // The WIT result type as a structured descriptor, straight from the `.wit` definition, so the
    // backend has the full shape (e.g. `list<tuple<string, string>>`) that `classOf` cannot carry:
    // `classOf` erases nested type arguments, collapsing a `tuple<string, string>` to a fieldless
    // tuple. The descriptor is a tree of calls to the `wit*` markers in `scala.scalajs.wit`
    // (resolved here, downstream, like `witImportCall` itself), which the backend deconstructs by
    // symbol; `classOf` still supplies the (erasure-safe) IR result type.
    def marker(name: String): Term = Ref(Symbol.requiredMethod("scala.scalajs.wit." + name))

    val primitives =
      Set("bool", "u8", "u16", "u32", "u64", "s8", "s16", "s32", "s64", "f32", "f64", "char",
          "string")

    def descriptor(witType: Foreign.Type): Term = witType match
      case Foreign.Type.Named(name) if primitives(name.s) =>
        Apply(marker("witPrim"), List(Literal(StringConstant(name.s))))

      // A non-primitive name is a WIT resource, variant, record, flags or enum, conveyed by its
      // facade class, from which the backend derives the WIT type (via the class's annotations).
      case Foreign.Type.Named(name) =>
        Apply(marker("witNamed"), List(Literal(ClassOfConstant(facadeOf(name).typeRef))))

      case Foreign.Type.Union(List(inner, Foreign.Type.Named(none))) if none.s == "none" =>
        Apply(marker("witOption"), List(descriptor(inner)))

      case Foreign.Type.Applied(constructor, arguments) => constructor.s match
        case "list" =>
          Apply(marker("witList"), List(descriptor(arguments.head)))

        case "option" =>
          Apply(marker("witOption"), List(descriptor(arguments.head)))

        case "tuple" =>
          val elements = Repeated(arguments.map(descriptor), TypeTree.of[Any])
          Apply(marker("witTuple"), List(Typed(elements, Inferred(repeatedAny))))

        case "result" =>
          def arm(tpe: Foreign.Type): Term = tpe match
            case Foreign.Type.Named(name) if name.s == "_" => marker("witUnit")
            case other                                     => descriptor(other)

          Apply(marker("witResult"), List(arm(arguments.head), arm(arguments(1))))

        case other =>
          halt(m"xenophile: the WIT type constructor $other cannot cross a WIT boundary yet")

      case other =>
        halt(m"xenophile: the WIT type ${other.text} cannot cross a WIT boundary yet")

    // A resource method is imported under its Component Model name, `[method]resource.function`,
    // and takes the resource's handle — the underlying value of the `WitHandle` receiver — as its
    // first (borrow) parameter.
    val importName: Text = prototype.resource.lay(function): resource =>
      t"[method]$resource.$function"

    val receiverPair: List[Term] = prototype.resource.lay(List()): resource =>
      val facade = facadeOf(resource)

      // The receiver's handle is recovered at runtime from the `Foreign` value's expression — a
      // converted `WitHandle` is an `Expression.Literal` wrapping it — so the receiver may be a
      // bound `val`, not only an inline chain.
      val handle =
        '{  ${receiverNode.asExprOf[Foreign.Expression]} match
              case Foreign.Expression.Literal(handle: WitHandle) => handle.value
              case _ => throw new RuntimeException("xenophile: not a WIT resource handle")  }

      List(Literal(ClassOfConstant(facade.typeRef)), handle.asTerm)

    // Each declared argument crosses the boundary through its `Encodable in Wasm` codec, passed as
    // a `(classOf[Carrier], encoded)` pair.
    val argumentPairs: List[Term] = argumentTerms.flatMap: argument =>
      val value = convertedValue(argument)

      value.tpe.widen.asType.absolve match
        case '[argument] =>
          val encodable = Expr.summon[argument is Encodable in Wasm].getOrElse:
            halt(m"xenophile: no way to pass a ${value.tpe.widen.show} across a WIT boundary")

          val encoded = '{$encodable.encoded(${value.asExprOf[argument]}).value}.asTerm

          List(Literal(ClassOfConstant(carrierType(encodable))), encoded)

    val varargs =
      Typed
        ( Repeated
            ( List(classOfCarrier, descriptor(prototype.result)) ++ receiverPair ++ argumentPairs,
              TypeTree.of[Any] ),
          Inferred(repeatedAny) )

    val callTerm =
      Apply
        ( Ref(witImportCall),
          List(Expr(module.s).asTerm, Expr(importName.s).asTerm, varargs) )

    decode(callTerm.asExprOf[Any]).asExprOf[result]

  // Releases a WIT resource: emits the `[resource-drop]<resource>` Component Model import for the
  // handle's resource type, passing the handle as its only parameter.
  def dispose(self: Expr[WitHandle])(using quotes: Quotes): Expr[Unit] =
    import quotes.reflect.*

    val topic: Text = self.asTerm.tpe.widen.dealias match
      case Refinement(_, "Topic", TypeBounds(_, ConstantType(StringConstant(name)))) => name.tt
      case _ => halt(m"xenophile: the handle does not have a known WIT resource type")

    val origin = TypeRepr.of[Wit]
    val definitions = Xenophile.definitions(origin, Xenophile.locusOf(origin))

    val module = definitions.at(topic).let(_.values.to(List).prim.let(_.module).or(Unset)).or:
      halt(m"xenophile: the WIT resource $topic has no known module")

    val facade = facadeClass(module, topic)
    val witImportCall = Symbol.requiredMethod("scala.scalajs.wit.witImportCall")
    val repeatedAny = defn.RepeatedParamClass.typeRef.appliedTo(TypeRepr.of[Any])
    val unitMarker = Ref(Symbol.requiredMethod("scala.scalajs.wit.witUnit"))
    val handle = Select.unique(self.asTerm, "value")

    val varargs =
      Typed
        ( Repeated
            ( List(Literal(ClassOfConstant(TypeRepr.of[Unit])), unitMarker,
                  Literal(ClassOfConstant(facade.typeRef)), handle),
              TypeTree.of[Any] ),
          Inferred(repeatedAny) )

    val call =
      Apply
        ( Ref(witImportCall),
          List(Expr(module.s).asTerm, Expr(s"[resource-drop]$topic").asTerm, varargs) )

    '{  ${call.asExprOf[Any]}
        ()  }

  // The facade class representing a named WIT type on the downstream classpath, following
  // scala-wasm's layout: module `wasi:io/streams@0.2.0` and name `output-stream` give
  // `scala.scalajs.wasi.io.streams.OutputStream` (kebab-case package path → snake_case, type
  // name → UpperCamelCase).
  private def facadeClass(using quotes: Quotes)(definedIn: Text, name: Text)
  :   quotes.reflect.Symbol =

    import quotes.reflect.*

    def camel(part: String): String =
      if part.isEmpty then part
      else part.substring(0, 1).nn.toUpperCase.nn + part.substring(1).nn

    val base = definedIn.s.split("@").nn.head.toString
    val namespace = base.split(":").nn.head.toString
    val path = base.split(":").nn.last.toString.replace("/", ".").nn.replace("-", "_").nn
    val parts = name.s.split("-").nn
    val className = new java.lang.StringBuilder()
    var index = 0

    while index < parts.length do
      className.append(camel(parts(index).nn.toString))
      index += 1

    Symbol.requiredClass(s"scala.scalajs.$namespace.$path.$className")

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

    // The carrier and a decode for a single `tuple<Lc, Rc>` (Scala `(L, R)`): scala-wasm's `Tuple2`
    // (whose name is resolved here, downstream, so it never appears in `xenophile.wasm`'s own
    // scala-wasm-free compilation), and a `Term => Term` reading `_1`/`_2` off a raw `Tuple2` term.
    // A WIT `record` of two fields shares a `tuple`'s ABI, so this also serves a two-field record.
    def pairCodec(leftType: TypeRepr, rightType: TypeRepr)
    :   (TypeRepr, TypeRepr, Term => Term) =

      leftType.asType.absolve match case '[left] => rightType.asType.absolve match
        case '[right] =>
          val (leftCarrier, leftDecode) = leaf(leftType)
          val (rightCarrier, rightDecode) = leaf(rightType)
          val witTuple2 = Symbol.requiredClass("scala.scalajs.wit.Tuple2")
          val tupleCarrier = witTuple2.typeRef.appliedTo(List(leftCarrier, rightCarrier))
          val pairType = TypeRepr.of[(left, right)]

          val decodeTuple: Term => Term = raw =>
            val tuple = TypeApply(Select.unique(raw, "asInstanceOf"), List(Inferred(tupleCarrier)))
            val leftValue = leftDecode(Select.unique(tuple, "_1").asExprOf[Any])
            val rightValue = rightDecode(Select.unique(tuple, "_2").asExprOf[Any])
            '{(${leftValue.asExprOf[left]}, ${rightValue.asExprOf[right]})}.asTerm

          (tupleCarrier, pairType, decodeTuple)

    // A bare `tuple<A, B>` (Scala `(A, B)`).
    def pairOf(leftType: TypeRepr, rightType: TypeRepr): (TypeRepr, Expr[Any] => Expr[Any]) =
      val (tupleCarrier, _, decodeTuple) = pairCodec(leftType, rightType)
      (tupleCarrier, call => decodeTuple(call.asTerm).asExprOf[Any])

    // A `list<tuple<Ac, Bc>>` (Scala `List[(A, B)]`).
    def listOfPairs(leftType: TypeRepr, rightType: TypeRepr): (TypeRepr, Expr[Any] => Expr[Any]) =
      val (tupleCarrier, pairType, decodeTuple) = pairCodec(leftType, rightType)
      val arrayCarrier = defn.ArrayClass.typeRef.appliedTo(tupleCarrier)

      val decode: Expr[Any] => Expr[Any] = call =>
        // A reflective `map` because the element type (`Tuple2`) is not nameable in this module.
        val method = MethodType(List("tuple"))(_ => List(TypeRepr.of[Any]), _ => pairType)

        val mapper = Lambda(Symbol.spliceOwner, method,
            (_, params) => decodeTuple(params.head.asInstanceOf[Term]))

        val wrapped =
          '{scala.collection.immutable.ArraySeq.unsafeWrapArray($call.asInstanceOf[Array[Any]])}

        val mapped = Select.overloaded(wrapped.asTerm, "map", List(pairType), List(mapper))
        Select.unique(mapped, "toList").asExprOf[Any]

      (arrayCarrier, decode)

    val optionClass = Symbol.requiredClass("java.util.Optional")

    // An `option<T>` (Scala `Optional[T]` = `Unset | T`), carried as a `java.util.Optional[Tc]`.
    def optionOf(inner: TypeRepr): (TypeRepr, Expr[Any] => Expr[Any]) =
      val (innerCarrier, innerDecode) = leaf(inner)
      val carrier = optionClass.typeRef.appliedTo(List(innerCarrier))

      // `java.util.Optional` is nameable here, so ordinary quotes suffice (no reflective decode);
      // an absent value becomes `Unset`.
      val decode: Expr[Any] => Expr[Any] = call =>
        '{  val option = $call.asInstanceOf[java.util.Optional[Any]]
            if option.isPresent then ${innerDecode('{option.get})} else Unset  }

      (carrier, decode)

    TypeRepr.of[result].dealias match
      case OrType(left, right) if left =:= TypeRepr.of[Unset] =>
        optionOf(right)

      case OrType(left, right) if right =:= TypeRepr.of[Unset] =>
        optionOf(left)

      case AppliedType(tuple, List(leftType, rightType))
      if tuple.typeSymbol == defn.TupleClass(2) =>
        pairOf(leftType, rightType)

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
