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
    // `witImportCall` as `classOf[Carrier]`) and how that carrier is decoded. The derivation is
    // driven by the WIT result type, recursing structurally so that wrapped shapes compose (the
    // WASI HTTP response flow yields e.g. `option<result<result<incoming-response, error-code>>>`):
    //
    //  -  A WIT `result<…>` crosses as a `scala.scalajs.wit.Result` and is checked: an `Err`
    //     raises an exception; an `Ok` becomes `()` or its recursively-decoded payload.
    //  -  A WIT `option<T>` (read as `T | none`) crosses as a `java.util.Optional` and becomes
    //     `Unset` or its recursively-decoded payload.
    //  -  A named WIT type requested as a `WitHandle of topic` is a resource: its carrier is the
    //     resource's facade class, and the raw handle is wrapped in a `WitHandle`.
    //  -  A WIT `tuple<…>` (or a record, which shares its ABI) crosses as the scala-wasm `TupleN`
    //     of its recursively-derived element carriers and becomes the corresponding Scala tuple;
    //     a `list<T>` crosses as an `Array` of `T`'s carrier and becomes a `List`. The scala-wasm
    //     class names are resolved here, downstream, so they never appear in this module's own
    //     compilation.
    //  -  Anything else is a leaf, derived from the requested Scala type's codec (`deriveResult`).
    val optionClass = Symbol.requiredClass("java.util.Optional")

    // The payload type of an `option`, whichever way the dialect rendered it.
    def optionPayload(witType: Foreign.Type): Optional[Foreign.Type] = witType match
      case Foreign.Type.Union(List(inner, Foreign.Type.Named(none))) if none.s == "none" =>
        inner

      case Foreign.Type.Applied(constructor, List(inner)) if constructor.s == "option" =>
        inner

      case _ =>
        Unset

    // The mandatory part of an `Optional[T]` (a plain `Unset | T` union).
    def scalaPayload(scala: TypeRepr): Optional[TypeRepr] = scala.dealias match
      case OrType(left, right) if left =:= TypeRepr.of[Unset]  => right
      case OrType(left, right) if right =:= TypeRepr.of[Unset] => left
      case _                                                   => Unset

    def isHandle(scala: TypeRepr): Boolean = scala.dealias match
      case Refinement(base, "Topic", _) => base =:= TypeRepr.of[WitHandle]
      case _                            => false

    def isCase(scala: TypeRepr): Boolean = scala.dealias match
      case Refinement(base, "Topic", _) => base =:= TypeRepr.of[WitCase]
      case _                            => false

    def isTuple(scala: TypeRepr, arity: Int): Boolean = scala.dealias match
      case AppliedType(tuple, arguments) =>
        tuple.typeSymbol == defn.TupleClass(arity) && arguments.length == arity

      case _ =>
        false

    // The element WIT types of a `tuple<…>` parameter (a WIT record, whose ABI it shares, may be
    // declared as its structural tuple), or `Unset` for any other parameter shape.
    def parameterTuple(parameter: Foreign.Type): Optional[List[Foreign.Type]] = parameter match
      case Foreign.Type.Applied(constructor, elements) if constructor.s == "tuple" => elements
      case _                                                                       => Unset

    val listClass = Symbol.requiredClass("scala.collection.immutable.List")

    def isList(scala: TypeRepr): Boolean = scala.dealias match
      case AppliedType(list, List(_)) => list.typeSymbol == listClass
      case _                          => false

    def handleDecode(name: Text, scala: TypeRepr): (TypeRepr, Expr[Any] => Expr[Any]) =
      val facade = facadeOf(name)

      val decode: Expr[Any] => Expr[Any] = call => scala.asType.absolve match
        case '[scala] =>
          val handle = '{new WitHandle($call)}.asTerm
          TypeApply(Select.unique(handle, "asInstanceOf"), List(TypeTree.of[scala])).asExprOf[Any]

      (facade.typeRef, decode)

    def decodeFor(witType: Foreign.Type, scala: TypeRepr): (TypeRepr, Expr[Any] => Expr[Any]) =
      witType match
        case Foreign.Type.Applied(constructor, arguments) if constructor.s == "result" =>
          val resultClass = Symbol.requiredClass("scala.scalajs.wit.Result")
          val okClass = Symbol.requiredClass("scala.scalajs.wit.Ok")
          val okType = AppliedType(okClass.typeRef, List(TypeBounds.empty))

          def isOk(outcome: Expr[Any]): Expr[Boolean] =
            TypeApply(Select.unique(outcome.asTerm, "isInstanceOf"), List(Inferred(okType)))
              .asExprOf[Boolean]

          val errClass = Symbol.requiredClass("scala.scalajs.wit.Err")
          val errType = AppliedType(errClass.typeRef, List(TypeBounds.empty))

          def payload(outcome: Expr[Any], tpe: TypeRepr): Expr[Any] =
            val cast =
              TypeApply(Select.unique(outcome.asTerm, "asInstanceOf"), List(Inferred(tpe)))

            Select.unique(cast, "value").asExprOf[Any]

          // An `Err` raises a `WitError` carrying the error value (e.g. an `error-code` case), so
          // callers can recover which failure occurred (`WitError.name`) and translate it. The
          // `canThrowAny` import supplies the `CanThrow` capability a checked `throw` needs in
          // the expansion.
          def raiseError(outcome: Expr[Any]): Expr[Nothing] =
            '{  import _root_.scala.unsafeExceptions.canThrowAny
                throw new WitError(${payload(outcome, errType)})  }

          val decode: Expr[Any] => Expr[Any] =
            if scala =:= TypeRepr.of[Unit] then call =>
              '{  val outcome = $call
                  if !${isOk('outcome)} then ${raiseError('outcome)}  }
            else
              val (_, payloadDecode) = decodeFor(arguments.head, scala)

              call =>
                '{  val outcome = $call

                    if ${isOk('outcome)} then ${payloadDecode(payload('outcome, okType))}
                    else ${raiseError('outcome)}  }

          (resultClass.typeRef, decode)

        // A `tuple<…>` (or a record, whose ABI it shares) with the matching Scala tuple type:
        // element-wise recursion, carried as the scala-wasm `TupleN` of the element carriers.
        case Foreign.Type.Applied(constructor, elements)
        if constructor.s == "tuple" && isTuple(scala, elements.length) =>
          val fields = scala.dealias.typeArgs
          // Macro-time only: mapping with a lambda would let the decoders' minted quote
          // capabilities leak into the closure's capture set, so the traversal is an
          // explicit loop with each pair laundered (macro-under-cc precedent,
          // rep/DECISIONS.md).
          val derivedBuffer = List.newBuilder[(TypeRepr, Expr[Any] -> Expr[Any])]

          val pairs = elements.zip(fields).iterator
          while pairs.hasNext do
            val (element, field) = pairs.next()
            val (repr, decode) = decodeFor(element, field)
            val decode1: Expr[Any] -> Expr[Any] = caps.unsafe.unsafeAssumePure(decode)
            derivedBuffer += ((repr, decode1))

          val derived = derivedBuffer.result()
          val carriers = derived.map(_(0))

          val tupleClass =
            Symbol.requiredClass("scala.scalajs.wit.Tuple" + elements.length.toString)

          val tupleCarrier = tupleClass.typeRef.appliedTo(carriers)
          val scalaTuple = defn.TupleClass(elements.length).companionModule

          val decode: Expr[Any] => Expr[Any] = call =>
            val cast =
              TypeApply(Select.unique(call.asTerm, "asInstanceOf"), List(Inferred(tupleCarrier)))

            // Explicit loop like `derived` above (macro-under-cc precedent).
            val decodedBuffer = List.newBuilder[Term]

            val indexed = derived.zipWithIndex.iterator
            while indexed.hasNext do
              val (derivation, index) = indexed.next()
              decodedBuffer +=
                derivation(1)(Select.unique(cast, "_" + (index + 1).toString).asExprOf[Any]).asTerm

            val decoded = decodedBuffer.result()

            val applied =
              Apply
                ( TypeApply
                    ( Select.unique(Ref(scalaTuple), "apply"),
                      fields.map { field => Inferred(field) } ),
                  decoded )

            applied.asExprOf[Any]

          (tupleCarrier, decode)

        // A `list<T>` with a Scala `List[T]`: carried as an `Array` of `T`'s carrier, decoded
        // element-wise (reflectively, since the element carrier may not be nameable here).
        case Foreign.Type.Applied(constructor, List(element))
        if constructor.s == "list" && isList(scala) =>
          val elementType = scala.dealias.typeArgs.head
          val (elementCarrier, elementDecode) = decodeFor(element, elementType)
          val arrayCarrier = defn.ArrayClass.typeRef.appliedTo(elementCarrier)

          val decode: Expr[Any] => Expr[Any] = call =>
            val method = MethodType(List("element"))(_ => List(TypeRepr.of[Any]), _ => elementType)

            val mapper = Lambda(Symbol.spliceOwner, method,
                (_, parameters) => elementDecode(parameters.head.asInstanceOf[Term].asExprOf[Any])
                  . asTerm)

            val elements = '{$call.asInstanceOf[Array[Any]]}
            val wrapped = '{_root_.scala.collection.immutable.ArraySeq.unsafeWrapArray($elements)}

            val mapped = Select.overloaded(wrapped.asTerm, "map", List(elementType), List(mapper))
            Select.unique(mapped, "toList").asExprOf[Any]

          (arrayCarrier, decode)

        case _ =>
          val payloadWit = optionPayload(witType)
          val payloadScala = scalaPayload(scala)

          if payloadWit.present && payloadScala.present then
            val (innerCarrier, innerDecode) = decodeFor(payloadWit.vouch, payloadScala.vouch)

            // `java.util.Optional` is nameable here, so ordinary quotes suffice; an absent
            // value becomes `Unset`.
            val decode: Expr[Any] => Expr[Any] = call =>
              '{  val option = $call.asInstanceOf[java.util.Optional[Any]]
                  if option.isPresent then ${innerDecode('{option.get})} else Unset  }

            (optionClass.typeRef.appliedTo(List(innerCarrier)), decode)
          else
            witType match
              case Foreign.Type.Named(name) if isHandle(scala) =>
                handleDecode(name, scala)

              // A variant (or enum) result requested as a `WitCase of topic`: the facade case
              // object arrives, and its lower-kebab-case name is recovered at runtime.
              // Payload-carrying cases lose their payload.
              case Foreign.Type.Named(name) if isCase(scala) =>
                val facade = facadeOf(name)

                val decode: Expr[Any] => Expr[Any] = call => scala.asType.absolve match
                  case '[scala] =>
                    val witCase = '{new WitCase(WitCase.caseName($call))}.asTerm

                    TypeApply(Select.unique(witCase, "asInstanceOf"), List(TypeTree.of[scala]))
                    . asExprOf[Any]

                (facade.typeRef, decode)

              // A genuinely void function (`block: func()`): nothing to check or decode.
              case Foreign.Type.Named(name) if name.s == "unit" && scala =:= TypeRepr.of[Unit] =>
                (TypeRepr.of[Unit], call => '{val _ = $call})

              case _ =>
                if scala =:= TypeRepr.of[Unit]
                then halt(m"xenophile: `invoke[Unit]` requires a WIT `result<…>` or void function")
                else scala.asType.absolve match
                  case '[scala] => deriveResult[scala]

    val (carrier, decode) = decodeFor(prototype.result, TypeRepr.of[result].dealias)

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
      case Foreign.Type.Named(name) if name.s == "unit" || name.s == "_" =>
        marker("witUnit")

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
    // first (borrow) parameter. A constructor or static function is addressed through the resource
    // (`[constructor]resource`, `[static]resource.function`) but takes no receiver.
    val importName: Text = prototype.resource.lay(function): resource =>
      if prototype.static then
        if function == t"constructor" then t"[constructor]$resource"
        else t"[static]$resource.$function"
      else
        t"[method]$resource.$function"

    val receiverPair: List[Term] = prototype.resource.lay(List()): resource =>
      if prototype.static then List() else
        val facade = facadeOf(resource)

        // The receiver's handle is recovered at runtime from the `Foreign` value's expression — a
        // converted `WitHandle` is an `Expression.Literal` wrapping it — so the receiver may be a
        // bound `val`, not only an inline chain.
        val handle =
          '{  ${receiverNode.asExprOf[Foreign.Expression]} match
                case Foreign.Expression.Literal(handle: WitHandle) =>
                  handle.value

                case _ =>
                  throw new RuntimeException("xenophile: not a WIT resource handle")  }

        List(Literal(ClassOfConstant(facade.typeRef)), handle.asTerm)

    // Each declared argument crosses the boundary through its `Encodable in Wasm` codec, passed as
    // a `(witParam(classOf[Carrier], <descriptor>), encoded)` pair: the carrier class gives the
    // IR-level type the argument is lowered as, and the structured descriptor the exact WIT type
    // (which `classOf` alone erases, e.g. `option<string>`'s payload). An `Optional[T]` argument
    // (WIT `option<T>`) crosses as a `java.util.Optional` of `T`'s carrier.
    val parameterTypes: List[Foreign.Type] = prototype.parameters.or(Nil)

    val arity = parameterTypes.length

    if arity != argumentTerms.length then
      halt(m"xenophile: $owner.$function takes $arity parameters, not ${argumentTerms.length}")

    // A facade child's WIT case name: its lower-kebab-case simple name (`DnsTimeout` matches
    // `dns-timeout`). Case objects loaded from Scala 2 classfiles (rather than Tasty) report
    // their module-class name, with a trailing `$` to strip.
    def caseNameOf(child: Symbol): Text = child.name.stripSuffix("$").tt.uncamel.kebab

    // A payload-less `variant` case argument (a `WitCase of topic`): the facade case object,
    // selected by name at runtime from the sealed facade trait's children.
    def caseArgument(topic: Text, value: Term): (TypeRepr, Term) =
      val facade = facadeOf(topic)
      val selector = '{${value.asExprOf[Any]}.asInstanceOf[WitCase].name.s}.asTerm

      val caseDefs: List[CaseDef] = facade.children.flatMap: child =>
        if child.flags.is(Flags.Module) then
          val name = caseNameOf(child)
          List(CaseDef(Literal(StringConstant(name.s)), None, Ref(child.companionModule)))
        else
          List()

      val missing =
        CaseDef
          ( Wildcard(), None,
            '{throw new RuntimeException("xenophile: not a case of " + ${Expr(topic.s)})}.asTerm )

      (facade.typeRef, Match(selector, caseDefs :+ missing))

    // The base class a (possibly multiply-)refined type refines, and the constant string / aliased
    // type a named refinement member is set to — used to read `WitVariant`'s `Topic`/`Case`/
    // `Payload` phantom members regardless of the order the compiler nests them.
    // `.dealias` at each step because the phantom members are carried through `of` — a type alias
    // (`X of topic` = `of[X, topic]` = `X { type Topic = topic }`) — so a `WitVariant` argument
    // reads as `of[WitVariant[payload], topic] { type Case = … }`, whose base and `Topic` member are
    // hidden inside the unexpanded `of` until dealiased.
    def refinementBase(tpe: TypeRepr): TypeRepr = tpe.dealias match
      case Refinement(parent, _, _) => refinementBase(parent)
      case other                    => other

    def refinedString(tpe: TypeRepr, name: String): Optional[Text] = tpe.dealias match
      case Refinement(_, `name`, TypeBounds(_, ConstantType(StringConstant(text)))) =>
        text.tt

      case Refinement(parent, _, _) =>
        refinedString(parent, name)

      case _ =>
        Unset

    val witVariantClass = Symbol.requiredClass("xenophile.WitVariant")
    val witRecordAnnotation = Symbol.requiredClass("scala.scalajs.wit.annotation.WitRecord")

    // Constructs a scala-wasm facade value of WIT type `target` from a Scala `value`, recursing
    // structurally: a scala-wasm `TupleN` and a `@WitRecord` class are each built element-wise via
    // their companion `apply` (a record shares the tuple ABI, so its fields map to the Scala
    // tuple's positionally); a leaf is encoded through its `Encodable in Wasm` codec, whose carrier
    // is the facade field's type (the `scala.scalajs.wit.unsigned` types are aliases of the boxed
    // primitives). The facade classes are resolved here, downstream, so they never appear in this
    // module's own compilation.
    def buildFacade(target: TypeRepr, value: Term): Term =
      val symbol = target.typeSymbol

      if symbol.fullName.startsWith("scala.scalajs.wit.Tuple") then
        val elements = target.typeArgs
        val applier = Select.unique(Ref(symbol.companionModule), "apply")

        val built = elements.zipWithIndex.map: (element, index) =>
          buildFacade(element, Select.unique(value, "_" + (index + 1).toString))

        Apply(TypeApply(applier, elements.map(Inferred(_))), built)

      else if symbol.hasAnnotation(witRecordAnnotation) then
        // Constructor-parameter order, not `declaredFields` (which does not preserve it), so the
        // record's fields line up with the Scala tuple's positions and the companion `apply`.
        val params = symbol.primaryConstructor.paramSymss.flatten.filter(!_.isType)

        val built = params.zipWithIndex.map: (param, index) =>
          val field = symbol.declaredField(param.name)
          buildFacade(target.memberType(field), Select.unique(value, "_" + (index + 1).toString))

        Apply(Select.unique(Ref(symbol.companionModule), "apply"), built)

      else
        value.tpe.widen.dealias.asType.absolve match
          case '[leaf] =>
            val encodable = Expr.summon[leaf is Encodable in Wasm].getOrElse:
              halt(m"xenophile: no `Encodable in Wasm` to build a WIT payload of ${value.tpe.show}")

            // `Wasm#value` is `Any`; cast it to the facade field's carrier type (`target`) so it
            // conforms to the record/tuple `apply`'s parameter.
            val encoded = '{$encodable.encoded(${value.asExprOf[leaf]}).value}.asTerm
            TypeApply(Select.unique(encoded, "asInstanceOf"), List(Inferred(target)))

    // A payload-carrying `variant` case argument (a `WitVariant`): the facade case class named by
    // `caseName` (fixed at compile time, so no runtime dispatch), constructed with its record/
    // tuple/primitive payload built from the `WitVariant`'s payload — cast back to the Scala type
    // `payloadType` the phantom `Payload` carried — and encoded element-wise.
    def variantArgument(topic: Text, caseName: Text, payloadType: TypeRepr, value: Term)
    :   (TypeRepr, Term) =

      val facade = facadeOf(topic)

      val child = facade.children.find(caseNameOf(_) == caseName).getOrElse:
        halt(m"xenophile: the variant $topic has no case $caseName")

      val valueField = child.declaredFields.headOption.getOrElse:
        halt(m"xenophile: the variant case $topic.$caseName carries no payload")

      val caseValueType = child.typeRef.memberType(valueField)

      val castPayload =
        TypeApply
          ( Select.unique
              ( '{${value.asExprOf[Any]}.asInstanceOf[WitVariant[Any]].payload}.asTerm,
                "asInstanceOf" ),
            List(Inferred(payloadType)) )

      val built = buildFacade(caseValueType, castPayload)
      (facade.typeRef, Apply(Select.unique(Ref(child.companionModule), "apply"), List(built)))

    def encodedArgument(value: Term, parameter: Foreign.Type): (TypeRepr, Term) =
      val (carrier, encoded, alreadyOption) = value.tpe.widen.dealias match
        // A `tuple<…>` (or a record, whose ABI it shares) argument with a matching Scala tuple:
        // element-wise recursion, constructed as the scala-wasm `TupleN` of the element carriers —
        // the mirror of the decode path. Nested tuples, `option<T>` and `WitCase` elements are
        // handled by the per-element recursion into `encodedArgument`.
        case valueType
        if parameterTuple(parameter).let { es => isTuple(valueType, es.length) }.or(false) =>
          val elements = parameterTuple(parameter).vouch

          // Explicit loop, not a mapping lambda: minted quote capabilities must not leak into a
          // closure's capture set (macro-under-cc precedent, rep/DECISIONS.md).
          val encodedBuffer = List.newBuilder[(TypeRepr, Term)]
          val indexed = elements.iterator.zipWithIndex

          while indexed.hasNext do
            val (element, index) = indexed.next()
            val field = Select.unique(value, "_" + (index + 1).toString)
            encodedBuffer += encodedArgument(field, element)

          val encodedElements = encodedBuffer.result()
          val carriers = encodedElements.map(_(0))

          val tupleClass =
            Symbol.requiredClass("scala.scalajs.wit.Tuple" + elements.length.toString)

          val tupleCarrier = tupleClass.typeRef.appliedTo(carriers)

          val constructed =
            Apply
              ( TypeApply
                  ( Select.unique(Ref(tupleClass.companionModule), "apply"),
                    carriers.map(Inferred(_)) ),
                encodedElements.map(_(1)) )

          (tupleCarrier, constructed, false)

        // A statically-absent argument (a bare `Unset` against an `option<T>` parameter, e.g. the
        // `option<request-options>` of `wasi:http`'s `handle`) needs no payload codec at all.
        case tpe if tpe =:= TypeRepr.of[Unset] =>
          (optionClass.typeRef, '{java.util.Optional.empty[Any]().nn}.asTerm, true)

        // A resource argument (a `WitHandle of topic`, e.g. the `fields` passed to
        // `outgoing-request`'s constructor) crosses as its facade class, unwrapped to the raw
        // handle.
        case Refinement(base, "Topic", TypeBounds(_, ConstantType(StringConstant(topic))))
        if base =:= TypeRepr.of[WitHandle] =>
          val encoded = '{${value.asExprOf[Any]}.asInstanceOf[WitHandle].value}.asTerm
          (facadeOf(topic.tt).typeRef, encoded, false)

        case Refinement(base, "Topic", TypeBounds(_, ConstantType(StringConstant(topic))))
        if base =:= TypeRepr.of[WitCase] =>
          val (carrier, encoded) = caseArgument(topic.tt, value)
          (carrier, encoded, false)

        // A payload-carrying `variant` case argument (a `WitVariant`, e.g. the `ip-socket-address`
        // passed to `start-connect`): read its `Topic`/`Case` phantom members and payload type
        // argument, and build the named facade case around its payload.
        case tpe if refinementBase(tpe).typeSymbol == witVariantClass =>
          val topic = refinedString(tpe, "Topic").or(halt(m"xenophile: WitVariant has no Topic"))
          val caseName = refinedString(tpe, "Case").or(halt(m"xenophile: WitVariant has no Case"))
          val payloadType = refinementBase(tpe).typeArgs.head
          val (carrier, encoded) = variantArgument(topic, caseName, payloadType, value)
          (carrier, encoded, false)

        case OrType(left, right)
        if left =:= TypeRepr.of[Unset] || right =:= TypeRepr.of[Unset] =>
          val inner0 = if left =:= TypeRepr.of[Unset] then right else left

          inner0.asType.absolve match
            case '[inner] =>
              val encodable = Expr.summon[inner is Encodable in Wasm].getOrElse:
                halt(m"xenophile: no way to pass a ${inner0.show} across a WIT boundary")

              val optional = value.asExprOf[Optional[inner]]

              // `match`, not `let`/`lay`: `Optional`'s inline combinators crash `pickleQuotes`
              // inside staged code.
              val encoded =
                '{  $optional match
                      case Unset => java.util.Optional.empty[Any]().nn

                      case value =>
                        java.util.Optional.of($encodable.encoded(value.asInstanceOf[inner]).value)
                        . nn  }

              (optionClass.typeRef.appliedTo(List(carrierType(encodable))), encoded.asTerm, true)

        case other => other.asType.absolve match
          case '[argument] =>
            val encodable = Expr.summon[argument is Encodable in Wasm].getOrElse:
              halt(m"xenophile: no way to pass a ${other.show} across a WIT boundary")

            val encoded = '{$encodable.encoded(${value.asExprOf[argument]}).value}.asTerm
            (carrierType(encodable), encoded, false)

      // A plain (always-present) value against an `option<T>` parameter crosses as a present
      // `java.util.Optional`, and one against a `result<T, E>` parameter as an `Ok` (used by
      // `wasi:http`'s `response-outparam.set`), matching the parameter's descriptor.
      if optionPayload(parameter).present && !alreadyOption then
        val wrapped = '{java.util.Optional.of(${encoded.asExprOf[Any]}).nn}.asTerm
        (optionClass.typeRef.appliedTo(List(carrier)), wrapped)
      else
        parameter match
          case Foreign.Type.Applied(constructor, _) if constructor.s == "result" =>
            val okClass = Symbol.requiredClass("scala.scalajs.wit.Ok")
            val resultClass = Symbol.requiredClass("scala.scalajs.wit.Result")

            val wrapped =
              Apply
                ( TypeApply
                    ( Select(New(Inferred(okClass.typeRef)), okClass.primaryConstructor),
                      List(TypeTree.of[Any]) ),
                  List(encoded) )

            (resultClass.typeRef, wrapped)

          case _ =>
            (carrier, encoded)

    val argumentPairs: List[Term] =
      argumentTerms.zip(parameterTypes).flatMap: (argument, parameter) =>
        val (carrierRepr, encoded) = encodedArgument(convertedValue(argument), parameter)

        val slot =
          Apply
            ( marker("witParam"),
              List(Literal(ClassOfConstant(carrierRepr)), descriptor(parameter)) )

        List(slot, encoded)

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

      case _ =>
        halt(m"xenophile: the handle does not have a known WIT resource type")

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

  // The WIT carrier a leaf result type crosses the boundary as, paired with a function decoding
  // that carrier (an `Any`, the erased `witImportCall` result) back to the result, using its
  // `Decodable in Wasm` codec. Structured shapes (tuples, lists, options, results, resources) are
  // derived recursively — and WIT-driven — by `decodeFor` in `invoke`; this is its base case.
  private def deriveResult[result: Type](using quotes: Quotes)
  :   (quotes.reflect.TypeRepr, Expr[Any] => Expr[Any]) =

    import quotes.reflect.*

    val decodable = Expr.summon[result is Decodable in Wasm].getOrElse:
      halt(m"xenophile: no way to read a ${TypeRepr.of[result].show} from a WIT boundary")

    (carrierType(decodable), call => '{$decodable.decoded(Wasm($call))})

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
