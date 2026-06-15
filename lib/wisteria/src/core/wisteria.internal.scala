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
package wisteria

import scala.quoted.*
import scala.reflect.ClassTag

import anticipation.*
import denominative.*
import gigantism.*
import rudiments.*
import vacuous.*

object internal:
  inline def default[product, field](index: Int): Optional[field] =
    ${getDefault[product, field]('index)}

  // Inline accessors over the symbol-inspection macros, for use outside this object.
  inline def sumName[derivation]: Text = ${typeName[derivation]}
  inline def variantLabelList[derivation]: List[Text] = ${variantLabels[derivation]}
  inline def fieldLabelList[derivation]: List[Text] = ${fieldLabels[derivation]}

  // Resolves a field/variant instance via the real implicit search while *ignoring the typeclass's
  // wrapper givens* (the derivation's own `derived`, which route data types into a fresh graph).
  // This makes a field summon land on a *sibling* synthetic given (a recursive or shared type in the
  // same derivation block), a codec (`List[T]`, …), or a leaf — never re-entering the derivation.
  // Deferred to the splice site (a sibling val's nested `Quotes`) so the siblings emitted alongside
  // it are in scope.
  inline def field[typeclass[_], elem]: typeclass[elem] = ${fieldInstance[typeclass, elem]}

  // Whether `derivation` is a sum — for `derivedOne` to pick `disjunction` vs `conjunction`.
  inline def isSum[derivation]: Boolean = ${isSumMacro[derivation]}

  def isSumMacro[derivation: Type]: Macro[Boolean] =
    import quotes.reflect.*
    Expr(isSumType(TypeRepr.of[derivation]))

  def fieldLabels[derivation: Type]: Macro[List[Text]] =
    import quotes.reflect.*

    val labels =
      productType(TypeRepr.of[derivation]).typeSymbol.caseFields.map: field =>
        '{${Expr(field.name)}.tt}

    Expr.ofList(labels)


  // Whether `tpe` is a sum: a sealed trait / enum with `children`. A `Variant & Parent` intersection
  // (a sum's variant) is the variant — a product. A singleton (case object / parameterless enum
  // case) has a *term* symbol whose *type* symbol widens to the sum, so the term test comes first.
  def isSumType(using Quotes)(tpe: quotes.reflect.TypeRepr): Boolean =
    import quotes.reflect.*

    tpe.dealias match
      case AndType(_, _)          => false
      case Refinement(base, _, _) => isSumType(base)
      case other                  => other.termSymbol.isNoSymbol && other.typeSymbol.children.nonEmpty

  // Whether `tpe` is a (non-sum) product wisteria can build structurally.
  def isProductType(using Quotes)(tpe: quotes.reflect.TypeRepr): Boolean =
    import quotes.reflect.*

    !isSumType(tpe)
    && (tpe <:< TypeRepr.of[Product] || tpe <:< TypeRepr.of[scala.Singleton]
        || tpe.typeSymbol.caseFields.nonEmpty)

  // The type of a variant: a `TypeRef` for a case-class variant, a `TermRef` for a singleton.
  private def variantType(using Quotes)(child: quotes.reflect.Symbol): quotes.reflect.TypeRepr =
    if child.isType then child.typeRef else child.termRef

  // The variant type intersected with the sum parent (`variant & derivation`): keeps a parameterless
  // enum case distinct (its bare type widens to the sum) while conforming to `variant <: derivation`.
  private def variantWith(using Quotes)
    ( child: quotes.reflect.Symbol, parent: quotes.reflect.TypeRepr )
  :   quotes.reflect.TypeRepr =

    import quotes.reflect.*
    AndType(variantType(child), parent)

  // A "wrapper" given supplies `typeclass[T]` at a bare type parameter `T` — the derivation's own
  // `derived`, or a `summonFrom` catch-all routing data types in — unlike a structural codec like
  // `List[T] is Encodable` whose argument is an applied type. These are ignored during field/variant
  // resolution and reachability probing so neither re-enters the in-progress derivation.
  private def isWrapper(using Quotes)
    ( typeclassConstructor: quotes.reflect.TypeRepr, method: quotes.reflect.Symbol )
  :   Boolean =

    import quotes.reflect.*

    method.info match
      case poly: PolyType if poly.paramNames.size == 1 =>
        def strip(tpe: TypeRepr): TypeRepr = tpe match
          case method: MethodType => strip(method.resType)
          case other              => other

        strip(poly.resType) =:= typeclassConstructor.appliedTo(poly.param(0))

      case _ => false

  // The typeclass's wrapper givens (its `derived` etc.), to be ignored during resolution/probing.
  private def wrappersOf[typeclass[_]: Type](using Quotes): List[quotes.reflect.Symbol] =
    import quotes.reflect.*
    val typeclassConstructor = TypeRepr.of[typeclass]
    val owner = typeclassConstructor.appliedTo(TypeRepr.of[Any]).typeSymbol.maybeOwner

    if owner.isNoSymbol then Nil
    else (owner.methodMembers ++ owner.fieldMembers).filter(isWrapper(typeclassConstructor, _)).distinct

  // Resolves a field/variant instance via the real implicit search, ignoring the typeclass's wrapper
  // givens so it lands on a sibling synthetic given (a recursive/shared type), a codec, or a leaf —
  // never re-entering the in-progress derivation. Deferred to the use site (a sibling val's nested
  // `Quotes`) so the siblings generated alongside it are in scope.
  def fieldInstance[typeclass[_]: Type, elem: Type]: Macro[typeclass[elem]] =
    import quotes.reflect.*

    val wrappers = wrappersOf[typeclass]
    val instance = TypeRepr.of[typeclass[elem]]

    // Prefer a resolution that avoids the wrappers — a sibling, a codec, or a concrete leaf. Only if
    // none exists is `elem` something a wrapper handles specially, so fall back to the ordinary search.
    val result = Implicits.searchIgnoring(instance)(wrappers*) match
      case success: ImplicitSearchSuccess => success
      case _                              => Implicits.search(instance)

    result.absolve match
      case success: ImplicitSearchSuccess => success.tree.asExprOf[typeclass[elem]]
      case failure: ImplicitSearchFailure => report.errorAndAbort(failure.explanation)

  // `self.derivedOne[elem]`, built in the given nested `Quotes` (a sibling val's scope).
  private def derivedOneInstance(using nested: Quotes)(self: Expr[Any], elem: Type[?]): Expr[Any] =
    import nested.reflect.*
    val selfTerm = self.asTerm
    val method = selfTerm.tpe.typeSymbol.methodMember("derivedOne").head

    Select(selfTerm, method).appliedToType(TypeRepr.of(using elem)).asExpr

  // Derives `typeclass[derivation]` by emitting a block of mutually-recursive synthetic
  // `given lazy val`s — one per distinct product/sum type reachable from `derivation` that a
  // structural derivation would otherwise build. Each instance is built once via `self.derivedOne`,
  // its fields/variants resolving through `field` (a real `searchIgnoring`) onto a sibling given, a
  // codec, or a leaf. Sharing deduplicates repeated subtrees (so a large graph stays within JVM
  // class limits) and ties recursive types safely (a lazy val may reference itself by name). The
  // lazy vals sit in this block at the use site, so a `summonInline[Mirror]` in a body captures the
  // surrounding frame — letting method-local and object-nested types construct via `fromProduct`.
  def deriveGraph[typeclass[_]: Type, derivation: Type](self: Expr[Any])
  :   Macro[typeclass[derivation]] =

    import quotes.reflect.*

    if self.asTerm.tpe.typeSymbol.methodMember("derivedOne").isEmpty then
      report.errorAndAbort("wisteria: the derivation defines no `derivedOne`")

    val typeclassConstructor = TypeRepr.of[typeclass]
    val wrappers = wrappersOf[typeclass]

    def instanceOf(tpe: TypeRepr): TypeRepr = typeclassConstructor.appliedTo(tpe)

    def resolves(tpe: TypeRepr): Boolean =
      Implicits.searchIgnoring(instanceOf(tpe))(wrappers*) match
        case _: ImplicitSearchSuccess => true
        case _                        => false

    def codecFunction(tpe: TypeRepr, args: List[TypeRepr]): TypeRepr =
      defn.FunctionClass(args.length, isContextual = true).typeRef
        . appliedTo(args.map(instanceOf) :+ instanceOf(tpe))

    def isCodec(tpe: TypeRepr, args: List[TypeRepr]): Boolean =
      args.nonEmpty
      && (Implicits.searchIgnoring(codecFunction(tpe, args))(wrappers*) match
            case _: ImplicitSearchSuccess => true
            case _                        => false)

    val reachable = scala.collection.mutable.LinkedHashMap[String, TypeRepr]()
    val seen = scala.collection.mutable.HashSet[String]()

    // The root is always derived structurally when an ADT (never probed with `resolves`, which would
    // find its own `derives TC` companion given and make the block self-reference, a lazy-val cycle).
    // Non-root types are probed so they share existing instances; a codec's element types are followed.
    def visit(raw: TypeRepr, isRoot: Boolean): Unit =
      val tpe = raw.dealias
      val key = tpe.show

      if !seen.contains(key) then
        seen += key
        val args = tpe.typeArgs

        if !isRoot && resolves(tpe) then ()
        else if isCodec(tpe, args) then args.foreach(visit(_, false))
        else if isSumType(tpe) then
          reachable(key) = tpe
          tpe.typeSymbol.children.foreach { child => visit(variantWith(child, tpe), false) }
        else if isProductType(tpe) then
          reachable(key) = tpe
          val product = productType(tpe)
          product.typeSymbol.caseFields.foreach { field => visit(product.memberType(field), false) }

    val rootType = TypeRepr.of[derivation].dealias
    visit(rootType, isRoot = true)

    val owner = Symbol.spliceOwner

    val bindings: List[(String, TypeRepr, Symbol)] =
      reachable.toList.zipWithIndex.map: (entry, index) =>
        val (key, tpe) = entry
        val flags = Flags.Given | Flags.Lazy
        val symbol = Symbol.newVal(owner, "wisteria$"+index, instanceOf(tpe), flags, Symbol.noSymbol)
        (key, tpe, symbol)

    val symbolByKey = bindings.map { (key, _, symbol) => key -> symbol }.toMap

    // Each body is built in the val's *own* nested `Quotes` (`symbol.asQuotes`), so the `field`
    // resolutions inside `derivedOne` resolve in that val's scope — where the sibling givens are.
    val definitions: List[ValDef] = bindings.map: (_, tpe, symbol) =>
      ValDef(symbol, Some(derivedOneInstance(using symbol.asQuotes)(self, tpe.asType).asTerm))

    def resolveDirect(tpe: TypeRepr): Term =
      Implicits.searchIgnoring(instanceOf(tpe))(wrappers*).absolve match
        case success: ImplicitSearchSuccess => success.tree
        case failure: ImplicitSearchFailure => report.errorAndAbort(failure.explanation)

    def elementInstance(tpe: TypeRepr): Term = symbolByKey.get(tpe.dealias.show) match
      case Some(symbol) => Ref(symbol)
      case None         => resolveDirect(tpe)

    val rootArgs = rootType.typeArgs

    val root: Term = symbolByKey.get(rootType.show) match
      case Some(symbol) => Ref(symbol)

      case None =>
        if !isCodec(rootType, rootArgs) then resolveDirect(rootType)
        else Implicits.searchIgnoring(codecFunction(rootType, rootArgs))(wrappers*).absolve.match
          case success: ImplicitSearchSuccess =>
            Select.unique(success.tree, "apply").appliedToArgs(rootArgs.map(elementInstance))

          case failure: ImplicitSearchFailure => report.errorAndAbort(failure.explanation)

    Block(definitions, root.changeOwner(owner)).asExprOf[typeclass[derivation]]


  // Wraps a homogeneous list of field results into an `IArray`, summoning the `ClassTag` at the
  // expansion site (where `result` is concrete).
  private def immutableArray[result: Type](results: List[Expr[result]])(using Quotes)
  :   Expr[IArray[result]] =

    import quotes.reflect.*

    val classTag = Expr.summon[ClassTag[result]].getOrElse:
      report.errorAndAbort("wisteria: no ClassTag available for the result type")

    '{Array[result](${Varargs(results)}*)(using $classTag).immutable(using Unsafe)}

  // The derivation type may be an intersection `Variant & Parent` (when deriving a sum's
  // variant); resolve to the variant side. The parent of `Variant & Parent` is the sum, which has
  // no term symbol and does have `children`; everything else is the variant. (A parameterless enum
  // case is a `TermRef` whose *type* symbol widens to the sum, so the term test comes first.)
  private def productType(using Quotes)(repr: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr =
    import quotes.reflect.*

    def parent(repr: TypeRepr): Boolean =
      repr.termSymbol.isNoSymbol && repr.typeSymbol.children.nonEmpty

    repr.dealias match
      case AndType(left, right)   => if parent(left) then productType(right) else productType(left)
      case Refinement(base, _, _) => productType(base)
      case other                  => other

  // Construct a product value from its field-value terms via the primary constructor — no `Mirror`,
  // no `fromProduct`. The constructed type is `productType(derivation)` (stripping any `over` carrier
  // refinement). For an inner class the type carries its prefix (`Outer.Inner` / `outer.Inner`), so
  // `New` of that type tree resolves the outer pointer automatically; type arguments (generic case
  // classes) are reapplied to the constructor. A singleton (case object / parameterless enum case) is
  // referenced directly.
  private def constructProduct[derivation: Type](using Quotes)
    ( arguments: List[quotes.reflect.Term] )
  :   quotes.reflect.Term =

    import quotes.reflect.*
    val tpe = productType(TypeRepr.of[derivation])
    val symbol = tpe.typeSymbol

    if !tpe.termSymbol.isNoSymbol then Ref(tpe.termSymbol)
    else
      // `TypeTree.ref(symbol)` carries the class's *defining* prefix (e.g. the enclosing object for an
      // inner class), giving a stable outer reference that resolves even within a nested context-bound
      // implicit search — unlike `tpe.asType`, whose prefix can be the context-relative `this`.
      val typeTree = tpe.typeArgs match
        case Nil  => TypeTree.ref(symbol)
        case args => Applied(TypeTree.ref(symbol), args.map(arg => TypeTree.of(using arg.asType)))

      Select(New(typeTree), symbol.primaryConstructor).appliedToArgs(arguments)


  // Retypes the constructed plain product `Term` to `derivation`. When `derivation` is a carrier
  // refinement (`Product over Format` = `Product { type Transport = Format }`) the plain product is
  // a *supertype*, so a runtime `asInstanceOf` downcast is needed (sound: `Transport` is phantom).
  // When `derivation` is the plain product, `asExprOf` suffices and — crucially — avoids wrapping
  // the `New` in a cast node, which makes erasure crash with "missing outer accessor" for a class
  // nested in an object/method (the `New` carries an outer pointer the cast node mishandles).
  private def castConstructed[derivation: Type](using Quotes)(constructed: quotes.reflect.Term)
  :   Expr[derivation] =

    import quotes.reflect.*
    val derivationType = TypeRepr.of[derivation]

    if productType(derivationType) =:= derivationType then constructed.asExprOf[derivation]
    else
      TypeApply(Select.unique(constructed, "asInstanceOf"), List(TypeTree.of[derivation]))
      . asExprOf[derivation]


  // The declared name of the resolved variant/product — the term symbol for a singleton case
  // object or parameterless enum case, otherwise the type symbol.
  private def caseName(using Quotes)(repr: quotes.reflect.TypeRepr): String =
    val resolved = productType(repr)
    if resolved.termSymbol.isNoSymbol then resolved.typeSymbol.name else resolved.termSymbol.name

  // Applies the polymorphic-function `lambda` at type `field`, passing `value` as the value
  // argument and `contextArgs` as the trailing context-function arguments. The applications are
  // beta-reduced — substituting `field` and inlining the body with concrete types, exactly as the
  // inliner does for an `inline` lambda parameter — so no residual abstract `apply` call survives
  // (whose path-dependent `#Self` parameter types would otherwise fail to reduce).
  private def applyLambda[field: Type]
    ( using Quotes )
    ( lambda: quotes.reflect.Term, value: Expr[Any], contextArgs: List[Expr[Any]] )
  :   quotes.reflect.Term =

    import quotes.reflect.*

    def reduce(term: Term): Term = Term.betaReduce(term).getOrElse(term)

    val typeApplied = TypeApply(Select.unique(lambda, "apply"), List(TypeTree.of[field]))
    val applied = reduce(Apply(typeApplied, List(value.asTerm)))

    reduce(Apply(Select.unique(applied, "apply"), contextArgs.map(_.asTerm)))

  // Produces the tuple of field values (each = `lambda` applied to that field with its instance,
  // default, label and index). This macro is mirror-free and does NOT construct — `build` does the
  // construction *inline* via `reflection.fromProduct(…)` using the `Mirror` threaded from the
  // caller's `summonFrom`. Keeping construction inline (and out of macro output) is what lets a
  // method-local / object-nested case class derive: `fromProduct` captures the outer pointer from
  // the surrounding frame, which a macro-emitted `New` cannot reach (erasure "missing outer
  // accessor"); and because the macro never embeds the `Mirror`, it never escapes its scope.
  def buildProduct[typeclass[_]: Type, derivation <: Product: Type]
    ( lambda: Expr[Any] )
  :   Macro[Tuple] =

    import quotes.reflect.*

    val tpe = productType(TypeRepr.of[derivation])
    val symbol = tpe.typeSymbol
    val lambdaTerm = lambda.asTerm

    val arguments: List[Expr[Any]] = symbol.caseFields.zipWithIndex.map: (field, index) =>
      tpe.memberType(field).asType.absolve match
        case '[field] =>
          val indexExpr = Expr(index)
          val nameExpr = Expr(field.name)
          val default = '{Default(wisteria.internal.default[derivation, field]($indexExpr))}
          val label = '{$nameExpr.tt.aka["label"]}
          val fieldIndex = '{$indexExpr.asInstanceOf[Int & FieldIndex[field]].aka["index"]}

          sharedInstance[typeclass, field](index): (reference, contextual) =>
            applyLambda[field](lambdaTerm, reference, List(contextual, default, label, fieldIndex))

          . asExpr

    Expr.ofTupleFromSeq(arguments)

  def contextsProduct[typeclass[_]: Type, derivation: Type, result: Type]
    ( lambda: Expr[Any] )
  :   Macro[IArray[result]] =

    import quotes.reflect.*

    val tpe = productType(TypeRepr.of[derivation])
    val lambdaTerm = lambda.asTerm

    val results: List[Expr[result]] = tpe.typeSymbol.caseFields.zipWithIndex.map: (field, index) =>
      tpe.memberType(field).asType.absolve match
        case '[field] =>
          val indexExpr = Expr(index)
          val nameExpr = Expr(field.name)
          val default = '{Default(wisteria.internal.default[derivation, field]($indexExpr))}
          val label = '{$nameExpr.tt.aka["label"]}

          val accessor: Expr[derivation => field] =
            '{value => value.asInstanceOf[Product].productElement($indexExpr).asInstanceOf[field]}

          val dereference = '{$accessor.aka["dereference"]}
          val fieldIndex = '{$indexExpr.asInstanceOf[Int & FieldIndex[field]].aka["index"]}

          sharedInstance[typeclass, field](index): (reference, contextual) =>
            val arguments = List(contextual, default, label, dereference, fieldIndex)
            applyLambda[field](lambdaTerm, reference, arguments)

          . asExprOf[result]

    immutableArray[result](results)

  def fieldsProduct[typeclass[_]: Type, derivation: Type, result: Type]
    ( product: Expr[derivation], lambda: Expr[Any] )
  :   Macro[IArray[result]] =

    import quotes.reflect.*

    val tpe = productType(TypeRepr.of[derivation])
    val lambdaTerm = lambda.asTerm

    val results: List[Expr[result]] = tpe.typeSymbol.caseFields.zipWithIndex.map: (field, index) =>
      tpe.memberType(field).asType.absolve match
        case '[field] =>
          val indexExpr = Expr(index)
          val nameExpr = Expr(field.name)

          val fieldValue: Expr[Any] =
            '{$product.asInstanceOf[Product].productElement($indexExpr).asInstanceOf[field]}

          val contextual = '{${resolveField[typeclass, field]}.aka["contextual"]}
          val default = '{Default(wisteria.internal.default[derivation, field]($indexExpr))}
          val label = '{$nameExpr.tt.aka["label"]}
          val fieldIndex = '{$indexExpr.asInstanceOf[Int & FieldIndex[field]].aka["index"]}
          val arguments = List(contextual, default, label, fieldIndex)

          applyLambda[field](lambdaTerm, fieldValue, arguments).asExprOf[result]

    immutableArray[result](results)

  def constructMonadic[typeclass[_]: Type, constructor[_]: Type, derivation <: Product: Type]
    ( bind: Expr[Any], pure: Expr[Any], lambda: Expr[Any] )
  :   Macro[constructor[derivation]] =

    import quotes.reflect.*

    val tpe = productType(TypeRepr.of[derivation])
    val symbol = tpe.typeSymbol
    val bindTerm = bind.asTerm
    val pureTerm = pure.asTerm
    val lambdaTerm = lambda.asTerm
    val tupleType = TypeRepr.of[Tuple]
    val constructorTuple = TypeRepr.of[constructor[Tuple]]
    val derivationType = TypeRepr.of[derivation]

    // `pure.apply[monadic](value)`
    def applyPure(monadic: TypeRepr, value: Term): Term =
      Apply
        ( TypeApply(Select.unique(pureTerm, "apply"), List(TypeTree.of(using monadic.asType))),
          List(value) )

    // `bind.apply[input, output](monad)(function)` — the poly `apply` is curried, so the function
    // argument is applied to the returned `Function1` via its own `apply`.
    def applyBind(input: TypeRepr, output: TypeRepr, monad: Term, function: Term): Term =
      val typed =
        TypeApply
          ( Select.unique(bindTerm, "apply"),
            List(TypeTree.of(using input.asType), TypeTree.of(using output.asType)) )

      Apply(Select.unique(Apply(typed, List(monad)), "apply"), List(function))

    // Accumulate each field monadically into a (reversed) tuple, threading through `bind`:
    //   bind(acc) { tuple => bind(lambda[field](ctx)) { value => pure(value *: tuple) } }
    var accumulator: Term = applyPure(tupleType, '{EmptyTuple}.asTerm)

    val tupleFunction = MethodType(List("tuple"))(_ => List(tupleType), _ => constructorTuple)

    symbol.caseFields.zipWithIndex.foreach: (field, index) =>
      tpe.memberType(field).asType.absolve match
        case '[field] =>
          val indexExpr = Expr(index)
          val nameExpr = Expr(field.name)
          val fieldType = TypeRepr.of[field]
          val valueFunction = MethodType(List("value"))(_ => List(fieldType), _ => constructorTuple)

          val outer = Lambda(Symbol.spliceOwner, tupleFunction, { (owner, parameters) =>
            val tupleRef = parameters.head.asInstanceOf[Term]
            val contextValue: Expr[Any] = resolveField[typeclass, field]
            val contextual = '{${resolveField[typeclass, field]}.aka["contextual"]}
            val default = '{Default(wisteria.internal.default[derivation, field]($indexExpr))}
            val label = '{$nameExpr.tt.aka["label"]}
            val fieldIndex = '{$indexExpr.asInstanceOf[Int & FieldIndex[field]].aka["index"]}
            val arguments = List(contextual, default, label, fieldIndex)
            val monad = applyLambda[field](lambdaTerm, contextValue, arguments)

            val inner = Lambda(owner, valueFunction, { (innerOwner, innerParameters) =>
              val valueRef = innerParameters.head.asInstanceOf[Term]
              val prepended = '{${valueRef.asExprOf[field]} *: ${tupleRef.asExprOf[Tuple]}}.asTerm
              applyPure(tupleType, prepended).changeOwner(innerOwner)
            })

            applyBind(fieldType, tupleType, monad, inner).changeOwner(owner)
          })

          accumulator = applyBind(tupleType, tupleType, accumulator, outer)

    // Finally reverse the accumulated tuple and construct the product from its elements.
    val derivationFunction =
      MethodType(List("tuple"))(_ => List(tupleType), _ => TypeRepr.of[constructor[derivation]])

    val product = productType(derivationType)

    val finalize = Lambda(Symbol.spliceOwner, derivationFunction, { (finalOwner, parameters) =>
      val reversed = '{${parameters.head.asInstanceOf[Term].asExprOf[Tuple]}.reverse}

      // Extract each field from the (reversed) accumulated tuple and pass it to the constructor.
      val arguments = product.typeSymbol.caseFields.zipWithIndex.map: (field, index) =>
        product.memberType(field).asType.absolve match
          case '[field] => '{$reversed.productElement(${Expr(index)}).asInstanceOf[field]}.asTerm

      // Cast to `derivation` (possibly a carrier refinement) — see `castConstructed`.
      val constructed = castConstructed[derivation](constructProduct[derivation](arguments)).asTerm
      applyPure(derivationType, constructed).changeOwner(finalOwner)
    })

    applyBind(tupleType, derivationType, accumulator, finalize).asExprOf[constructor[derivation]]

  def typeName[derivation: Type]: Macro[Text] =
    import quotes.reflect.*
    val name: String = caseName(TypeRepr.of[derivation])
    '{${Expr(name)}.tt}

  def variantLabels[derivation: Type]: Macro[List[Text]] =
    import quotes.reflect.*

    val labels = TypeRepr.of[derivation].typeSymbol.children.map: child =>
      '{${Expr(child.name)}.tt}

    Expr.ofList(labels)

  // Resolves a field's typeclass instance via an inline `summonInline` at the use site. Deferring
  // resolution to the splice site (rather than `Expr.summon` here) is what lets a recursive or
  // repeated field find a sibling synthetic given generated alongside it, instead of re-deriving.
  private def resolveField[typeclass[_]: Type, field: Type](using Quotes): Expr[typeclass[field]] =
    '{wisteria.internal.field[typeclass, field]}

  // Binds a field/variant's resolved instance to a single val, so the lambda's value argument and
  // its `"contextual"` context argument share one instance instead of resolving it twice. Without
  // that sharing each use re-emits — and, absent dedup, re-derives — the whole sub-instance,
  // doubling the generated code (and anonymous classes) for every decoder- or `contexts`-style
  // derivation. `build(ref, contextual)` produces the applied term given the shared ref and its
  // `"contextual"`-tagged view; the result is wrapped in a block that introduces the val.
  private def sharedInstance[typeclass[_]: Type, field: Type](using Quotes)
    ( index: Int )
    ( build: (Expr[typeclass[field]], Expr[Any]) => quotes.reflect.Term )
  :   quotes.reflect.Term =

    import quotes.reflect.*

    val symbol =
      Symbol.newVal
        ( Symbol.spliceOwner,
          "wisteria$field$"+index,
          TypeRepr.of[typeclass[field]],
          Flags.EmptyFlags,
          Symbol.noSymbol )

    val reference = Ref(symbol).asExprOf[typeclass[field]]
    val applied = build(reference, '{$reference.aka["contextual"]})

    Block(List(ValDef(symbol, Some(resolveField[typeclass, field].asTerm))), applied)

  def getDefault[product: Type, field: Type](index: Expr[Int]): Macro[Optional[field]] =
    import quotes.reflect.*

    val methodName: String = "$lessinit$greater$default$"+(index.valueOrAbort + 1)
    val productSymbol = TypeRepr.of[product].classSymbol

    productSymbol.flatMap: symbol =>
      symbol.companionClass.declaredMethod(methodName).headOption.map: method =>
        Ref(symbol.companionModule).select(method)

    . map: selection =>
        TypeRepr.of[product].typeArgs match
          case Nil       => selection
          case arguments => selection.appliedToTypes(arguments)

    . map(_.asExprOf[field]).getOrElse('{Unset})
