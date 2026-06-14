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
import contingency.*
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

  def fieldLabels[derivation: Type]: Macro[List[Text]] =
    import quotes.reflect.*

    val labels =
      productType(TypeRepr.of[derivation]).typeSymbol.caseFields.map: field =>
        '{${Expr(field.name)}.tt}

    Expr.ofList(labels)

  // Erased witnesses for the `Reflection` markers — never inspected; the right runtime subtype is
  // produced so `derivedOne`'s `match` dispatches product vs sum, and the structural detail comes
  // from symbol inspection. `Nothing <: Product`, so it inhabits `ProductReflection`'s bound.
  val productWitness: ProductReflection[Nothing] = new ProductReflection[Nothing] {}
  val sumWitness: SumReflection[Any] = new SumReflection[Any] {}

  def isSumType(using Quotes)(tpe: quotes.reflect.TypeRepr): Boolean =
    import quotes.reflect.*

    tpe.dealias match
      // A `Variant & Parent` intersection (a sum's variant) is derived as the variant — a product.
      case AndType(_, _)          => false
      case Refinement(base, _, _) => isSumType(base)
      // A singleton (case object / parameterless enum case) has a *term* symbol; its *type* symbol
      // widens to the sum, so the term test must come first — it is a product, not a sum.
      case other =>
        other.termSymbol.isNoSymbol && other.typeSymbol.children.nonEmpty

  def isProductType(using Quotes)(tpe: quotes.reflect.TypeRepr): Boolean =
    import quotes.reflect.*

    !isSumType(tpe)
    && (tpe <:< TypeRepr.of[Product] || tpe <:< TypeRepr.of[scala.Singleton]
        || tpe.typeSymbol.caseFields.nonEmpty)

  def reflectData[derivation: Type]: Macro[Reflection[derivation]] =
    import quotes.reflect.*
    val tpe = TypeRepr.of[derivation]

    if isSumType(tpe) then '{wisteria.internal.sumWitness.asInstanceOf[Reflection[derivation]]}
    else if isProductType(tpe)
    then '{wisteria.internal.productWitness.asInstanceOf[Reflection[derivation]]}
    else report.errorAndAbort("wisteria: "+tpe.show+" is not a product or sum type")

  def reflectProduct[derivation <: Product: Type]: Macro[ProductReflection[derivation]] =
    import quotes.reflect.*
    val tpe = TypeRepr.of[derivation]

    if isProductType(tpe)
    then '{wisteria.internal.productWitness.asInstanceOf[ProductReflection[derivation]]}
    else report.errorAndAbort("wisteria: "+tpe.show+" is not a product type")

  inline def isSum[derivation]: Boolean = ${isSumMacro[derivation]}

  def isSumMacro[derivation: Type]: Macro[Boolean] =
    import quotes.reflect.*
    Expr(isSumType(TypeRepr.of[derivation]))

  def reflectSum[derivation: Type]: Macro[SumReflection[derivation]] =
    import quotes.reflect.*
    val tpe = TypeRepr.of[derivation]

    if isSumType(tpe) then '{wisteria.internal.sumWitness.asInstanceOf[SumReflection[derivation]]}
    else report.errorAndAbort("wisteria: "+tpe.show+" is not a sum type")

  // Resolves a field/variant instance while *ignoring the typeclass's wrapper givens* (which route
  // data types into `derived`). This keeps a field summon — and the nested element summons of any
  // codec it resolves — from re-entering the in-progress derivation, so each lands on a sibling
  // (a recursive/shared type) or a codec (`List[T]`, …) or a leaf instead.
  inline def field[typeclass[_], elem]: typeclass[elem] = ${fieldInstance[typeclass, elem]}

  // A "wrapper" given supplies `typeclass[T]` at a bare type parameter `T` — this derivation's own
  // `derived`/`derivedOne`, or a `summonFrom` catch-all routing data types into the derivation —
  // unlike a structural codec like `List[T] is Encodable` whose argument is an applied type.
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

  // Resolves a field/variant instance via the real implicit search, ignoring the typeclass's
  // wrapper givens so it lands on a sibling synthetic given (a recursive/shared type), a codec
  // (`List[T]`, …), or a leaf — never re-entering the in-progress derivation. Deferred to the use
  // site (the sibling val's nested `Quotes`) so the siblings generated alongside it are in scope.
  def fieldInstance[typeclass[_]: Type, elem: Type]: Macro[typeclass[elem]] =
    import quotes.reflect.*

    val typeclassConstructor = TypeRepr.of[typeclass]
    val typeclassOwner = typeclassConstructor.appliedTo(TypeRepr.of[Any]).typeSymbol.maybeOwner

    // The typeclass's wrapper givens, ignored so neither this summon nor any nested element summon
    // of a codec it resolves re-enters the derivation.
    val wrappers =
      if typeclassOwner.isNoSymbol then Nil
      else (typeclassOwner.methodMembers ++ typeclassOwner.fieldMembers)
        . filter(isWrapper(typeclassConstructor, _))
        . distinct

    val instance = TypeRepr.of[typeclass[elem]]

    // Prefer a resolution that avoids the wrappers — a codec, a sibling, or a concrete leaf — so
    // the derivation isn't re-entered. Only if none exists is `elem` something the wrapper handles
    // specially (e.g. `Json` itself, or a text-codec leaf), so fall back to the ordinary search.
    val result = Implicits.searchIgnoring(instance)(wrappers*) match
      case success: ImplicitSearchSuccess => success
      case _                              => Implicits.search(instance)

    result.absolve match
      case success: ImplicitSearchSuccess => success.tree.asExprOf[typeclass[elem]]
      case failure: ImplicitSearchFailure => report.errorAndAbort(failure.explanation)

  private def derivedOneInstance(using nested: Quotes)(self: Expr[Any], elem: Type[?]): Expr[Any] =
    import nested.reflect.*
    val selfTerm = self.asTerm
    val method = selfTerm.tpe.typeSymbol.methodMember("derivedOne").head

    Select(selfTerm, method).appliedToType(TypeRepr.of(using elem)).asExpr

  // Derives `typeclass[derivation]` by emitting a block of mutually-recursive synthetic
  // `given lazy val`s — one per distinct product/sum type reachable from `derivation` that a
  // structural derivation would otherwise build. Each instance is built once via `self.derivedOne`,
  // its fields resolving through `field` (a real `searchIgnoring`), which lands on a sibling given,
  // a codec, or a leaf. Sharing both deduplicates repeated subtrees (so a large graph stays within
  // JVM class limits) and ties recursive types safely (a lazy val may reference itself by name).
  //
  // Reachability is discovered by the real implicit search, never by reimplementing it: for each
  // type we ask the compiler (a) does a non-wrapper instance already exist (`resolves`), and (b) if
  // not, is it a codec whose only gap is its element instances (`isCodec`, via a context-function
  // search). Only types that are neither become structural siblings.
  def deriveGraph[typeclass[_]: Type, derivation: Type](self: Expr[Any])
  :   Macro[typeclass[derivation]] =

    import quotes.reflect.*

    val selfSymbol = self.asTerm.tpe.typeSymbol

    if selfSymbol.methodMember("derivedOne").isEmpty then
      report.errorAndAbort("wisteria: the derivation defines no `derivedOne`")

    val typeclassConstructor = TypeRepr.of[typeclass]

    // The typeclass's wrapper givens (its `derived`/`derivedOne` and any `summonFrom` catch-all
    // routing data types into the derivation). Ignored during probing so the search reports a real
    // codec/leaf — or fails — instead of looping back into this derivation.
    val typeclassOwner = typeclassConstructor.appliedTo(TypeRepr.of[Any]).typeSymbol.maybeOwner

    val wrappers =
      if typeclassOwner.isNoSymbol then Nil
      else (typeclassOwner.methodMembers ++ typeclassOwner.fieldMembers)
        . filter(isWrapper(typeclassConstructor, _))
        . distinct

    def instanceOf(tpe: TypeRepr): TypeRepr = typeclassConstructor.appliedTo(tpe)

    // A real implicit search for `typeclass[tpe]`, ignoring the typeclass's wrapper givens.
    def resolves(tpe: TypeRepr): Boolean =
      Implicits.searchIgnoring(instanceOf(tpe))(wrappers*) match
        case _: ImplicitSearchSuccess => true
        case _                        => false

    // The context-function type `(typeclass[arg]…) ?=> typeclass[tpe]` — used to ask the compiler
    // whether `tpe` is a codec (`List[T]`, `Option[T]`, …) whose only unmet need is its elements.
    def codecFunction(tpe: TypeRepr, args: List[TypeRepr]): TypeRepr =
      defn.FunctionClass(args.length, isContextual = true).typeRef
        . appliedTo(args.map(instanceOf) :+ instanceOf(tpe))

    // Whether `tpe` is such a codec: the context function resolves assuming the element instances.
    // The result is matched *directly* (never stored in an `Optional`): the type test for the
    // path-dependent `ImplicitSearchSuccess` only holds against the raw search result. Real search,
    // no unification.
    def isCodec(tpe: TypeRepr, args: List[TypeRepr]): Boolean =
      args.nonEmpty
      && (Implicits.searchIgnoring(codecFunction(tpe, args))(wrappers*) match
            case _: ImplicitSearchSuccess => true
            case _                        => false)

    // The macro-derived types reachable from `derivation` — those a structural derivation builds
    // itself. A type provided by a codec/leaf is not a sibling, but a codec's element types are
    // followed (the `Role` inside `List[Role]` is still macro-derived and must become a sibling).
    val reachable = scala.collection.mutable.LinkedHashMap[String, TypeRepr]()
    val seen = scala.collection.mutable.HashSet[String]()

    // The root is always derived structurally when it is an ADT — never probed with `resolves`,
    // which would find its own `derives TC` companion given (`T.derived$TC: TC[T]`) and make the
    // block reference itself, a lazy-val cycle. Only a codec root (a collection requested through
    // `derived`) is resolved. Non-root types are probed normally so they share existing instances.
    def visit(raw: TypeRepr, isRoot: Boolean): Unit =
      val tpe = raw.dealias
      val key = tpe.show

      if !seen.contains(key) then
        seen += key
        val args = tpe.typeArgs

        if !isRoot && resolves(tpe) then () // a leaf, a companion given, or a supplied codec
        else if isCodec(tpe, args) then args.foreach(visit(_, false))
        else if isSumType(tpe) then
          reachable(key) = tpe

          tpe.typeSymbol.children.foreach: child =>
            visit(variantWith(child, tpe), false)
        else if isProductType(tpe) then
          reachable(key) = tpe
          val product = productType(tpe)

          product.typeSymbol.caseFields.foreach: field =>
            visit(product.memberType(field), false)
        // else: no instance and not derivable — `field` reports the error at the use site.

    val rootType = TypeRepr.of[derivation].dealias

    visit(rootType, isRoot = true)

    val owner = Symbol.spliceOwner

    val bindings: List[(String, TypeRepr, Symbol)] =
      reachable.toList.zipWithIndex.map: (entry, index) =>
        val (key, tpe) = entry
        // Build the instance type from the stable `TypeRepr` so the synthetic val references no
        // local type that would be out of scope once the block is assembled.
        val instance = instanceOf(tpe)
        val flags = Flags.Given | Flags.Lazy
        val symbol = Symbol.newVal(owner, "wisteria$"+index, instance, flags, Symbol.noSymbol)

        (key, tpe, symbol)

    val symbolByKey = bindings.map { (key, _, symbol) => key -> symbol }.toMap

    // Each body is built in the val's *own* nested `Quotes` (`symbol.asQuotes`), so the `field`
    // resolutions inside `derivedOne` resolve in that val's scope — where the sibling givens are
    // visible — instead of in the outer macro scope.
    val definitions: List[ValDef] = bindings.map: (_, tpe, symbol) =>
      val body = derivedOneInstance(using symbol.asQuotes)(self, tpe.asType).asTerm

      ValDef(symbol, Some(body))

    // The instance for an element of a root codec: the sibling given if it was derived
    // structurally, otherwise a direct resolution (a leaf, or a nested codec complete in scope).
    def elementInstance(tpe: TypeRepr): Term = symbolByKey.get(tpe.dealias.show) match
      case Some(symbol) => Ref(symbol)
      case None         => resolveDirect(tpe)

    def resolveDirect(tpe: TypeRepr): Term =
      Implicits.searchIgnoring(instanceOf(tpe))(wrappers*).absolve match
        case success: ImplicitSearchSuccess => success.tree
        case failure: ImplicitSearchFailure => report.errorAndAbort(failure.explanation)

    // The root expression: a reference to the root's sibling, or — when the root is itself a codec
    // (a collection type requested through `derived`) — the codec function applied to its elements.
    val rootArgs = rootType.typeArgs

    val root: Term =
      symbolByKey.get(rootType.show) match
        case Some(symbol) => Ref(symbol)

        case None =>
          if !isCodec(rootType, rootArgs) then resolveDirect(rootType)
          else Implicits.searchIgnoring(codecFunction(rootType, rootArgs))(wrappers*).absolve.match
            case success: ImplicitSearchSuccess =>
              Select.unique(success.tree, "apply").appliedToArgs(rootArgs.map(elementInstance))

            case failure: ImplicitSearchFailure =>
              report.errorAndAbort(failure.explanation)

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

  // Construct a product value via its primary constructor — `new T(arg0, …)` — from the
  // field-value terms, in declaration order. Replaces `Mirror.fromProduct`: pure symbol inspection.
  private def constructProduct[derivation: Type](using Quotes)
    ( arguments: List[quotes.reflect.Term] )
  :   quotes.reflect.Term =

    import quotes.reflect.*
    val tpe = productType(TypeRepr.of[derivation])

    // A singleton (case object / parameterless enum case) is its own value — reference the module,
    // don't `new` it. Otherwise call the primary constructor with the field values.
    if !tpe.termSymbol.isNoSymbol then Ref(tpe.termSymbol)
    else Select(New(TypeTree.of(using tpe.asType)), tpe.typeSymbol.primaryConstructor)
      . appliedToArgs(arguments)

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

  def buildProduct[typeclass[_]: Type, derivation <: Product: Type]
    ( lambda: Expr[Any], reflection: Expr[ProductReflection[derivation]] )
  :   Macro[derivation] =

    import quotes.reflect.*

    val tpe = productType(TypeRepr.of[derivation])
    val symbol = tpe.typeSymbol
    val lambdaTerm = lambda.asTerm

    val arguments: List[Expr[Any]] = symbol.caseFields.zipWithIndex.map: (field, index) =>
      tpe.memberType(field).asType.absolve match
        case '[field] =>
          val indexExpr = Expr(index)
          val nameExpr = Expr(field.name)

          // `requirement.summon[typeclass[field]]` — built twice (idempotent): once as the
          // polymorphic-function value argument, once tagged as the `"contextual"` context arg.
          val contextValue: Expr[Any] = resolveField[typeclass, field]
          val contextual = '{${resolveField[typeclass, field]}.aka["contextual"]}
          val default = '{Default(wisteria.internal.default[derivation, field]($indexExpr))}
          val label = '{$nameExpr.tt.aka["label"]}
          val fieldIndex = '{$indexExpr.asInstanceOf[Int & FieldIndex[field]].aka["index"]}

          applyLambda[field](lambdaTerm, contextValue, List(contextual, default, label, fieldIndex))
          . asExpr

    // Construct via the primary constructor — symbol inspection only, no `Mirror`.
    constructProduct[derivation](arguments.map(_.asTerm)).asExprOf[derivation]

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

          val contextValue: Expr[Any] = resolveField[typeclass, field]
          val contextual = '{${resolveField[typeclass, field]}.aka["contextual"]}
          val default = '{Default(wisteria.internal.default[derivation, field]($indexExpr))}
          val label = '{$nameExpr.tt.aka["label"]}

          val accessor: Expr[derivation => field] =
            '{value => value.asInstanceOf[Product].productElement($indexExpr).asInstanceOf[field]}

          val dereference = '{$accessor.aka["dereference"]}
          val fieldIndex = '{$indexExpr.asInstanceOf[Int & FieldIndex[field]].aka["index"]}
          val arguments = List(contextual, default, label, dereference, fieldIndex)

          applyLambda[field](lambdaTerm, contextValue, arguments).asExprOf[result]

    immutableArray[result](results)

  // The sum analogue of `contextsProduct`: a value-less fold over the variants. For each variant it
  // resolves the variant's typeclass instance (the lambda's value argument and its `"contextual"`
  // context arg), passes the label and index, and collects the results into an `IArray`.
  def choicesSum[typeclass[_]: Type, derivation: Type, result: Type]
    ( lambda: Expr[Any] )
  :   Macro[IArray[result]] =

    import quotes.reflect.*

    val lambdaTerm = lambda.asTerm
    val derivationType = TypeRepr.of[derivation]

    val results: List[Expr[result]] =
      derivationType.typeSymbol.children.zipWithIndex.map: (child, index) =>
        variantWith(child, derivationType).asType.absolve match
          case '[variant] =>
            val indexExpr = Expr(index)
            val contextValue: Expr[Any] = resolveField[typeclass, variant]
            val contextual = '{${resolveField[typeclass, variant]}.aka["contextual"]}
            val label = '{${Expr(child.name)}.tt.aka["label"]}
            val variantIndex = '{VariantIndex[variant]($indexExpr).aka["index"]}
            val arguments = List(contextual, label, variantIndex)

            applyLambda[variant](lambdaTerm, contextValue, arguments).asExprOf[result]

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
    ( bind: Expr[Any], pure: Expr[Any], lambda: Expr[Any],
      reflection: Expr[ProductReflection[derivation]] )
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

      val constructed = constructProduct[derivation](arguments)
      applyPure(derivationType, constructed).changeOwner(finalOwner)
    })

    applyBind(tupleType, derivationType, accumulator, finalize).asExprOf[constructor[derivation]]

  def isTuple[derivation: Type]: Macro[Boolean] =
    import quotes.reflect.*
    Expr(productType(TypeRepr.of[derivation]) <:< TypeRepr.of[Tuple])

  def isSingleton[derivation: Type]: Macro[Boolean] =
    import quotes.reflect.*
    Expr(productType(TypeRepr.of[derivation]) <:< TypeRepr.of[scala.Singleton])

  def typeName[derivation: Type]: Macro[Text] =
    import quotes.reflect.*
    val name: String = caseName(TypeRepr.of[derivation])
    '{${Expr(name)}.tt}

  // ---- sum derivation ----

  // The type of a variant: a `TypeRef` for a case-class variant, a `TermRef` (singleton) for a
  // case object or parameterless enum case.
  private def variantType(using Quotes)(child: quotes.reflect.Symbol): quotes.reflect.TypeRepr =
    if child.isType then child.typeRef else child.termRef

  // The variant type intersected with the sum parent (`variant & derivation`). The intersection
  // is what keeps a parameterless enum case distinct: its bare type widens to the sum, but the
  // intersection both stays distinct and conforms to the lambda's `variant <: derivation` bound.
  private def variantWith(using Quotes)
    ( child: quotes.reflect.Symbol, parent: quotes.reflect.TypeRepr )
  :   quotes.reflect.TypeRepr =

    import quotes.reflect.*
    AndType(variantType(child), parent)

  def variantLabels[derivation: Type]: Macro[List[Text]] =
    import quotes.reflect.*

    val labels = TypeRepr.of[derivation].typeSymbol.children.map: child =>
      '{${Expr(child.name)}.tt}

    Expr.ofList(labels)

  def isChoice[derivation: Type]: Macro[Boolean] =
    import quotes.reflect.*
    val children = TypeRepr.of[derivation].typeSymbol.children

    val singleton = children.forall: child =>
      variantType(child) <:< TypeRepr.of[scala.Singleton]

    Expr(children.nonEmpty && singleton)

  // The ordinal of the active variant — `Enum#ordinal` for enums, otherwise a type-test match in
  // declaration order, matching the order of `children`.
  private def ordinalOf[derivation: Type](sum: Expr[derivation])(using Quotes): Expr[Int] =
    import quotes.reflect.*
    val symbol = TypeRepr.of[derivation].typeSymbol

    if symbol.flags.is(Flags.Enum) then '{$sum.asInstanceOf[scala.reflect.Enum].ordinal} else
      val cases = symbol.children.zipWithIndex.map: (child, index) =>
        val pattern = Typed(Wildcard(), TypeTree.of(using variantType(child).asType))
        CaseDef(pattern, None, Literal(IntConstant(index)))

      Match(sum.asTerm, cases).asExprOf[Int]

  // Dispatches on the ordinal, applying `lambda` to the active variant.
  private def ordinalMatch[result: Type]
    ( ordinal: Expr[Int], branches: List[Expr[result]], default: Expr[result] )
    ( using Quotes )
  :   Expr[result] =

    import quotes.reflect.*

    val cases = branches.zipWithIndex.map: (branch, index) =>
      CaseDef(Literal(IntConstant(index)), None, branch.asTerm)

    Match(ordinal.asTerm, cases :+ CaseDef(Wildcard(), None, default.asTerm)).asExprOf[result]

  // Resolves a field's typeclass instance via an inline `summonInline` at the use site. Deferring
  // resolution to the splice site (rather than `Expr.summon` here) is what lets a recursive or
  // repeated field find a sibling synthetic given generated alongside it, instead of re-deriving.
  private def resolveField[typeclass[_]: Type, field: Type](using Quotes): Expr[Any] =
    '{wisteria.internal.field[typeclass, field]}

  def variantDispatch[typeclass[_]: Type, derivation: Type, result: Type]
    ( sum: Expr[derivation], lambda: Expr[Any] )
  :   Macro[result] =

    import quotes.reflect.*

    val lambdaTerm = lambda.asTerm

    val derivationType = TypeRepr.of[derivation]

    val branches: List[Expr[result]] =
      derivationType.typeSymbol.children.zipWithIndex.map: (child, index) =>
        variantWith(child, derivationType).asType.absolve match
          case '[variant] =>
            val indexExpr = Expr(index)
            val variantValue: Expr[Any] = '{$sum.asInstanceOf[variant]}
            val contextual = '{${resolveField[typeclass, variant]}.aka["contextual"]}
            val label = '{${Expr(child.name)}.tt.aka["label"]}
            val variantIndex = '{VariantIndex[variant]($indexExpr).aka["index"]}
            val arguments = List(contextual, label, variantIndex)

            applyLambda[variant](lambdaTerm, variantValue, arguments).asExprOf[result]

    ordinalMatch[result](ordinalOf[derivation](sum), branches, '{throw new MatchError($sum)})

  def delegateDispatch[typeclass[_]: Type, derivation: Type, result: Type]
    ( delegation: Expr[Text], lambda: Expr[Any] )
  :   Macro[result] =

    import quotes.reflect.*

    val lambdaTerm = lambda.asTerm

    val default: Expr[result] =
      '{provide[Tactic[VariantError]](abort(VariantError[derivation]($delegation)))}

    val derivationType = TypeRepr.of[derivation]

    derivationType.typeSymbol.children.zipWithIndex.foldRight(default):
      case ((child, index), rest) =>
        variantWith(child, derivationType).asType.absolve match
          case '[variant] =>
            val indexExpr = Expr(index)
            val nameExpr = Expr(child.name)
            val contextValue: Expr[Any] = resolveField[typeclass, variant]
            val contextual = '{${resolveField[typeclass, variant]}.aka["contextual"]}
            val label = '{$nameExpr.tt.aka["label"]}
            val variantIndex = '{VariantIndex[variant]($indexExpr).aka["index"]}
            val arguments = List(contextual, label, variantIndex)
            val branch = applyLambda[variant](lambdaTerm, contextValue, arguments).asExprOf[result]

            '{if $delegation == $nameExpr.tt then $branch else $rest}

  def complementVariant[derivation: Type, variant: Type]
    ( sum: Expr[derivation], variantIndex: Expr[Any] )
  :   Macro[Optional[variant]] =

    '{  if ${ordinalOf[derivation](sum)} == $variantIndex.asInstanceOf[Int]
        then $sum.asInstanceOf[variant]: Optional[variant]
        else Unset  }

  def singletonByLabel[derivation: Type](input: Expr[Text]): Macro[derivation] =
    import quotes.reflect.*

    val default: Expr[derivation] =
      '{provide[Tactic[VariantError]](abort(VariantError[derivation]($input)))}

    TypeRepr.of[derivation].typeSymbol.children.foldRight(default): (child, rest) =>
      val reference = Ref(child).asExprOf[derivation]
      '{if $input == ${Expr(child.name)}.tt then $reference else $rest}

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
