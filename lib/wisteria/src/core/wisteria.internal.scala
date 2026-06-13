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
    ( lambda: Expr[Any], requirement: Expr[ContextRequirement],
      reflection: Expr[ProductReflection[derivation]] )
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
          val contextValue: Expr[Any] = resolveField[typeclass, field](requirement)
          val contextual = '{${resolveField[typeclass, field](requirement)}.aka["contextual"]}
          val default = '{Default(wisteria.internal.default[derivation, field]($indexExpr))}
          val label = '{$nameExpr.tt.aka["label"]}
          val fieldIndex = '{$indexExpr.asInstanceOf[Int & FieldIndex[field]].aka["index"]}

          applyLambda[field](lambdaTerm, contextValue, List(contextual, default, label, fieldIndex))
          . asExpr

    // Construct through the `Mirror`'s `fromProduct`, which correctly handles inner-class outer
    // pointers and refined (`… over Json` carrier) derivation types — the only use of the Mirror;
    // fields, labels and types all come from symbol inspection.
    val tuple = Expr.ofTupleFromSeq(arguments)
    '{$reflection.fromProduct($tuple)}

  def contextsProduct[typeclass[_]: Type, derivation: Type, result: Type]
    ( lambda: Expr[Any], requirement: Expr[ContextRequirement] )
  :   Macro[IArray[result]] =

    import quotes.reflect.*

    val tpe = productType(TypeRepr.of[derivation])
    val lambdaTerm = lambda.asTerm

    val results: List[Expr[result]] = tpe.typeSymbol.caseFields.zipWithIndex.map: (field, index) =>
      tpe.memberType(field).asType.absolve match
        case '[field] =>
          val indexExpr = Expr(index)
          val nameExpr = Expr(field.name)

          val contextValue: Expr[Any] = resolveField[typeclass, field](requirement)
          val contextual = '{${resolveField[typeclass, field](requirement)}.aka["contextual"]}
          val default = '{Default(wisteria.internal.default[derivation, field]($indexExpr))}
          val label = '{$nameExpr.tt.aka["label"]}

          val accessor: Expr[derivation => field] =
            '{value => value.asInstanceOf[Product].productElement($indexExpr).asInstanceOf[field]}

          val dereference = '{$accessor.aka["dereference"]}
          val fieldIndex = '{$indexExpr.asInstanceOf[Int & FieldIndex[field]].aka["index"]}
          val arguments = List(contextual, default, label, dereference, fieldIndex)

          applyLambda[field](lambdaTerm, contextValue, arguments).asExprOf[result]

    immutableArray[result](results)

  def fieldsProduct[typeclass[_]: Type, derivation: Type, result: Type]
    ( product: Expr[derivation], lambda: Expr[Any], requirement: Expr[ContextRequirement] )
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

          val contextual = '{${resolveField[typeclass, field](requirement)}.aka["contextual"]}
          val default = '{Default(wisteria.internal.default[derivation, field]($indexExpr))}
          val label = '{$nameExpr.tt.aka["label"]}
          val fieldIndex = '{$indexExpr.asInstanceOf[Int & FieldIndex[field]].aka["index"]}
          val arguments = List(contextual, default, label, fieldIndex)

          applyLambda[field](lambdaTerm, fieldValue, arguments).asExprOf[result]

    immutableArray[result](results)

  def constructMonadic[typeclass[_]: Type, constructor[_]: Type, derivation <: Product: Type]
    ( bind: Expr[Any], pure: Expr[Any], lambda: Expr[Any], requirement: Expr[ContextRequirement],
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
            val contextValue: Expr[Any] = resolveField[typeclass, field](requirement)
            val contextual = '{${resolveField[typeclass, field](requirement)}.aka["contextual"]}
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

    val finalize = Lambda(Symbol.spliceOwner, derivationFunction, { (finalOwner, parameters) =>
      val reversed = '{${parameters.head.asInstanceOf[Term].asExprOf[Tuple]}.reverse}
      val constructed = '{$reflection.fromProduct($reversed)}.asTerm
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

  // Resolves a field's typeclass instance, wrapped per the `ContextRequirement`, deferring to the
  // inline `summon` at the use site.
  private def resolveField[typeclass[_]: Type, field: Type]
    (requirement: Expr[ContextRequirement])(using Quotes)
  :   Expr[Any] =

    '{$requirement.summon[typeclass[field]]}

  def variantDispatch[typeclass[_]: Type, derivation: Type, result: Type]
    ( sum: Expr[derivation], lambda: Expr[Any], requirement: Expr[ContextRequirement] )
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
            val contextual = '{${resolveField[typeclass, variant](requirement)}.aka["contextual"]}
            val label = '{${Expr(child.name)}.tt.aka["label"]}
            val variantIndex = '{VariantIndex[variant]($indexExpr).aka["index"]}
            val arguments = List(contextual, label, variantIndex)

            applyLambda[variant](lambdaTerm, variantValue, arguments).asExprOf[result]

    ordinalMatch[result](ordinalOf[derivation](sum), branches, '{throw new MatchError($sum)})

  def delegateDispatch[typeclass[_]: Type, derivation: Type, result: Type]
    ( delegation: Expr[Text], lambda: Expr[Any], requirement: Expr[ContextRequirement] )
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
            val contextValue: Expr[Any] = resolveField[typeclass, variant](requirement)
            val contextual = '{${resolveField[typeclass, variant](requirement)}.aka["contextual"]}
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
