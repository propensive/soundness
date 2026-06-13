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
      case AndType(left, right) => if parent(left) then productType(right) else productType(left)
      case other                => other

  // The declared name of the resolved variant/product — the term symbol for a singleton case
  // object or parameterless enum case, otherwise the type symbol.
  private def caseName(using Quotes)(repr: quotes.reflect.TypeRepr): String =
    val resolved = productType(repr)
    if resolved.termSymbol.isNoSymbol then resolved.typeSymbol.name else resolved.termSymbol.name

  // Applies the polymorphic-function `lambda` at type `field`, passing `value` as the value
  // argument and `contextArgs` as the trailing context-function arguments.
  private def applyLambda[field: Type]
    ( using Quotes )
    ( lambda: quotes.reflect.Term, value: Expr[Any], contextArgs: List[Expr[Any]] )
  :   quotes.reflect.Term =

    import quotes.reflect.*

    val applied =
      Apply
        ( TypeApply(Select.unique(lambda, "apply"), List(TypeTree.of[field])),
          List(value.asTerm) )

    Apply(Select.unique(applied, "apply"), contextArgs.map(_.asTerm))

  def buildProduct[typeclass[_]: Type, derivation: Type]
    ( lambda: Expr[Any], requirement: Expr[ContextRequirement] )
  :   Macro[derivation] =

    import quotes.reflect.*

    val tpe = productType(TypeRepr.of[derivation])
    val symbol = tpe.typeSymbol
    val lambdaTerm = lambda.asTerm

    val arguments: List[Term] = symbol.caseFields.zipWithIndex.map: (field, index) =>
      tpe.memberType(field).asType.absolve match
        case '[field] =>
          val indexExpr = Expr(index)
          val nameExpr = Expr(field.name)

          // `requirement.summon[typeclass[field]]` — built twice (idempotent): once as the
          // polymorphic-function value argument, once tagged as the `"contextual"` context arg.
          val contextValue: Expr[Any] = '{$requirement.summon[typeclass[field]]}
          val contextual = '{$requirement.summon[typeclass[field]].aka["contextual"]}
          val default = '{Default(wisteria.internal.default[derivation, field]($indexExpr))}
          val label = '{$nameExpr.tt.aka["label"]}
          val fieldIndex = '{$indexExpr.asInstanceOf[Int & FieldIndex[field]].aka["index"]}

          applyLambda[field](lambdaTerm, contextValue, List(contextual, default, label, fieldIndex))

    val constructor = Select(New(TypeTree.of(using tpe.asType)), symbol.primaryConstructor)

    val construction = tpe.widen.dealias match
      case AppliedType(_, typeArguments) =>
        TypeApply(constructor, typeArguments.map { argument => TypeTree.of(using argument.asType) })

      case _ =>
        constructor

    Apply(construction, arguments).asExprOf[derivation]

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

          val contextValue: Expr[Any] = '{$requirement.summon[typeclass[field]]}
          val contextual = '{$requirement.summon[typeclass[field]].aka["contextual"]}
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

          val contextual = '{$requirement.summon[typeclass[field]].aka["contextual"]}
          val default = '{Default(wisteria.internal.default[derivation, field]($indexExpr))}
          val label = '{$nameExpr.tt.aka["label"]}
          val fieldIndex = '{$indexExpr.asInstanceOf[Int & FieldIndex[field]].aka["index"]}
          val arguments = List(contextual, default, label, fieldIndex)

          applyLambda[field](lambdaTerm, fieldValue, arguments).asExprOf[result]

    immutableArray[result](results)

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
