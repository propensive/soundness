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
package mercator

import scala.compiletime.*
import scala.quoted.*

import anticipation.*
import fulminate.*
import gigantism.*
import proscenium.*

object internal:
  def point[typeConstructor[_]: Type]: Macro[Identity[typeConstructor]] =
    import quotes.reflect.*

    val identityType = TypeRepr.of[typeConstructor].typeSymbol
    val companion = Ref(identityType.companionModule)

    val applyMethods = companion.symbol.typeRef.typeSymbol.methodMembers.filter: method =>
      method.tree match
        case DefDef("apply", List(TypeParamClause(List(tpe)), terms), _, _) =>
          terms match
            case TermParamClause(List(ValDef(_, tRef, _))) => tRef.tpe.asMatchable match
              case AnnotatedType(AppliedType(ap, List(tRef)), annotation)
              if annotation.tpe.typeSymbol == defn.RepeatedAnnot =>
                tRef.typeSymbol == tpe.symbol

              case ByNameType(tRef) =>
                tpe.symbol.typeRef <:< tRef

              case other =>
                tpe.symbol.typeRef <:< tRef.tpe

            case other =>
              false

        case _ =>
          false

    if applyMethods.length == 1 then
      ' {
          new Identity[typeConstructor]:
            def point[value](value: value): typeConstructor[value] =
              $ {
                  companion
                  . select(applyMethods(0))
                  . appliedToType(TypeRepr.of[value])
                  . appliedTo('value.asTerm)
                  . asExprOf[typeConstructor[value]]
                }
        }

    else if applyMethods.length == 0
    then halt(1, m"the companion object ${identityType.name} has no candidate apply methods")
    else halt(2, m"the companion object ${identityType.name} has more than one candidate apply method")

  def functor[functor[_]](using Type[functor], Quotes): Expr[Functor[functor]] =
    import quotes.reflect.*

    val functorType = TypeRepr.of[functor].typeSymbol

    val mapMethods = functorType.methodMembers.filter: method =>
      method.tree match
        case DefDef("map", _, _, _) => true
        case _                      => false

    val pointExpr: Expr[Identity[functor]] = Expr.summon[Identity[functor]].getOrElse:
      halt(3, m"could not find Identity value for ${functorType.name}")

    lazy val makeFunctor =
      ' {
          new Functor[functor]:
            def point[value](value: value): functor[value] = ${pointExpr}.point(value)

            def apply[value, value2](value: functor[value])(lambda: value => value2)
            :   functor[value2] =

              $ {
                  'value.asTerm.select(mapMethods(0)).appliedToType(TypeRepr.of[value2])
                  . appliedTo('lambda.asTerm).asExprOf[functor[value2]]
                }
        }

    if mapMethods.length == 1 then makeFunctor
    else if mapMethods.length == 0 then halt(4, m"the type ${functorType.name} has no map methods")
    else halt(5, m"the type ${functorType.name} has more than one possible map method")

  def monad[monad[_]](using Type[monad], Quotes): Expr[Monad[monad]] =
    import quotes.reflect.*

    val monadType = TypeRepr.of[monad].typeSymbol

    val flatMapMethods = monadType.methodMembers.filter: method =>
      method.tree match
        case DefDef("flatMap", _, _, _) => true
        case _                          => false

    val functorExpr: Expr[Functor[monad]] = Expr.summon[Functor[monad]].getOrElse:
      halt(6, m"could not find Functor value for ${monadType.name}")

    lazy val makeMonad =
      ' {
          new Monad[monad]:
            def point[value](value: value): monad[value] = ${functorExpr}.point(value)

            def apply[value, value2](value: monad[value])(lambda: value => value2): monad[value2] =
              ${functorExpr}.map(value)(lambda)


            def bind[value, value2](value: monad[value])(lambda: value => monad[value2])
            :   monad[value2] =

              $ {
                  'value.asTerm
                  . select(flatMapMethods(0))
                  . appliedToType(TypeRepr.of[value2])
                  . appliedTo('lambda.asTerm)
                  . asExprOf[monad[value2]]
                }

        }

    if flatMapMethods.length == 1 then makeMonad
    else if flatMapMethods.length == 0
    then halt(7, m"the type ${monadType.name} has no flatMap methods")
    else halt(8, m"the type ${monadType.name} has more than one possible flatMap method")
