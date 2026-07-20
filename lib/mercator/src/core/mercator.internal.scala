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
package mercator

import scala.annotation

import scala.collection.immutable.{List, Nil, ::}

import scala.compiletime.*
import scala.quoted.*

import anticipation.*
import fulminate.*
import gigantism.*

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
    then
      halt(569, m"the companion object ${identityType.name} has no candidate apply methods")
    else
      halt
        ( 659,
          m"the companion object ${identityType.name} has more than one candidate apply method" )

  def functor[functor[_]](using Type[functor], Quotes): Expr[Functor[functor]] =
    import quotes.reflect.*

    val functorType = TypeRepr.of[functor].typeSymbol

    val mapMethods = functorType.methodMembers.filter: method =>
      method.tree match
        case DefDef("map", _, _, _) => true
        case _                      => false

    val pointExpr: Expr[Identity[functor]] = Expr.summon[Identity[functor]].getOrElse:
      halt(970, m"could not find Identity value for ${functorType.name}")

    lazy val makeFunctor =
      ' {
          new Functor[functor]:
            def point[value](value: value): functor[value] = ${pointExpr}.point(value)

            def apply[value, value2](value: functor[value])(lambda: value => value2)
            :   functor[value2] =

              // Build `(v, l) => v.map(l)` as a reflective lambda whose parameters are real symbols,
              // then apply it to `value`/`lambda` at the quote level. Quoting the `lambda` parameter
              // directly into the splice (`'lambda`) would reify it as a boxed `Expr` hole that does
              // not conform to its unboxed `value ->{any} value2` type under capture checking.
              $ {
                  val methodType = MethodType(List("v", "l"))(
                    _ => List(TypeRepr.of[functor[value]], TypeRepr.of[value => value2]),
                    _ => TypeRepr.of[functor[value2]])

                  Lambda(Symbol.spliceOwner, methodType, (_, args) =>
                    args(0).asInstanceOf[Term].select(mapMethods(0)).appliedToType(TypeRepr.of[value2])
                    . appliedTo(args(1).asInstanceOf[Term])).asExpr
                }
              . asInstanceOf[(functor[value], value => value2) => functor[value2]]
              . apply(value, lambda)
        }

    if mapMethods.length == 1 then makeFunctor
    else if mapMethods.length == 0
    then halt(899, m"the type ${functorType.name} has no map methods")
    else halt(380, m"the type ${functorType.name} has more than one possible map method")

  def monad[monad[_]](using Type[monad], Quotes): Expr[Monad[monad]] =
    import quotes.reflect.*

    val monadType = TypeRepr.of[monad].typeSymbol

    val flatMapMethods = monadType.methodMembers.filter: method =>
      method.tree match
        case DefDef("flatMap", _, _, _) => true
        case _                          => false

    val functorExpr: Expr[Functor[monad]] = Expr.summon[Functor[monad]].getOrElse:
      halt(585, m"could not find Functor value for ${monadType.name}")

    lazy val makeMonad =
      ' {
          new Monad[monad]:
            def point[value](value: value): monad[value] = ${functorExpr}.point(value)

            def apply[value, value2](value: monad[value])(lambda: value => value2): monad[value2] =
              ${functorExpr}.map(value)(lambda)


            def bind[value, value2](value: monad[value])(lambda: value => monad[value2])
            :   monad[value2] =

              // As in `Functor.apply`: build `(v, l) => v.flatMap(l)` reflectively and apply it at
              // the quote level, so the capturing `lambda` never crosses a splice boundary as a
              // boxed `Expr` hole (which capture checking rejects).
              $ {
                  val methodType = MethodType(List("v", "l"))(
                    _ => List(TypeRepr.of[monad[value]], TypeRepr.of[value => monad[value2]]),
                    _ => TypeRepr.of[monad[value2]])

                  Lambda(Symbol.spliceOwner, methodType, (_, args) =>
                    args(0).asInstanceOf[Term].select(flatMapMethods(0))
                    . appliedToType(TypeRepr.of[value2]).appliedTo(args(1).asInstanceOf[Term])).asExpr
                }
              . asInstanceOf[(monad[value], value => monad[value2]) => monad[value2]]
              . apply(value, lambda)

        }

    if flatMapMethods.length == 1 then makeMonad
    else if flatMapMethods.length == 0
    then halt(356, m"the type ${monadType.name} has no flatMap methods")
    else halt(538, m"the type ${monadType.name} has more than one possible flatMap method")
