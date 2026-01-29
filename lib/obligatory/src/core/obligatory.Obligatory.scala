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
┃    Soundness, version 0.53.0.                                                                    ┃
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
package obligatory

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import gossamer.*
import hieroglyph.*
import inimitable.*
import jacinta.*
import parasite.*
import prepositional.*
import proscenium.*
import revolution.*
import rudiments.*
import spectacular.*
import telekinesis.*
import urticose.*
import vacuous.*
import zephyrine.*

import scala.annotation.*
import scala.quoted.*

import errorDiagnostics.stackTraces

case class remote() extends StaticAnnotation

object Obligatory:
  given Realm = realm"obligatory"

  def remote[interface: Type](url: Expr[HttpUrl]): Macro[interface] =
    import quotes.reflect.*
    val remoteType = TypeRepr.of[remote].typeSymbol
    val interface = TypeRepr.of[interface]

    val remoteMethods = TypeRepr.of[interface].typeSymbol.declaredMethods.filter: method =>
      method.annotations.exists(_.tpe.typeSymbol == remoteType)

    def decls(cls: Symbol) = remoteMethods.map: method =>
      Symbol.newMethod(cls, method.name, method.info, Flags.EmptyFlags, Symbol.noSymbol)


    val parents  = List(TypeTree.of[Object], TypeTree.of[interface])

    val module = Symbol.newModule
     (owner    = Symbol.spliceOwner,
      name     = Symbol.freshName(interface.typeSymbol.name),
      modFlags = Flags.EmptyFlags,
      clsFlags = Flags.EmptyFlags,
      parents  = _ => parents.map(_.tpe),
      decls    = decls,
      privateWithin = Symbol.noSymbol)

    val cls = module.moduleClass


    val defDefs = remoteMethods.map: method =>
      val paramSymbols = method.paramSymss.head

      val runSym = cls.declaredMethod(method.name).head
      DefDef(runSym, {
        case List(params) =>
          given Quotes = runSym.asQuotes
          val entries = params.zip(paramSymbols).map: (ident, param) =>
            param.info.asType match
              case '[param] =>
                Expr.summon[param is Encodable in Json] match
                  case Some('{$encoder: (`param` `is` Encodable `in` Json) }) =>
                    val name = Expr(param.name)
                    '{ $name -> ${encoder}.encoded(${ident.asExprOf[param]}) }
                  case _ =>
                    halt(m"no encoder found for parameter ${param.name} of method ${method.name}")
              case _ =>
                halt(m"all remote methods in ${TypeRepr.of[interface].show} must have a single parameter list")

          val notification = runSym.info match
            case MethodType(_, _, rtn) => rtn.typeSymbol == TypeRepr.of[Unit].typeSymbol
            case _                     => false

          val id = if notification then '{Unset} else Expr(Uuid().show)
          val methodName = Expr(method.name.tt)
          (Expr.summon[Monitor], Expr.summon[Codicil]) match
            case (Some(monitor), Some(codicil)) =>
              Some:
                ' {
                    val json = Map(${Varargs(entries)}*).json
                    unsafely:
                      JsonRpc.handle($url, $methodName, json)(using $monitor, $codicil)
                      . await()
                  }.asTerm
            case _ =>
              halt(m"no monitor found")
        }
      )


    val modDef = ClassDef.module(module, parents, body = defDefs)


    Block(modDef.toList, Ref(module)).asExprOf[interface]
