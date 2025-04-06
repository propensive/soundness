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
┃    Soundness, version 0.27.0.                                                                    ┃
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
package mandible

import java.lang.classfile as jlc

import scala.quoted.*

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import hellenism.*
import hyperbole.*
import nomenclature.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import turbulence.*
import vacuous.*

object Mandible:
  def disassemble[target: Type](block: Expr[target => Any], classloader: Expr[Classloader])
       (using Quotes)
  :     Expr[Optional[Bytecode]] =
    import quotes.reflect.*
    given Realm = realm"mandible"

    println(block.introspect)

    val name = block.asTerm match
      case Inlined(_, _, Block(List(DefDef(_, _, _, Some(Select(_, name)))), _)) => name

      case _ =>
        halt(m"this type of lambda is not supported")

    val classname: String =
      TypeRepr.of[target].classSymbol.get.fullName.replace(".", "/").nn+".class"

    '{
        val classfile = Classfile(${Expr(classname)})(using $classloader)
        println(${Expr(classname)})
        println("classfile: "+classfile)
        classfile.let(_.methods.find(_.name.s == ${Expr(name)}).getOrElse(Unset))
        . let(_.bytecode)
    }



    // '{  given Classloader = $classloader

    //     class Proxy():
    //       def bar: Any = $block
    //     val classname = ${Expr(TypeRepr.of[Proxy].typeSymbol.fullName)}
    //     println("classname: "+classname)
    //     val classfile = Classfile(classname)
    //     println("classfile: "+classfile)
    //     val method = classfile.let(_.methods.find(_.name == t"bar").getOrElse(Unset))
    //     method.let(_.bytecode)
    //  }
