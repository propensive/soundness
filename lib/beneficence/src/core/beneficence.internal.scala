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
package beneficence

import scala.io.{Codec, Source}
import scala.jdk.CollectionConverters.*
import scala.quoted.*

object internal:
  def givens(using quotes: Quotes)(typeRepr: quotes.reflect.TypeRepr)
  :   List[quotes.reflect.Symbol] =

    val classLoader = this.getClass.getClassLoader.nn

    val classSymbol = typeRepr.dealias.classSymbol
    if classSymbol.isEmpty then Nil
    else
      val typeclassFqn = classSymbol.get.fullName
      val resourcePath = s"META-INF/givens/$typeclassFqn"

      val urls = classLoader.getResources(resourcePath).nn.asScala.toList
      val rawEntries: List[String] = urls.flatMap: url =>
        val source = Source.fromURL(url)(using Codec.UTF8)
        try source.getLines().toList.collect:
          case line if !line.isBlank && !line.startsWith("#") => line.trim.nn
        finally source.close()

      rawEntries.distinct.flatMap(lookup)

  private def lookup(fqn: String)(using quotes: Quotes): Option[quotes.reflect.Symbol] =
    import quotes.reflect.Symbol

    val lastDot = fqn.lastIndexOf('.')
    if lastDot < 0 then None
    else
      val ownerFqn   = fqn.substring(0, lastDot).nn
      val memberName = fqn.substring(lastDot + 1).nn

      val owner: Option[Symbol] =
        attempt(Symbol.requiredModule(ownerFqn))
        . orElse(attempt(Symbol.requiredClass(ownerFqn)))

      owner.flatMap: ownerSymbol =>
        val methods = attempt(ownerSymbol.declaredMethod(memberName)).getOrElse(Nil)
        if methods.nonEmpty then Some(methods.head)
        else
          val field = attempt(ownerSymbol.declaredField(memberName)).getOrElse(Symbol.noSymbol)
          if field.exists then Some(field) else None

  private def attempt[A](thunk: => A): Option[A] =
    try Option(thunk) catch case _: Throwable => None
