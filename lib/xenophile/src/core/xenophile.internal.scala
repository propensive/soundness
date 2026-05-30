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
package xenophile

import scala.quoted.*

import anticipation.*
import fulminate.*
import gigantism.*
import gossamer.*
import hellenism.*
import rudiments.*
import vacuous.*

object Xenophile:

  // Parses the trivial line-based definitions format into a map from each foreign type name to a
  // map from its member names to the names of their (foreign) types. For example:
  //
  //     Foo:
  //       bar Bar
  //       baz Text
  //     Bar:
  //       qux Foo
  //
  // becomes `Map(Foo -> Map(bar -> Bar, baz -> Text), Bar -> Map(qux -> Foo))`.
  def parse(lines: List[Text]): Map[Text, Map[Text, Text]] =
    def recur(todo: List[Text], current: Text, acc: Map[Text, Map[Text, Text]])
    :   Map[Text, Map[Text, Text]] =

      todo match
        case Nil =>
          acc

        case line :: tail =>
          val tokens = line.cut(t" ").to(List).filter(_ != t"")

          if tokens.isEmpty then
            recur(tail, current, acc)
          else if !line.starts(t" ") && line.ends(t":") then
            val name = line.cut(t":").to(List).head
            recur(tail, name, acc.updated(name, Map()))
          else
            tokens match
              case member :: kind :: _ =>
                val updated = acc.at(current).or(Map()).updated(member, kind)
                recur(tail, current, acc.updated(current, updated))

              case _ =>
                recur(tail, current, acc)

    recur(lines, t"", Map())

  def definitions(path: Text)(using Quotes): Map[Text, Map[Text, Text]] =
    val stream = Optional(getClass.getResourceAsStream(path.s)).or:
      halt(m"xenophile: could not read foreign definitions at $path on the classpath")

    parse(scala.io.Source.fromInputStream(stream).getLines().map(Text(_)).to(List))

  def select(self: Expr[Foreign], field: Expr[String]): Macro[Foreign] =
    import quotes.reflect.*

    val fieldName = field.valueOrAbort.tt

    // Collects every `type X = …` member from a (possibly nested) refinement type into a map.
    def refinements(repr: TypeRepr): Map[Text, TypeRepr] = repr.dealias match
      case Refinement(parent, name, TypeBounds(_, hi)) => refinements(parent).updated(name.tt, hi)
      case Refinement(parent, name, info)              => refinements(parent).updated(name.tt, info)
      case AndType(left, right)                        => refinements(left) ++ refinements(right)
      case _                                           => Map()

    val members = refinements(self.asTerm.tpe.widen)

    val topicRepr = members.at(t"Topic").or:
      halt(m"xenophile: the receiver is not a singleton foreign type (it has no `Topic`)")

    val topic = topicRepr.absolve match
      case ConstantType(StringConstant(name)) => name.tt

      case _ =>
        halt(m"xenophile: the foreign type's `Topic` is not a string literal type")

    val originRepr = members.at(t"Origin").or:
      halt(m"xenophile: the receiver does not record its source language (it has no `Origin`)")

    val interfaceType =
      Refinement(TypeRepr.of[Interface], "Form", TypeBounds(originRepr, originRepr))

    val interfaceTerm = interfaceType.asType.absolve match
      case '[interface] =>
        val interface = Expr.summon[interface].getOrElse:
          halt(m"xenophile: no `Interface` is available for the foreign source language")

        interface.asTerm

    val interfaceMembers =
      refinements(interfaceTerm.tpe) ++ refinements(interfaceTerm.tpe.widen)

    val locusRepr = interfaceMembers.at(t"Locus").or:
      halt(m"xenophile: the `Interface` does not specify a definitions path (it has no `Locus`)")

    val locus = locusRepr.absolve match
      case ConstantType(StringConstant(path)) => path.tt

      case _ =>
        halt(m"xenophile: the definitions path is not a string literal type")

    val defs = definitions(locus)

    val typeMembers = defs.at(topic).or:
      halt(m"xenophile: the foreign type $topic is not defined in $locus")

    val memberType = typeMembers.at(fieldName).or:
      halt(m"xenophile: the foreign type $topic has no member $fieldName")

    val topicType = ConstantType(StringConstant(memberType.s))

    val resultType =
      Refinement
        ( Refinement(TypeRepr.of[Foreign], "Topic", TypeBounds(topicType, topicType)),
          "Origin",
          TypeBounds(originRepr, originRepr) )

    resultType.asType.absolve match
      case '[type result <: Foreign; result] =>
        '{Foreign.make(${Expr(memberType.s)}).asInstanceOf[result]}

  def interface[form: Type](resource: Expr[Resource]): Macro[Interface] =
    import quotes.reflect.*

    def refinements(repr: TypeRepr): Map[Text, TypeRepr] = repr.dealias match
      case Refinement(parent, name, TypeBounds(_, hi)) => refinements(parent).updated(name.tt, hi)
      case Refinement(parent, name, info)              => refinements(parent).updated(name.tt, info)
      case AndType(left, right)                        => refinements(left) ++ refinements(right)
      case _                                           => Map()

    val members = refinements(resource.asTerm.tpe) ++ refinements(resource.asTerm.tpe.widen)

    val locusRepr = members.at(t"Locus").or:
      halt(m"xenophile: the resource does not carry a singleton path type (it has no `Locus`)")

    val formRepr = TypeRepr.of[form]

    val resultType =
      Refinement
        ( Refinement(TypeRepr.of[Interface], "Form", TypeBounds(formRepr, formRepr)),
          "Locus",
          TypeBounds(locusRepr, locusRepr) )

    resultType.asType.absolve match
      case '[type result <: Interface; result] =>
        '{(new Interface {}).asInstanceOf[result]}

  def root[name <: Label: Type, origin: Type]: Macro[Foreign] =
    import quotes.reflect.*

    val nameRepr = TypeRepr.of[name]

    val name = nameRepr.absolve match
      case ConstantType(StringConstant(string)) => string

      case _ =>
        halt(m"xenophile: the foreign type name must be a string literal type")

    val originRepr = TypeRepr.of[origin]

    val resultType =
      Refinement
        ( Refinement(TypeRepr.of[Foreign], "Topic", TypeBounds(nameRepr, nameRepr)),
          "Origin",
          TypeBounds(originRepr, originRepr) )

    resultType.asType.absolve match
      case '[type result <: Foreign; result] =>
        '{Foreign.make(${Expr(name)}).asInstanceOf[result]}
