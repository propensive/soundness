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

  case class Signature(parameters: Optional[List[Text]], result: Text)

  // Parses the trivial line-based definitions format into a map from each foreign type name to a
  // map from member names to their signatures. A member is either a field (`bar Bar`) or a method
  // (`greet(string) Text`); a method records its parameter type names and result type.
  def parse(lines: List[Text]): Map[Text, Map[Text, Signature]] =
    def recur(todo: List[Text], current: Text, acc: Map[Text, Map[Text, Signature]])
    :   Map[Text, Map[Text, Signature]] =

      todo match
        case Nil =>
          acc

        case line :: tail =>
          val tokens = line.cut(t" ").to(List).filter(_ != t"")

          if tokens.isEmpty then recur(tail, current, acc)
          else if !line.starts(t" ") && line.ends(t":") then
            val name = line.cut(t":").to(List).head
            recur(tail, name, acc.updated(name, Map()))
          else
            val result = tokens.last
            val segments = tokens.head.cut(t"(").to(List)
            val name = segments.head

            val signature =
              if segments.length == 1 then Signature(Unset, result)
              else
                val inside = segments.tail.head.cut(t")").to(List).head
                Signature(inside.cut(t",").to(List).filter(_ != t""), result)

            val members = acc.at(current).or(Map()).updated(name, signature)
            recur(tail, current, acc.updated(current, members))

    recur(lines, t"", Map())

  def definitions(path: Text)(using Quotes): Map[Text, Map[Text, Signature]] =
    val stream = Optional(getClass.getResourceAsStream(path.s)).or:
      halt(m"xenophile: could not read foreign definitions at $path on the classpath")

    parse(scala.io.Source.fromInputStream(stream).getLines().map(Text(_)).to(List))

  // Collects every `type X = …` member from a (possibly nested) refinement type into a map.
  private def refinements(using quotes: Quotes)(repr: quotes.reflect.TypeRepr)
  :   Map[Text, quotes.reflect.TypeRepr] =

    import quotes.reflect.*

    repr.dealias match
      case Refinement(parent, name, TypeBounds(_, hi)) => refinements(parent).updated(name.tt, hi)
      case Refinement(parent, name, info)              => refinements(parent).updated(name.tt, info)
      case AndType(left, right)                        => refinements(left) ++ refinements(right)
      case _                                           => Map()

  // Reads the `Topic` (foreign type name) and `Origin` (source language) from a `Foreign` receiver.
  private def receiver(using quotes: Quotes)(self: Expr[Foreign])
  :   (Text, quotes.reflect.TypeRepr) =

    import quotes.reflect.*

    val members = refinements(self.asTerm.tpe.widen)

    val topicRepr = members.at(t"Topic").or:
      halt(m"xenophile: the receiver is not a singleton foreign type (it has no `Topic`)")

    val topic = topicRepr.absolve match
      case ConstantType(StringConstant(name)) => name.tt

      case _ =>
        halt(m"xenophile: the foreign type's `Topic` is not a string literal type")

    val origin = members.at(t"Origin").or:
      halt(m"xenophile: the receiver does not record its source language (it has no `Origin`)")

    (topic, origin)

  // Summons the `Interface` given for a source language and reads its definitions path (`Locus`).
  private def locusOf(using quotes: Quotes)(origin: quotes.reflect.TypeRepr): Text =
    import quotes.reflect.*

    val interfaceType = Refinement(TypeRepr.of[Interface], "Form", TypeBounds(origin, origin))

    val interfaceTerm = interfaceType.asType.absolve match
      case '[interface] =>
        val found = Expr.summon[interface].getOrElse:
          halt(m"xenophile: no `Interface` is available for the foreign source language")

        found.asTerm

    val members = refinements(interfaceTerm.tpe) ++ refinements(interfaceTerm.tpe.widen)

    val locusRepr = members.at(t"Locus").or:
      halt(m"xenophile: the `Interface` does not specify a definitions path (it has no `Locus`)")

    locusRepr.absolve match
      case ConstantType(StringConstant(path)) => path.tt

      case _ =>
        halt(m"xenophile: the definitions path is not a string literal type")

  // Builds the refined type `Foreign of <topic> from <origin>`.
  private def foreignType(using quotes: Quotes)(topic: Text, origin: quotes.reflect.TypeRepr)
  :   quotes.reflect.TypeRepr =

    import quotes.reflect.*

    val topicType = ConstantType(StringConstant(topic.s))

    Refinement
      ( Refinement(TypeRepr.of[Foreign], "Topic", TypeBounds(topicType, topicType)),
        "Origin",
        TypeBounds(origin, origin) )

  def select(self: Expr[Foreign], field: Expr[String]): Macro[Foreign] =
    val fieldName = field.valueOrAbort.tt
    val (topic, originRepr) = receiver(self)

    val typeMembers = definitions(locusOf(originRepr)).at(topic).or:
      halt(m"xenophile: the foreign type $topic is not defined")

    val signature = typeMembers.at(fieldName).or:
      halt(m"xenophile: the foreign type $topic has no member $fieldName")

    signature.parameters.let: _ =>
      halt(m"xenophile: $fieldName is a method of $topic and must be called with arguments")

    foreignType(signature.result, originRepr).asType.absolve match
      case '[type result <: Foreign; result] =>
        val tree = '{ForeignExpr.Select($self.expr, ${Expr(fieldName.s)}.tt)}
        '{Foreign.make($tree).asInstanceOf[result]}

  def applied(self: Expr[Foreign], field: Expr[String], arguments: Expr[Seq[Any]]): Macro[Foreign] =
    import quotes.reflect.*

    val fieldName = field.valueOrAbort.tt
    val (topic, originRepr) = receiver(self)

    val typeMembers = definitions(locusOf(originRepr)).at(topic).or:
      halt(m"xenophile: the foreign type $topic is not defined")

    val signature = typeMembers.at(fieldName).or:
      halt(m"xenophile: the foreign type $topic has no member $fieldName")

    val parameters = signature.parameters.or:
      halt(m"xenophile: $fieldName is not a method of $topic")

    val args = arguments match
      case Varargs(exprs) => exprs.to(List)

      case _ =>
        halt(m"xenophile: the arguments to $fieldName must be passed directly")

    if args.length != parameters.length then
      halt(m"xenophile: $fieldName expects ${parameters.length} arguments, not ${args.length}")

    val argTrees: List[Expr[ForeignExpr]] = args.map: arg =>
      arg.asTerm.tpe.widen.asType.absolve match
        case '[argument] =>
          if TypeRepr.of[argument] <:< TypeRepr.of[Foreign] then '{${arg.asExprOf[Foreign]}.expr}
          else
            val interopType =
              Refinement
                ( Refinement
                    ( TypeRepr.of[Interoperable],
                      "Self",
                      TypeBounds(TypeRepr.of[argument], TypeRepr.of[argument]) ),
                  "Form",
                  TypeBounds(originRepr, originRepr) )

            interopType.asType.absolve match
              case '[type interop <: Interoperable { type Self = argument }; interop] =>
                Expr.summon[interop].absolve match
                  case Some(instance) =>
                    '{ForeignExpr.Literal($instance.operand(${arg.asExprOf[argument]}))}

                  case _ =>
                    halt(m"xenophile: no `Interoperable` instance for this argument to $fieldName")

    val target = '{ForeignExpr.Select($self.expr, ${Expr(fieldName.s)}.tt)}
    val tree = '{ForeignExpr.Apply($target, ${Expr.ofList(argTrees)})}

    foreignType(signature.result, originRepr).asType.absolve match
      case '[type result <: Foreign; result] =>
        '{Foreign.make($tree).asInstanceOf[result]}

  // Converts a foreign value to a Scala type, via the `Interoperable` instance registered for the
  // receiver's foreign type (its `Topic`) — the same typeclass that converts Scala values inwards.
  def convert[target: Type](self: Expr[Foreign]): Macro[target] =
    import quotes.reflect.*

    val (topic, originRepr) = receiver(self)
    val selfRepr = TypeRepr.of[target]
    val topicLiteral = ConstantType(StringConstant(topic.s))

    val operandRepr = originRepr.memberType(originRepr.typeSymbol.typeMember("Operand")) match
      case TypeBounds(_, hi) => hi
      case repr              => repr

    val base = Refinement(TypeRepr.of[Interoperable], "Self", TypeBounds(selfRepr, selfRepr))
    val withForm = Refinement(base, "Form", TypeBounds(originRepr, originRepr))
    val withTopic = Refinement(withForm, "Topic", TypeBounds(topicLiteral, topicLiteral))
    val interopType = Refinement(withTopic, "Operand", TypeBounds(operandRepr, operandRepr))

    operandRepr.asType.absolve match
      case '[operand] => interopType.asType.absolve match
        case '[type interop <: Interoperable { type Operand = operand }; interop] =>
          Expr.summon[interop].absolve match
            case Some(instance) =>
              val operand = '{Foreign.operandOf($self.expr).asInstanceOf[operand]}
              '{$instance.value($operand).asInstanceOf[target]}

            case _ =>
              halt(m"xenophile: cannot read $topic as this type; no matching `Interoperable`")

  def interface[form: Type](resource: Expr[Resource]): Macro[Interface] =
    import quotes.reflect.*

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

    foreignType(name.tt, TypeRepr.of[origin]).asType.absolve match
      case '[type result <: Foreign; result] =>
        '{Foreign.make(ForeignExpr.Reference(${Expr(name)}.tt)).asInstanceOf[result]}
