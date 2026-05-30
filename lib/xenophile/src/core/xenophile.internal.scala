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

  // Selects the grammar for a source language. New ecosystems register here.
  private def dialectFor(using Quotes)(origin: quotes.reflect.TypeRepr): Dialect =
    import quotes.reflect.*

    if origin =:= TypeRepr.of[Typescript] then TypescriptDialect
    else halt(m"xenophile: no grammar is available for the foreign source language")

  // Reads the definitions resource at `path` and parses it with the grammar for `origin`.
  def definitions(using quotes: Quotes)(origin: quotes.reflect.TypeRepr, path: Text)
  :   Map[Text, Map[Text, Signature]] =

    val stream = Optional(getClass.getResourceAsStream(path.s)).or:
      halt(m"xenophile: could not read foreign definitions at $path on the classpath")

    dialectFor(origin).parse(scala.io.Source.fromInputStream(stream).mkString.tt)

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

  // Builds the `ForeignExpr` for a single method argument, checking it against the declared
  // parameter type: a `Foreign` argument must already have that foreign type (`Topic`); any other
  // value must have an `Interoperable` instance mapping it to exactly that foreign type.
  private def argTree(using quotes: Quotes)
    ( arg: Expr[Any], paramType: Text, originRepr: quotes.reflect.TypeRepr, method: Text )
  :   Expr[ForeignExpr] =

    import quotes.reflect.*

    val paramTopic = ConstantType(StringConstant(paramType.s))

    arg.asTerm.tpe.widen.asType.absolve match
      case '[argument] =>
        val argRepr = TypeRepr.of[argument]

        if argRepr <:< TypeRepr.of[Foreign] then
          val argTopic = refinements(argRepr).at(t"Topic").or:
            halt(m"xenophile: the foreign type of an argument to $method is not known")

          val matches = argTopic.absolve match
            case ConstantType(StringConstant(name)) => name.tt == paramType
            case _                                  => false

          if matches then '{${arg.asExprOf[Foreign]}.expr}
          else halt(m"xenophile: $method expects an argument of foreign type $paramType")
        else
          val base = Refinement(TypeRepr.of[Interoperable], "Self", TypeBounds(argRepr, argRepr))
          val withForm = Refinement(base, "Form", TypeBounds(originRepr, originRepr))
          val interopType = Refinement(withForm, "Topic", TypeBounds(paramTopic, paramTopic))

          interopType.asType.absolve match
            case '[type interop <: Interoperable { type Self = argument }; interop] =>
              Expr.summon[interop].absolve match
                case Some(instance) =>
                  '{ForeignExpr.Literal($instance.operand(${arg.asExprOf[argument]}))}

                case _ =>
                  halt(m"xenophile: cannot pass this argument as $paramType to $method")

  def select(self: Expr[Foreign], field: Expr[String]): Macro[Foreign] =
    val fieldName = field.valueOrAbort.tt
    val (topic, originRepr) = receiver(self)

    val typeMembers = definitions(originRepr, locusOf(originRepr)).at(topic).or:
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
    val fieldName = field.valueOrAbort.tt
    val (topic, originRepr) = receiver(self)

    val typeMembers = definitions(originRepr, locusOf(originRepr)).at(topic).or:
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

    val argTrees: List[Expr[ForeignExpr]] = args.zip(parameters).map: (arg, paramType) =>
      argTree(arg, paramType, originRepr, fieldName)

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

    val operandRepr = originRepr.memberType(originRepr.typeSymbol.typeMember("Operand")) match
      case TypeBounds(_, hi) => hi
      case repr              => repr

    val evalForm = Refinement(TypeRepr.of[Evaluator], "Form", TypeBounds(originRepr, originRepr))
    val evaluatorType = Refinement(evalForm, "Operand", TypeBounds(operandRepr, operandRepr))

    operandRepr.asType.absolve match
      case '[operand] => evaluatorType.asType.absolve match
        case '[type evaluator <: Evaluator { type Operand = operand }; evaluator] =>
          val ev = Expr.summon[evaluator].getOrElse:
            halt(m"xenophile: no `Evaluator` is available for the foreign source language")

          // Summons the `Interoperable` for `elementTopic` and decodes one operand value with it.
          def element(elementRepr: TypeRepr, elementTopic: Text, data: Expr[operand]): Expr[Any] =
            val literal = ConstantType(StringConstant(elementTopic.s))
            val bounds = TypeBounds(elementRepr, elementRepr)
            val withSelf = Refinement(TypeRepr.of[Interoperable], "Self", bounds)
            val withForm = Refinement(withSelf, "Form", TypeBounds(originRepr, originRepr))
            val withTopic = Refinement(withForm, "Topic", TypeBounds(literal, literal))
            val full = Refinement(withTopic, "Operand", TypeBounds(operandRepr, operandRepr))

            full.asType.absolve match
              case '[type io <: Interoperable { type Operand = operand }; io] =>
                val instance = Expr.summon[io].getOrElse:
                  halt(m"xenophile: cannot read $elementTopic; no matching `Interoperable`")

                '{$instance.value($data)}.asExprOf[Any]

          if topic.ends(t"?") then
            val base = topic.cut(t"?").to(List).head

            val innerRepr = TypeRepr.of[target].dealias match
              case OrType(left, right) => if left =:= TypeRepr.of[Unset.type] then right else left

              case _ =>
                halt(m"xenophile: reading an optional foreign type needs an `Optional` Scala type")

            val tree =
              ' {
                  val data: operand = $ev.evaluate($self.expr)
                  if $ev.absent(data) then Unset else ${element(innerRepr, base, 'data)}
                }

            tree.asExprOf[target]

          else
            element(TypeRepr.of[target], topic, '{$ev.evaluate($self.expr)}).asExprOf[target]

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
