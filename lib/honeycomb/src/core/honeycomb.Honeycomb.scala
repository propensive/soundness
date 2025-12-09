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
┃    Soundness, version 0.46.0.                                                                    ┃
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
package honeycomb

import anticipation.*
import contingency.*
import denominative.*
import fulminate.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import stenography.*
import vacuous.*

import scala.quoted.*

object Honeycomb:
  private given realm: Realm = realm"honeycomb"

  def interpolator(context: Expr[StringContext], insertions0: Expr[Seq[Any]]): Macro[Html] =
    import quotes.reflect.*
    import doms.whatwg
    import Html.Hole

    val StringContext(parts*) = context.valueOrAbort

    val insertions: Seq[Expr[Any]] = insertions0.absolve match
      case Varargs(insertions) => insertions

    abortive:
      var holes: Map[Ordinal, Html.Hole] = Map()

      val html: Html =
        Html.parse(Iterator(parts.mkString("\u0000")), whatwg.generic, (ordinal, hole) => holes = holes.updated(ordinal, hole))

      val iterator: Iterator[Expr[Any]] =
        holes.to(List).sortBy(_(0)).map(_(1)).zip(insertions).map: (hole, expr) =>
          expr match
            case '{ $expr: value } => hole match
              case Hole.Attribute(tag, attribute) => ConstantType(StringConstant(tag.s)).asType match
                case '[tag] => ConstantType(StringConstant(attribute.s)).asType match
                  case '[attribute] =>
                    Expr.summon[attribute is Attribute in Whatwg on (? >: tag)]
                    . orElse(Expr.summon[attribute is Attribute in Whatwg]) match
                      case Some('{ type result; $typeclass: Attribute { type Topic = result } }) =>

                        Expr.summon[(? >: value) is Attributive to result] match
                          case Some('{$attributive}) =>
                            '{$attributive.attribute(${Expr(attribute)}, $expr).let(_(1))}

                          case None =>
                            halt(m"${TypeRepr.of[value].show} cannot be attributed to an attribute of ${TypeRepr.of[result].show}")

                      case None =>
                        halt(m"the attribute $attribute cannot be used on the element <$tag>")

              case Hole.Node(tag) =>
                ConstantType(StringConstant(tag.s)).asType match
                  case '[tag] => Expr.summon[(? >: value) is Renderable in (? >: tag)] match
                    case Some('{$renderable: Renderable}) =>
                      '{$renderable.render($expr)}

                    case None =>
                      Expr.summon[(? >: value) is Showable] match
                        case Some('{$showable: Showable}) =>
                          '{Html.Textual($showable.text($expr))}

                        case None =>
                          halt(m"""a value of ${TypeRepr.of[value].show} is not renderable
                                  or showable inside a <$tag> element""")

              case Hole.Comment => Expr.summon[(? >: value) is Showable] match
                case Some(showable) =>
                  '{$showable.text($expr)}

                case None =>
                  halt(m"a ${TypeRepr.of[value is Showable].show} is required")

              case Hole.Text => Expr.summon[(? >: value) is Showable] match
                case Some(showable) =>
                  '{$showable.text($expr)}

                case None =>
                  halt(m"a ${TypeRepr.of[value is Showable].show} is required")

              case Hole.Tagbody => Type.of[value] match
                case '[Map[Text, Optional[Text]]] =>
                  expr
                case _ =>
                  halt(m"only a ${TypeRepr.of[Map[Text, Optional[Text]]].show} can be applied in a tag body")
        . iterator

      def serialize(html: Html): Seq[Expr[Html]] = html match
        case Html.Fragment(children*) => children.flatMap(serialize(_))
        case Html.Node(label, attributes, children, foreign) =>
          val exprs = attributes.to(List).map: (key, value) =>
            '{  (${Expr(key)},
                 ${  if value == "\u0000".tt then iterator.next().asExprOf[Optional[Text]]
                     else if value == Unset then '{Unset}
                     else Expr[Text](value.asInstanceOf[Text])  })  }
            . asExprOf[(Text, Optional[Text])]

          val map = '{Map(${Expr.ofList(exprs)}*)}
          val elements = '{IArray(${Expr.ofList(children.flatMap(serialize(_)))}*)}

          List('{Html.Node(${Expr(label)}, $map, $elements, ${Expr(foreign)})})

        case Html.Comment(text) =>
          val parts = text.s.split("\u0000").nn.map(_.nn).to(List)

          def recur(parts: List[String], expr: Expr[String]): Expr[String] = parts match
            case Nil => expr
            case head :: tail =>
              recur(tail, '{$expr+${iterator.next().asExprOf[Text]}+${Expr(head)}})

          val content = recur(parts.tail, Expr(parts.head))

          List('{Html.Comment($content.tt)})

        case Html.Textual("\u0000") =>
          List(iterator.next().asExprOf[Html])

        case Html.Textual(text) =>
          val parts = text.s.split("\u0000").nn.map(_.nn).to(List)

          def recur(parts: List[String], expr: Expr[String]): Expr[String] = parts match
            case Nil => expr
            case head :: tail =>
              recur(tail, '{$expr+${iterator.next().asExprOf[Text]}+${Expr(head)}})

          val content = recur(parts.tail, Expr(parts.head))

          List('{Html.Textual($content.tt)})

      def resultType(html: Html): Set[String] = html match
        case Html.Textual(_)         => Set("#text")
        case Html.Node(tag, _, _, _) => Set(tag.s)
        case Html.Fragment(values*)  => values.to(Set).flatMap(resultType(_))
        case Html.Comment(_)         => Set()

      resultType(html)
      . map { label => ConstantType(StringConstant(label)) }
      . foldLeft(TypeRepr.of[Nothing]) { (left, right) => OrType(left, right) }
      . asType match
          case '[type topic <: Label; topic] =>
            '{
                ${  serialize(html) match
                      case List(one: Expr[Html]) => one
                      case many                  => '{Html.Fragment(${Expr.ofList(many)}*)}  }
                . of[topic]  }




  def attributes[result: Type, thisType <: Tag to result: Type]
       (tag: Expr[Tag], attributes0: Expr[Seq[(String, Any)]])
  : Macro[result] =
      import quotes.reflect.*

      val Varargs(args) = attributes0

      val attributes: Seq[Expr[Optional[(Text, Optional[Text])]]] =
        Type.of[thisType] match
          case '[ type topic <: Label;
                  type form;
                  Tag { type Topic = topic; type Form = form } ] => args.map:
            case '{ ($key, $value: value) } =>
              TypeRepr.of[topic].literal[String].let: topic =>
                key.asTerm match
                  case Literal(StringConstant(key)) =>
                    if key == "" then halt(m"Empty key")
                    else ConstantType(StringConstant(key)).asType match
                      case '[type key <: Label; key] =>
                        Expr.summon[key is Attribute in form on (? >: topic)]
                        . orElse(Expr.summon[key is Attribute in form]) match
                          case Some('{ type result; $expr: Attribute { type Topic = result } }) =>
                            Expr.summon[(? >: value) is Attributive to result] match
                              case Some('{ $converter: Attributive }) =>
                                '{$converter.attribute(${Expr(key)}, $value)}

                              case _ =>
                                halt(m"there is no converter for ${TypeRepr.of[result].show} attributes")

                          case _ =>
                            halt(m"the attribute $key cannot be used on the element <$topic>")

                  case _ =>
                    halt(m"unable to determine attribute key type")
              . or(halt(m"unexpected type"))

      '{$tag.node(${Expr.ofList(attributes)}.compact.to(Map))}.asExprOf[result]
