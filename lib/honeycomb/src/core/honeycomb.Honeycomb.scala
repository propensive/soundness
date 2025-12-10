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

import language.dynamics

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

  class Interpolator():
    type Topic <: Tuple
    inline def apply(inline insertions: Any*): Html = ${interpolator[Topic]('insertions)}

    transparent inline def unapply(node: Html): Any = ${extractor[Topic]('node)}

  def h(context: Expr[StringContext]): Macro[Interpolator] =
    import quotes.reflect.*

    def recur(parts: List[String], repr: TypeRepr = TypeRepr.of[EmptyTuple.type]): TypeRepr =
      parts match
        case head :: tail =>
          ConstantType(StringConstant(head)).asType.absolve match
            case '[label] => repr.asType.absolve match
              case '[type tuple <: Tuple; tuple] =>  recur(tail, TypeRepr.of[label *: tuple])
        case Nil =>
          repr

    recur(context.valueOrAbort.parts.to(List)).asType.absolve match
      case '[type tuple <: Tuple; tuple] => '{  new Interpolator() { type Topic = tuple }  }

  def extractor[parts <: Tuple: Type](scrutinee: Expr[Html]): Macro[Any] =
    import quotes.reflect.*
    import doms.whatwg

    def recur[tuple: Type](strings: List[String]): List[String] = Type.of[tuple] match
      case '[head *: tail] => recur[tail](TypeRepr.of[head].literal[String].vouch :: strings)
      case _               => strings

    val parts = recur[parts](Nil)

    abortive:
      var holes: Map[Ordinal, Html.Hole] = Map()
      def capture(ordinal: Ordinal, hole: Html.Hole) = holes = holes.updated(ordinal, hole)

      val html: Html = Html.parse(Iterator(parts.mkString("\u0000").tt), whatwg.generic, capture(_, _))

      println(html)
      val iterator = holes.to(List).sortBy(_(0)).map(_(1)).to(Iterator)
      var idx: Int = -1

      def checkText(array: Expr[Array[Any]], pattern: Html.Textual, scrutinee: Expr[Html.Textual])
      : Expr[Boolean] =

          '{  ${Expr(pattern.text)} == $scrutinee.text  }

      def checkComment(array: Expr[Array[Any]], pattern: Html.Comment, scrutinee: Expr[Html.Comment])
      : Expr[Boolean] =

          '{  ${Expr(pattern.text)} == $scrutinee.text  }

      def checkFragment(array: Expr[Array[Any]], pattern: Html.Fragment, scrutinee: Expr[Html.Fragment])
      : Expr[Boolean] =

          val children = '{$scrutinee.nodes}

          def elements(index: Int)(expr: Expr[Boolean]): Expr[Boolean] =
            if index == pattern.nodes.length then expr else
              val expr2 =
                recur(array, pattern.nodes(index), '{$children(${Expr(index)})}, '{true})

              elements(index + 1)('{$expr && $expr2})


          elements(0):
            '{  $scrutinee.nodes.length == ${Expr(pattern.nodes.length)} }

      def checkElement(array: Expr[Array[Any]], pattern: Html.Element, scrutinee: Expr[Html.Element])
      : Expr[Boolean] =

          val children = '{$scrutinee.children}

          def elements(index: Int)(expr: Expr[Boolean]): Expr[Boolean] =
            if index == pattern.children.length then expr else
              val expr2 =
                recur(array, pattern.children(index), '{$children(${Expr(index)})}, '{true})

              elements(index + 1)('{$expr && $expr2})

          elements(0):
            '{  ${Expr(pattern.label)} == $scrutinee.label
                && $scrutinee.children.length == ${Expr(pattern.children.length)} }

      def recur(array: Expr[Array[Any]], pattern: Html, scrutinee: Expr[Html], expr: Expr[Boolean])
      : Expr[Boolean] =

          println(s"pattern: $pattern")
          pattern match
            case Html.Textual("\u0000") =>
              println(s"textual extraction: $pattern")
              println("extraction of "+iterator.next())
              idx += 1
              '{  $expr && { $array(${Expr(idx)}) = $scrutinee; true }  }

            case textual@Html.Textual(text) =>
              println(s"textual: $pattern")
              val checked = checkText(array, textual, '{$scrutinee.asInstanceOf[Html.Textual]})
              '{  $expr && $scrutinee.isInstanceOf[Html.Textual] && $checked  }

            case comment@Html.Comment(text) =>
              println(s"comment: $pattern")
              val checked = checkComment(array, comment, '{$scrutinee.asInstanceOf[Html.Comment]})
              '{  $expr && $scrutinee.isInstanceOf[Html.Comment] && $checked  }

            case Html.Element("\u0000", _, _, _) =>
              println("extraction of "+iterator.next())
              idx += 1
              '{  $expr && { $array(${Expr(idx)}) = $scrutinee; true }  }

            case element: Html.Element =>
              println(s"element: $pattern")
              def checked = checkElement(array, element, '{$scrutinee.asInstanceOf[Html.Element]})
              '{  $expr && $scrutinee.isInstanceOf[Html.Element] && $checked  }

            case fragment@Html.Fragment(nodes*) =>
              println(s"fragment: $pattern")
              val checked = checkFragment(array, fragment, '{$scrutinee.asInstanceOf[Html.Fragment]})
              '{  $expr && $scrutinee.isInstanceOf[Html.Fragment] && $checked  }

      Type.of[(Html, Html)].absolve match
        case '[result] =>
          '{
              val extracts = new Array[Any](${Expr(holes.size)})
              extracts(0) = Html.Textual("zero")
              extracts(1) = Html.Textual("one")
              val matches: Boolean = ${recur('extracts, html, scrutinee, '{true})}
              if matches then Some(Tuple.fromArray(extracts).asInstanceOf[result]) else None  }



  def interpolator[parts <: Tuple: Type](insertions0: Expr[Seq[Any]]): Macro[Html] =
    import quotes.reflect.*
    import doms.whatwg
    import Html.Hole

    def recur[tuple: Type](strings: List[String]): List[String] = Type.of[tuple] match
      case '[head *: tail] => recur[tail](TypeRepr.of[head].literal[String].vouch :: strings)
      case _               => strings

    val parts = recur[parts](Nil)

    val insertions: Seq[Expr[Any]] = insertions0.absolve match
      case Varargs(insertions) => insertions

    abortive:
      var holes: Map[Ordinal, Html.Hole] = Map()
      def capture(ordinal: Ordinal, hole: Hole) = holes = holes.updated(ordinal, hole)

      val html: Html =
        Html.parse(Iterator(parts.mkString("\u0000").tt), whatwg.generic, capture(_, _))

      val iterator: Iterator[Expr[Any]] =
        holes.to(List).sortBy(_(0)).map(_(1)).zip(insertions).map: (hole, expr) =>
          expr.absolve match
            case '{ $expr: value } => hole match
              case Hole.Attribute(tag, attribute) =>
                ConstantType(StringConstant(tag.s)).asType.absolve match
                  case '[tag] => ConstantType(StringConstant(attribute.s)).asType.absolve match
                    case '[attribute] =>
                      Expr.summon[attribute is Attribute in Whatwg on (? >: tag)]
                      . orElse(Expr.summon[attribute is Attribute in Whatwg]) match
                        case Some('{ type result;
                                     $typeclass: Attribute { type Topic = result } }) =>

                          Expr.summon[(? >: value) is Attributive to result] match
                            case Some('{$attributive}) =>
                              '{$attributive.attribute(${Expr(attribute)}, $expr).let(_(1))}

                            case _ =>
                              halt(m"""${TypeRepr.of[value].show} cannot be attributed to an attribute of ${Syntax(TypeRepr.of[result]).show}""")

                        case _ =>
                          halt(m"the attribute $attribute cannot be used on the element <$tag>")

              case Hole.Node(tag) =>
                ConstantType(StringConstant(tag.s)).asType.absolve match
                  case '[tag] => Expr.summon[(? >: value) is Renderable in (? >: tag)] match
                    case Some('{$renderable: Renderable}) =>
                      '{$renderable.render($expr)}

                    case _ =>
                      Expr.summon[(? >: value) is Showable] match
                        case Some('{$showable: Showable}) =>
                          '{Html.Textual($showable.text($expr))}

                        case _ =>
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

      def serialize(html: Html): Seq[Expr[Node]] = html match
        case Html.Fragment(children*) => children.flatMap(serialize(_))
        case Html.Element(label, attributes, children, foreign) =>
          val exprs = attributes.to(List).map: (key, value) =>
            '{  (${Expr(key)},
                 ${  if value == "\u0000".tt then iterator.next().asExprOf[Optional[Text]]
                     else if value == Unset then '{Unset}
                     else Expr[Text](value.asInstanceOf[Text])  })  }
            . asExprOf[(Text, Optional[Text])]

          val map = '{Map(${Expr.ofList(exprs)}*)}
          val elements = '{IArray(${Expr.ofList(children.flatMap(serialize(_)))}*)}

          List('{Html.Element(${Expr(label)}, $map, $elements, ${Expr(foreign)})})

        case Html.Comment(text) =>
          val parts = text.s.split("\u0000").nn.map(_.nn).to(List)

          def recur(parts: List[String], expr: Expr[String]): Expr[String] = parts match
            case Nil => expr
            case head :: tail =>
              recur(tail, '{$expr+${iterator.next().asExprOf[Text]}+${Expr(head)}})

          val content = recur(parts.tail, Expr(parts.head))

          List('{Html.Comment($content.tt)})

        case Html.Textual("\u0000") =>
          List(iterator.next().asExprOf[Node])

        case Html.Textual(text) =>
          val parts = text.s.split("\u0000").nn.map(_.nn).to(List)

          def recur(parts: List[String], expr: Expr[String]): Expr[String] = parts match
            case Nil => expr
            case head :: tail =>
              recur(tail, '{$expr+${iterator.next().asExprOf[Text]}+${Expr(head)}})

          val content = recur(parts.tail, Expr(parts.head))

          List('{Html.Textual($content.tt)})

      def resultType(html: Html): Set[String] = html match
        case Html.Textual(_)          => Set("#text")
        case Html.Element(tag, _, _, _) => Set(tag.s)
        case Html.Fragment(values*)   => values.to(Set).flatMap(resultType(_))
        case Html.Comment(_)          => Set()

      resultType(html)
      . map { label => ConstantType(StringConstant(label)) }
      . foldLeft(TypeRepr.of[Nothing]) { (left, right) => OrType(left, right) }
      . asType
      . absolve match
          case '[type topic <: Label; topic] =>
            '{
                ${  serialize(html).absolve match
                      case List(one: Expr[?]) => one.asExprOf[Html]
                      case many               => '{Html.Fragment(${Expr.ofList(many)}*)}  }
                . of[topic]  }


  def attributes[result: Type, thisType <: Tag to result: Type]
       (tag: Expr[Tag], attributes0: Expr[Seq[(String, Any)]])
  : Macro[result] =
      import quotes.reflect.*

      val args = attributes0.absolve match
        case Varargs(args) => args

      val attributes: Seq[Expr[Optional[(Text, Optional[Text])]]] =
        Type.of[thisType].absolve match
          case '[ type topic <: Label;
                  type form;
                  Tag { type Topic = topic; type Form = form } ] => args.map: arg =>
            arg.absolve match
              case '{ ($key, $value: value) } =>
                TypeRepr.of[topic].literal[String].let: topic =>
                  key.asTerm match
                    case Literal(StringConstant(key)) =>
                      if key == "" then halt(m"Empty key")
                      else ConstantType(StringConstant(key)).asType.absolve match
                        case '[type key <: Label; key] =>
                          Expr.summon[key is Attribute in form on (? >: topic)]
                          . orElse(Expr.summon[key is Attribute in form]) match
                            case Some('{ type result; $expr: Attribute { type Topic = result } }) =>
                              Expr.summon[(? >: value) is Attributive to result] match
                                case Some('{ $converter: Attributive }) =>
                                  '{$converter.attribute(${Expr(key.tt)}, $value)}

                                case _ =>
                                  halt(m"there is no converter for ${TypeRepr.of[result].show} attributes")

                            case _ =>
                              halt(m"the attribute $key cannot be used on the element <$topic>")

                    case _ =>
                      halt(m"unable to determine attribute key type")
                . or(halt(m"unexpected type"))

      '{$tag.node(${Expr.ofList(attributes)}.compact.to(Map))}.asExprOf[result]
