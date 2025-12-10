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
import gossamer.*
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

  def extractor[parts <: Tuple: Type](scrutinee: Expr[Html]): Macro[Boolean | Option[Any]] =
    import quotes.reflect.*
    import doms.whatwg

    def recur[tuple: Type](strings: List[String]): List[String] = Type.of[tuple] match
      case '[head *: tail] => recur[tail](TypeRepr.of[head].literal[String].vouch :: strings)
      case _               => strings

    val parts = recur[parts](Nil)

    def intersect(parts: List[String], repr: TypeRepr = TypeRepr.of[Nothing]): TypeRepr =
      parts match
        case head :: tail =>  intersect(tail, OrType(repr, ConstantType(StringConstant(head))))
        case Nil          =>  repr

    abortive:
      var holes: Map[Ordinal, Html.Hole] = Map()
      def capture(ordinal: Ordinal, hole: Html.Hole) = holes = holes.updated(ordinal, hole)

      val html: Html = Html.parse(Iterator(parts.mkString("\u0000").tt), whatwg.generic, capture(_, _))

      val holes2 = holes.to(List).sortBy(_(0)).map(_(1))
      val iterator = holes2.to(Iterator)
      var idx: Int = -1

      var types: List[TypeRepr] = Nil

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
                descend(array, pattern.nodes(index), '{$children(${Expr(index)})}, '{true})

              elements(index + 1)('{$expr && $expr2})


          elements(0):
            '{  $scrutinee.nodes.length == ${Expr(pattern.nodes.length)} }

      def checkElement(array: Expr[Array[Any]], pattern: Html.Element, scrutinee: Expr[Html.Element])
      : Expr[Boolean] =

          def attributes(todo: List[Text])(expr: Expr[Boolean]): Expr[Boolean] = todo match
            case Nil => expr
            case "\u0000" :: tail =>
              idx += 1
              types ::= TypeRepr.of[Map[Text, Optional[Text]]]
              iterator.next()
              val others = Expr.ofList(pattern.attributes.keys.to(List).map(Expr(_)))
              '{ $expr && { $array(${Expr(idx)}) = ${scrutinee}.attributes -- $others; true } }

            case head :: tail =>
              attributes(tail):
                val boolean: Expr[Boolean] = pattern.attributes(head).let(_.s).absolve match
                  case Unset      => '{$scrutinee.attributes(${Expr(head)}) == Unset}
                  case "\u0000"   =>
                    idx += 1
                    types ::= TypeRepr.of[Text]
                    iterator.next()
                    '{ $array(${Expr(idx)}) = $scrutinee.attributes(${Expr(head)}); true }

                  case text: Text =>
                    '{ $scrutinee.attributes(${Expr(head)}) == ${Expr(text)} }

                '{ $expr && $boolean }

          val attributesChecked = attributes(pattern.attributes.to(List).map(_(0)))('{true})

          val children = '{$scrutinee.children}

          def elements(index: Int)(expr: Expr[Boolean]): Expr[Boolean] =
            if index == pattern.children.length then expr else
              val expr2 =
                descend(array, pattern.children(index), '{$children(${Expr(index)})}, '{true})

              elements(index + 1)('{$expr && $expr2})

          val elementsChecked = elements(0):
            '{  ${Expr(pattern.label)} == $scrutinee.label
                && $scrutinee.children.length == ${Expr(pattern.children.length)} }

          '{ $attributesChecked && $elementsChecked }

      def descend(array: Expr[Array[Any]], pattern: Html, scrutinee: Expr[Html], expr: Expr[Boolean])
      : Expr[Boolean] =

          pattern match
            case Html.Comment("\u0000") =>
              idx += 1
              iterator.next()
              types ::= TypeRepr.of[Text]

              '{  $expr
                  && $scrutinee.isInstanceOf[Html.Comment]
                  && { $array(${Expr(idx)}) = $scrutinee.asInstanceOf[Html.Comment].text; true }  }

            case Html.Textual("\u0000") =>
              idx += 1
              iterator.next() match
                case Html.Hole.Node(label) =>
                  types ::= whatwg.elements(label).lay(TypeRepr.of[Html]): tag =>
                    intersect(tag.admissible.map(_.s).to(List)).asType.absolve match
                      case '[type children <: Label; children] => TypeRepr.of[Html of children]

                case _ =>
                  panic(m"unexpected hole type")

              '{  $expr && { $array(${Expr(idx)}) = $scrutinee; true }  }

            case textual@Html.Textual(text) =>
              val checked = checkText(array, textual, '{$scrutinee.asInstanceOf[Html.Textual]})
              '{  $expr && $scrutinee.isInstanceOf[Html.Textual] && $checked  }

            case comment@Html.Comment(text) =>
              if text.contains("\u0000")
              then halt(m"""only the entire comment text can be matched; write the extractor as
                            ${t"<!--$$text-->"}""")
              val checked = checkComment(array, comment, '{$scrutinee.asInstanceOf[Html.Comment]})
              '{  $expr && $scrutinee.isInstanceOf[Html.Comment] && $checked  }

            case Html.Element("\u0000", _, _, _) =>
              idx += 1
              types ::= TypeRepr.of[Html]
              iterator.next()
              '{  $expr && { $array(${Expr(idx)}) = $scrutinee; true }  }

            case element: Html.Element =>
              def checked = checkElement(array, element, '{$scrutinee.asInstanceOf[Html.Element]})
              '{  $expr && $scrutinee.isInstanceOf[Html.Element] && $checked  }

            case fragment@Html.Fragment(nodes*) =>
              val checked = checkFragment(array, fragment, '{$scrutinee.asInstanceOf[Html.Fragment]})
              '{  $expr && $scrutinee.isInstanceOf[Html.Fragment] && $checked  }


      val result: Expr[Boolean | Option[Any]] =
        '{  val extracts = new Array[Any](${Expr(holes.size)})
            val matches: Boolean = ${descend('extracts, html, scrutinee, '{true})}
            ${  if holes.size == 0 then '{matches}
                else if holes.size == 1 then '{if !matches then None else Some(extracts(0))}
                else '{if !matches then None else Some(Tuple.fromArray(extracts))} }  }

      types.length match
        case 0 => '{$result.asInstanceOf[Boolean]}
        case 1 => types.head.asType.absolve match
          case '[result] => '{$result.asInstanceOf[Option[result]]}
        case _ =>
          AppliedType(defn.TupleClass(types.length).info.typeSymbol.typeRef, types.reverse)
          . asType
          . absolve match
              case '[result] => '{$result.asInstanceOf[Option[result]]}



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
