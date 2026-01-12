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
┃    Soundness, version 0.50.0.                                                                    ┃
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
package xylophone

import language.dynamics

import anticipation.*
import contextual.*
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

private given realm: Realm = realm"xylophone"

object Xylophone:
  def extractor[parts <: Tuple: Type](scrutinee: Expr[Xml]): Macro[Extrapolation[Xml]] =
    import quotes.reflect.*

    def recur[tuple: Type](strings: List[String]): List[String] = Type.of[tuple] match
      case '[head *: tail] => recur[tail](TypeRepr.of[head].literal[String].vouch :: strings)
      case _               => strings

    val parts = recur[parts](Nil)

    def intersect(parts: List[String], repr: TypeRepr = TypeRepr.of[Nothing]): TypeRepr =
      parts match
        case head :: tail =>  intersect(tail, OrType(repr, ConstantType(StringConstant(head))))
        case Nil          =>  repr

    abortive:
      var holes: Map[Ordinal, Xml.Hole] = Map()
      def capture(ordinal: Ordinal, hole: Xml.Hole) = holes = holes.updated(ordinal, hole)

      given XmlSchema = XmlSchema.Freeform

      val generic: Tag = Tag.root(Set())
      val xml: Xml = Xml.parse(Iterator(parts.mkString("\u0000").tt), generic, capture(_, _))

      val holes2 = holes.to(List).sortBy(_(0)).map(_(1))
      val iterator = holes2.to(Iterator)
      var idx: Int = -1

      var types: List[TypeRepr] = Nil

      def checkText(array: Expr[Array[Any]], pattern: TextNode, scrutinee: Expr[TextNode])
      : Expr[Boolean] =
          '{  ${Expr(pattern.text)} == $scrutinee.text  }

      def checkComment(array: Expr[Array[Any]], pattern: Comment, scrutinee: Expr[Comment])
      : Expr[Boolean] =

          '{  ${Expr(pattern.text)} == $scrutinee.text  }

      def checkCdata(array: Expr[Array[Any]], pattern: Cdata, scrutinee: Expr[Cdata])
      : Expr[Boolean] =

          '{  ${Expr(pattern.text)} == $scrutinee.text  }

      def checkPi
           (array:     Expr[Array[Any]],
            pattern:   ProcessingInstruction,
            scrutinee: Expr[ProcessingInstruction])
      : Expr[Boolean] =

          '{  ${Expr(pattern.target)} == $scrutinee.target
              && ${Expr(pattern.data)} == $scrutinee.data  }

      def checkHeader(array: Expr[Array[Any]], pattern: Header, scrutinee: Expr[Header])
      : Expr[Boolean] =

          '{  ${Expr(pattern.version)} == $scrutinee.version  } // FIXME: Check encoding and standalone too

      def checkFragment(array: Expr[Array[Any]], pattern: Fragment, scrutinee: Expr[Fragment])
      : Expr[Boolean] =

          val children = '{$scrutinee.nodes}

          def elements(index: Int)(expr: Expr[Boolean]): Expr[Boolean] =
            if index == pattern.nodes.length then expr else
              val expr2 =
                descend(array, pattern.nodes(index), '{$children(${Expr(index)})}, '{true})

              elements(index + 1)('{$expr && $expr2})


          elements(0):
            '{  $scrutinee.nodes.length == ${Expr(pattern.nodes.length)} }

      def checkElement(array: Expr[Array[Any]], pattern: Element, scrutinee: Expr[Element])
      : Expr[Boolean] =

          def attributes(todo: List[Text])(expr: Expr[Boolean]): Expr[Boolean] = todo match
            case Nil => expr
            case "\u0000" :: tail =>
              idx += 1
              types ::= TypeRepr.of[Map[Text, Text]]
              iterator.next()
              val others = Expr.ofList(pattern.attributes.keys.to(List).map(Expr(_)))
              '{ $expr && { $array(${Expr(idx)}) = ${scrutinee}.attributes -- $others; true } }

            case head :: tail =>
              attributes(tail):
                val boolean: Expr[Boolean] = pattern.attributes(head).s.absolve match
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

      def descend(array: Expr[Array[Any]], pattern: Xml, scrutinee: Expr[Xml], expr: Expr[Boolean])
      : Expr[Boolean] =

          pattern match
            case Comment("\u0000") =>
              idx += 1
              iterator.next()
              types ::= TypeRepr.of[Text]

              '{  $expr
                  && $scrutinee.isInstanceOf[Comment]
                  && { $array(${Expr(idx)}) = $scrutinee.asInstanceOf[Comment].text; true }  }

            case ProcessingInstruction("\u0000", t"") =>
              idx += 1
              iterator.next()
              types ::= TypeRepr.of[ProcessingInstruction]

              '{  $expr
                  && $scrutinee.isInstanceOf[ProcessingInstruction]
                  && { $array(${Expr(idx)}) = $scrutinee.asInstanceOf[ProcessingInstruction].data
                       true }  }

            case Cdata("\u0000") =>
              idx += 1
              iterator.next()
              types ::= TypeRepr.of[Cdata]

              '{  $expr
                  && $scrutinee.isInstanceOf[Cdata]
                  && { $array(${Expr(idx)}) = $scrutinee.asInstanceOf[Cdata].text; true }  }

            case TextNode("\u0000") =>
              idx += 1
              iterator.next() match
                case Xml.Hole.Node(label) =>
                  types ::= TypeRepr.of[Node]

                case _ =>
                  panic(m"unexpected hole type")

              '{  $expr && { $array(${Expr(idx)}) = $scrutinee; true }  }

            case textual@TextNode(text) =>
              val checked = checkText(array, textual, '{$scrutinee.asInstanceOf[TextNode]})
              '{  $expr && $scrutinee.isInstanceOf[TextNode] && $checked  }

            case comment@Comment(text) =>
              if text.contains("\u0000")
              then halt(m"""only the entire comment text can be matched; write the extractor as
                            ${t"<!--$$text-->"}""")
              val checked = checkComment(array, comment, '{$scrutinee.asInstanceOf[Comment]})
              '{  $expr && $scrutinee.isInstanceOf[Comment] && $checked  }

            case cdata@Cdata(content) =>
              if content.contains("\u0000")
              then halt(m"""only the entire CDATA content can be matched; write the extractor as
                            ${t"<![CDATA[$$text]]>"}""")
              val checked = checkCdata(array, cdata, '{$scrutinee.asInstanceOf[Cdata]})
              '{  $expr && $scrutinee.isInstanceOf[Cdata] && $checked  }

            case pi@ProcessingInstruction(target, data) =>
              if data.contains("\u0000") || target.contains("\u0000")
              then halt(m"""only the entire data part of a processing instruction can be matched""")
              val checked = checkPi(array, pi, '{$scrutinee.asInstanceOf[ProcessingInstruction]})
              '{  $expr && $scrutinee.isInstanceOf[ProcessingInstruction] && $checked  }

            case Element("\u0000", _, _) =>
              idx += 1
              iterator.next() match
                case Xml.Hole.Element(label) =>
                  types ::= TypeRepr.of[Element]
                case _ =>
                  halt(m"unexpected hole type")

              '{  $expr && { $array(${Expr(idx)}) = $scrutinee; true }  }

            case element: Element =>
              def checked = checkElement(array, element, '{$scrutinee.asInstanceOf[Element]})
              '{  $expr && $scrutinee.isInstanceOf[Element] && $checked  }

            case header: Header =>
              def checked = checkHeader(array, header, '{$scrutinee.asInstanceOf[Header]})
              '{  $expr && $scrutinee.isInstanceOf[Header] && $checked  }

            case fragment@Fragment(nodes*) =>
              val checked = checkFragment(array, fragment, '{$scrutinee.asInstanceOf[Fragment]})
              '{  $expr && $scrutinee.isInstanceOf[Fragment] && $checked  }


      val result: Expr[Extrapolation[Xml]] =
        '{  val extracts = new Array[Any](${Expr(holes.size)})
            val matches: Boolean = ${descend('extracts, xml, scrutinee, '{true})}
            ${  if holes.size == 0 then '{matches}
                else if holes.size == 1
                then '{if !matches then None else Some(extracts(0).asInstanceOf[Xml])}
                else '{if !matches then None else Some(Tuple.fromArray(extracts))} }  }

      types.length match
        case 0 =>
          '{$result.asInstanceOf[Boolean]}

        case 1 => types.head.asType.absolve match
          case '[type result <: Xml; result] =>
            '{$result.asInstanceOf[Option[result]]}

        case _ =>
          AppliedType(defn.TupleClass(types.length).info.typeSymbol.typeRef, types.reverse)
          . asType
          . absolve match
              case '[type result <: Tuple; result] =>
                '{$result.asInstanceOf[Option[result]]}


  def interpolator[parts <: Tuple: Type](insertions0: Expr[Seq[Any]]): Macro[Xml] =
    import quotes.reflect.*
    import Xml.Hole

    def recur[tuple: Type](strings: List[String]): List[String] = Type.of[tuple] match
      case '[head *: tail] => recur[tail](TypeRepr.of[head].literal[String].vouch :: strings)
      case _               => strings

    val parts = recur[parts](Nil)

    val insertions: Seq[Expr[Any]] = insertions0.absolve match
      case Varargs(insertions) => insertions

    abortive:
      var holes: Map[Ordinal, Xml.Hole] = Map()
      def capture(ordinal: Ordinal, hole: Hole) = holes = holes.updated(ordinal, hole)

      given XmlSchema = XmlSchema.Freeform

      val xml: Xml =
        Xml.parse(Iterator(parts.mkString("\u0000").tt), XmlSchema.generic, capture(_, _))

      val iterator: Iterator[Expr[Any]] =
        holes.to(List).sortBy(_(0)).map(_(1)).zip(insertions).map: (hole, expr) =>
          expr.absolve match
            case '{ $expr: value } => hole match
              case Hole.Attribute(tag, attribute) =>
                ConstantType(StringConstant(tag.s)).asType.absolve match
                  case '[tag] => ConstantType(StringConstant(attribute.s)).asType.absolve match
                    case '[attribute] =>
                      Expr.summon[attribute is Xml.XmlAttribute] match
                        case Some('{ type result;
                                     $typeclass: Xml.XmlAttribute { type Topic = result } }) =>

                          Expr.summon[(? >: value) is Attributive to result] match
                            case Some('{$attributive}) =>
                              '{$attributive.attribute(${Expr(attribute)}, $expr).let(_(1))}

                            case _ =>
                              halt(m"""${TypeRepr.of[value].show} cannot be attributed to an attribute of ${Syntax(TypeRepr.of[result]).show}""")

                        case _ =>
                          expr match
                            case '{$expr: Text} =>
                              expr

                            case _ =>
                              halt(m"the attribute $attribute cannot be used on the element <$tag>")

              case Hole.Element(tag) =>
                ConstantType(StringConstant(tag.s)).asType.absolve match
                  case '[tag] => Expr.summon[(? >: value) is Encodable in Xml] match
                    case Some('{$encodable: Encodable}) =>
                      '{$encodable.encode($expr)}

                    case _ =>
                      halt(m"""a value of ${TypeRepr.of[value].show} is not encodable inside a
                               <$tag> element""")

              case Hole.Node(tag) =>
                ConstantType(StringConstant(tag.s)).asType.absolve match
                  case '[tag] => Expr.summon[(? >: value) is Renderable in (? >: tag)] match
                    case Some('{$renderable: Renderable}) =>
                      '{$renderable.render($expr)}

                    case _ =>
                      Expr.summon[(? >: value) is Showable] match
                        case Some('{$showable: Showable}) =>
                          '{TextNode($showable.text($expr))}

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
                case '[Map[Text, Text]] =>
                  expr
                case _ =>
                  halt(m"only a ${TypeRepr.of[Map[Text, Text]].show} can be applied in a tag body")
        . iterator

      def serialize(xml: Xml): Seq[Expr[Node]] = xml match
        case Fragment(children*) => children.flatMap(serialize(_))
        case Header(version, encoding, standalone) =>
          val encoding2: Expr[Optional[Text]] =
            if encoding == Unset then '{Unset} else Expr(encoding.asInstanceOf[Text])

          val standalone2: Expr[Optional[Boolean]] =
            if standalone == Unset then '{Unset} else Expr(encoding.asInstanceOf[Boolean])

          List('{Header(${Expr(version)}, $encoding2, $standalone2)})
        case Element(label, attributes, children) =>
          val exprs = attributes.to(List).map: (key, value) =>
            '{  (${Expr(key)},
                 ${  if value == "\u0000".tt then iterator.next().asExprOf[Text]
                     else Expr[Text](value)  })  }
            . asExprOf[(Text, Text)]

          val map = '{Map(${Expr.ofList(exprs)}*)}
          val elements = '{IArray(${Expr.ofList(children.flatMap(serialize(_)))}*)}

          List('{Element(${Expr(label)}, $map, $elements)})

        case Comment(text) =>
          val parts = text.s.split("\u0000").nn.map(_.nn).to(List)

          def recur(parts: List[String], expr: Expr[String]): Expr[String] = parts match
            case Nil => expr
            case head :: tail =>
              recur(tail, '{$expr+${iterator.next().asExprOf[Text]}+${Expr(head)}})

          val content = recur(parts.tail, Expr(parts.head))

          List('{Comment($content.tt)})

        case Cdata(text) =>
          val parts = text.s.split("\u0000").nn.map(_.nn).to(List)

          def recur(parts: List[String], expr: Expr[String]): Expr[String] = parts match
            case Nil => expr
            case head :: tail =>
              recur(tail, '{$expr+${iterator.next().asExprOf[Text]}+${Expr(head)}})

          val content = recur(parts.tail, Expr(parts.head))

          List('{Cdata($content.tt)})

        case ProcessingInstruction(target, data0) =>
          val parts = data0.s.split("\u0000").nn.map(_.nn).to(List)

          def recur(parts: List[String], expr: Expr[String]): Expr[String] = parts match
            case Nil => expr
            case head :: tail =>
              recur(tail, '{$expr+${iterator.next().asExprOf[Text]}+${Expr(head)}})

          val data = recur(parts.tail, Expr(parts.head))

          List('{ProcessingInstruction(${Expr(target)}, $data.tt)})

        case TextNode("\u0000") =>
          List(iterator.next().asExprOf[Node])

        case TextNode(text) =>
          val parts = text.s.split("\u0000").nn.map(_.nn).to(List)

          def recur(parts: List[String], expr: Expr[String]): Expr[String] = parts match
            case Nil => expr
            case head :: tail =>
              recur(tail, '{$expr+${iterator.next().asExprOf[Text]}+${Expr(head)}})

          val content = recur(parts.tail, Expr(parts.head))

          List('{TextNode($content.tt)})

      def resultType(xml: Xml): Set[String] = xml match
        case TextNode(_)        =>  Set("#text")
        case Element(tag, _, _) =>  Set(tag.s)
        case Fragment(values*)  =>  values.to(Set).flatMap(resultType(_))
        case _                  =>  Set()

      resultType(xml)
      . map { label => ConstantType(StringConstant(label)) }
      . foldLeft(TypeRepr.of[Nothing]) { (left, right) => OrType(left, right) }
      . asType
      . absolve match
          case '[type topic <: Label; topic] =>
            '{
                ${  serialize(xml).absolve match
                      case List(one: Expr[?]) => one.asExprOf[Xml]
                      case many               => '{Fragment(${Expr.ofList(many)}*)}  }
                . of[topic]  }


  def attributes[result: Type, thisType <: Tag to result: Type]
       (tag: Expr[Tag], attributes0: Expr[Seq[(String, Any)]])
  : Macro[result] =
      import quotes.reflect.*

      val args = attributes0.absolve match
        case Varargs(args) => args

      val attributes: Seq[Expr[Optional[(Text, Text)]]] =
        Type.of[thisType].absolve match
          case '[ type topic <: Label;
                  type form;
                  Tag { type Topic = topic; type Form = form } ] => args.map: arg =>
            arg.absolve match
              case '{ ($key, $value: value) } =>
                TypeRepr.of[topic].literal[String].let: topic =>
                  key.asTerm match
                    case Literal(StringConstant(key)) =>
                      if key == "" then panic(m"Empty key")
                      else ConstantType(StringConstant(key)).asType.absolve match
                        case '[type key <: Label; key] =>
                          Expr.summon[key is Xml.XmlAttribute in form on (? >: topic)]
                          . orElse(Expr.summon[key is Xml.XmlAttribute in form]) match
                            case Some('{ type result; $expr: Xml.XmlAttribute { type Topic = result } }) =>
                              Expr.summon[(? >: value) is Attributive to result] match
                                case Some('{ $converter: Attributive }) =>
                                  '{$converter.attribute(${Expr(key.tt)}, $value)}

                                case _ =>
                                  halt(m"""$key has attribute type ${TypeRepr.of[result].show},
                                           but ${TypeRepr.of[value].show} cannot be attributed as
                                           a ${TypeRepr.of[result].show} without a contextual
                                           instance of
                                           ${TypeRepr.of[value is Attributive to result].show}""")

                            case _ =>
                              halt(m"the attribute $key cannot be used on the element <$topic>")

                    case _ =>
                      halt(m"unable to determine attribute key type")
                . or(halt(m"unexpected type"))

      '{$tag.node(${Expr.ofList(attributes)}.compact.to(Map))}.asExprOf[result]
