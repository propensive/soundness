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
┃    Soundness, version 0.64.0.                                                                    ┃
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

import honeycomb.internal.Attributes

import scala.collection.immutable.Seq

import scala.language.dynamics

import scala.collection.immutable.{List, Nil, ::}
import scala.quoted.*

import anticipation.*
import contextual.*
import contingency.*
import denominative.*
import fulminate.*
import gigantism.*

import gossamer.*
import prepositional.*
import rudiments.*
import spectacular.*
import stenography.*
import symbolism.*
import vacuous.*
import rudiments.sortingAlgorithms.timsort

object Honeycomb:
  def extractor[parts <: Tuple: Type](scrutinee: Expr[Html]): Macro[Extrapolation[Html]] =
    import quotes.reflect.*
    import doms.html.whatwg

    def recur[tuple: Type](strings: List[String]): List[String] = Type.of[tuple] match
      case '[head *: tail] => recur[tail](TypeRepr.of[head].literal[String].or(halt(m"an interpolator's parts are string-literal types")) :: strings)
      case _               => strings

    val parts = recur[parts](Nil)

    def intersect(parts: List[String], repr: TypeRepr = TypeRepr.of[Nothing]): TypeRepr =
      parts match
        case head :: tail => intersect(tail, OrType(repr, ConstantType(StringConstant(head))))
        case Nil          => repr

    abortive:
      var holes: Map[Ordinal, Html.Hole] = Map()
      def capture(ordinal: Ordinal, hole: Html.Hole) = holes = holes.define(ordinal, hole)

      val html: Html =
        Html.parse(Iterator(parts.mkString("\u0000").tt), whatwg.generic, capture(_, _))

      val holes2 = holes.to[proscenium.List].order(_(0)).map(_(1))
      // Deliberate stdlib opt-out: the macro walks the holes with a stdlib `Iterator`.
      val iterator = holes2.stdlib.iterator
      var index: Int = -1

      var types: List[TypeRepr] = Nil

      def checkText(array: Expr[scala.Array[Any]], pattern: TextNode, scrutinee: Expr[TextNode])
      :   Expr[Boolean] =

        '{${Expr(pattern.text)} == $scrutinee.text}

      def checkComment(array: Expr[scala.Array[Any]], pattern: Comment, scrutinee: Expr[Comment])
      :   Expr[Boolean] =

        '{${Expr(pattern.text)} == $scrutinee.text}

      def checkFragment(array: Expr[scala.Array[Any]], pattern: Fragment, scrutinee: Expr[Fragment])
      :   Expr[Boolean] =

        val children = '{$scrutinee.nodes}

        def elements(index: Int)(expr: Expr[Boolean]): Expr[Boolean] =
          if index == pattern.nodes.length then expr else
            val expr2 =
              descend(array, pattern.nodes(index), '{$children(${Expr(index)})}, '{true})

            elements(index + 1)('{$expr && $expr2})

        elements(0):
          '{$scrutinee.nodes.length == ${Expr(pattern.nodes.length)}}

      def checkElement(array: Expr[scala.Array[Any]], pattern: Element, scrutinee: Expr[Element])
      :   Expr[Boolean] =

        def attributes(todo: List[Text])(expr: Expr[Boolean]): Expr[Boolean] = todo match
          case Nil => expr

          case "\u0000" :: tail =>
            index += 1
            types = TypeRepr.of[Map[Text, Optional[Text]]] :: types
            iterator.next()
            val others = Expr.ofList(pattern.attributes.keys.to(List).map(Expr(_)))

            ' {
                $expr &&
                  { $array(${Expr(index)}) = (${scrutinee}.attributes -- $others).toMap; true }
              }

          case head :: tail =>
            attributes(tail):
              val boolean: Expr[Boolean] = pattern.attributes(head).let(_.s).absolve match
                case Unset      => '{$scrutinee.attributes(${Expr(head)}) == Unset}

                case "\u0000" =>
                  index += 1
                  types = TypeRepr.of[Text] :: types
                  iterator.next()
                  '{$array(${Expr(index)}) = $scrutinee.attributes(${Expr(head)}); true}

                case text: Text =>
                  '{$scrutinee.attributes(${Expr(head)}) == ${Expr(text)}}

              '{$expr && $boolean}

        val attributesChecked = attributes(pattern.attributes.toList.map(_(0)))('{true})

        // The children access is quoted in one piece: splicing a `val children` Expr gives
        // the frozen array a reach capture (`children*.rd`) that cannot subsume into the
        // read shim's `any.rd` receiver.
        def elements(index: Int)(expr: Expr[Boolean]): Expr[Boolean] =
          if index == pattern.children.length then expr else
            val expr2 =
              descend
                (array, pattern.children.readUnchecked(index), '{$scrutinee.children.readUnchecked(${Expr(index)})}, '{true})

            elements(index + 1)('{$expr && $expr2})

        val elementsChecked = elements(0):
          ' {
              ${Expr(pattern.label)} == $scrutinee.label &&
                $scrutinee.children.length == ${Expr(pattern.children.length)}
            }

        '{$attributesChecked && $elementsChecked}

      def descend
        ( array: Expr[scala.Array[Any]], pattern: Html, scrutinee: Expr[Html], expr: Expr[Boolean] )
      :   Expr[Boolean] =

        pattern match
          case Comment("\u0000") =>
            index += 1
            iterator.next()
            types = TypeRepr.of[Text] :: types

            ' {
                $expr &&
                  $scrutinee.isInstanceOf[Comment] &&
                  { $array(${Expr(index)}) = $scrutinee.asInstanceOf[Comment].text; true }
              }

          case TextNode("\u0000") =>
            index += 1

            iterator.next() match
              case Html.Hole.Node(label) =>
                val nodeType = whatwg.elements(label).lay(TypeRepr.of[Node]): tag =>
                  // Deliberate stdlib opt-out: the macro works in the quotes API's stdlib `List`.
                  intersect(tag.admissible.stdlib.map(_.s).to(List)).asType.absolve match
                    case '[type children <: Label; children] => TypeRepr.of[Node of children]

                types = nodeType :: types

              case _ =>
                panic(m"unexpected hole type")

            '{$expr && { $array(${Expr(index)}) = $scrutinee; true }}

          case textual@TextNode(text) =>
            val checked = checkText(array, textual, '{$scrutinee.asInstanceOf[TextNode]})
            '{$expr && $scrutinee.isInstanceOf[TextNode] && $checked}

          case comment@Comment(text) =>
            if text.contains("\u0000") then halt:
              m"""
                only the entire comment text can be matched; write the extractor as
                ${t"<!--$$text-->"}
              """

            val checked = checkComment(array, comment, '{$scrutinee.asInstanceOf[Comment]})
            '{$expr && $scrutinee.isInstanceOf[Comment] && $checked}

          case Doctype(_) =>
            halt(520, m"cannot match against a document type declaration")

          case Element("\u0000", _, _, _) =>
            index += 1

            iterator.next() match
              case Html.Hole.Element(label) =>
                val elementType = whatwg.elements(label).lay(TypeRepr.of[Element]): tag =>
                  // Deliberate stdlib opt-out: the macro works in the quotes API's stdlib `List`.
                  intersect(tag.admissible.stdlib.map(_.s).to(List)).asType.absolve match
                    case '[type children <: Label; children] => TypeRepr.of[Element of children]

                types = elementType :: types

              case _ =>
                halt(m"unexpected hole type")

            '{$expr && { $array(${Expr(index)}) = $scrutinee; true }}

          case element: Element =>
            def checked = checkElement(array, element, '{$scrutinee.asInstanceOf[Element]})
            '{$expr && $scrutinee.isInstanceOf[Element] && $checked}

          case fragment@Fragment(nodes*) =>
            val checked = checkFragment(array, fragment, '{$scrutinee.asInstanceOf[Fragment]})
            '{$expr && $scrutinee.isInstanceOf[Fragment] && $checked}

      val result: Expr[Extrapolation[Html]] =
        ' {
            val extracts = new scala.Array[Any](${Expr(holes.size)})
            val matches: Boolean = ${descend('extracts, html, scrutinee, '{true})}

            $ {
                if holes.size == 0 then '{matches}
                else if holes.size == 1
                then '{if !matches then None else Some(extracts(0).asInstanceOf[Html])}
                else '{if !matches then None else Some(Tuple.fromArray(extracts))}
              }
          }

      types.length match
        case 0 =>
          '{$result.asInstanceOf[Boolean]}

        case 1 => types.head.asType.absolve match
          case '[type result <: Html; result] =>
            '{$result.asInstanceOf[Option[result]]}

        case _ =>
          AppliedType(defn.TupleClass(types.length).info.typeSymbol.typeRef, types.reverse)
          . asType
          . absolve match
            case '[type result <: Tuple; result] =>
              '{$result.asInstanceOf[Option[result]]}

  def interpolator[parts <: Tuple: Type](insertions0: Expr[Seq[Any]]): Macro[Html] =
    import quotes.reflect.*
    import doms.html.whatwg
    import Html.Hole

    def recur[tuple: Type](strings: List[String]): List[String] = Type.of[tuple] match
      case '[head *: tail] => recur[tail](TypeRepr.of[head].literal[String].or(halt(m"an interpolator's parts are string-literal types")) :: strings)
      case _               => strings

    val parts = recur[parts](Nil)

    val insertions: Seq[Expr[Any]] = insertions0.absolve match
      case Varargs(insertions) => insertions

    abortive:
      var holes: Map[Ordinal, Html.Hole] = Map()
      def capture(ordinal: Ordinal, hole: Hole) = holes = holes.define(ordinal, hole)

      val html: Html =
        Html.parse(Iterator(parts.mkString("\u0000").tt), whatwg.generic, capture(_, _))

      val iterator: Iterator[Expr[Any]] =
        holes.to[proscenium.List].order(_(0)).map(_(1)).zip(insertions).map: (hole, expr) =>
          expr.absolve match
            case '{$expr: value} => hole match
              case Hole.Attribute(tag, attribute) =>
                ConstantType(StringConstant(tag.s)).asType.absolve match
                  case '[tag] => ConstantType(StringConstant(attribute.s)).asType.absolve match
                    case '[attribute] =>
                      Expr.summon[attribute is Attribute in Whatwg on (? >: tag)]
                      . orElse(Expr.summon[attribute is Attribute in Whatwg]) match
                        case Some
                          ( ' {
                                type result
                                $typeclass: Attribute { type Topic = result }
                              } ) =>

                          Expr.summon[(? >: value) is Attributive to result] match
                            case Some('{$attributive}) =>
                              '{$attributive.attribute(${Expr(attribute)}, $expr).let(_(1))}

                            case _ =>
                              halt:
                                m"""
                                  ${TypeRepr.of[value].show} cannot be attributed to an attribute of
                                  ${Syntax(TypeRepr.of[result]).show}
                                """

                        case _ =>
                          halt(m"the attribute $attribute cannot be used on the element <$tag>")

              case Hole.Element(tag) =>
                ConstantType(StringConstant(tag.s)).asType.absolve match
                  case '[tag] => Expr.summon[(? >: value) is Renderable in (? >: tag)] match
                    case Some('{$renderable: Renderable}) =>
                      '{$renderable.render($expr)}

                    case _ => halt:
                      m"""
                        a value of ${TypeRepr.of[value].show} is not renderable inside a <$tag>
                        element
                      """

              case Hole.Node(tag) =>
                ConstantType(StringConstant(tag.s)).asType.absolve match
                  case '[tag] => Expr.summon[(? >: value) is Renderable in (? >: tag)] match
                    case Some('{$renderable: Renderable}) =>
                      '{$renderable.render($expr)}

                    case _ =>
                      Expr.summon[(? >: value) is Showable] match
                        case Some('{$showable: Showable}) =>
                          '{TextNode($showable.text($expr))}

                        case _ => halt:
                          m"""
                            a value of ${TypeRepr.of[value].show} is not renderable or showable
                            inside a <$tag> element
                          """

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

              case Hole.Tagbody =>
                // A reflection-level test rather than a quoted type pattern
                // (`case '[Map[Text, Optional[Text]]]`), which fails to unify
                // against the capture-decorated scrutinee under capture checking.
                if TypeRepr.of[value] <:< TypeRepr.of[Map[Text, Optional[Text]]] then expr else
                  halt:
                    m"""
                      only a ${TypeRepr.of[Map[Text, Optional[Text]]].show} can be applied in a tag
                      body
                    """

        // Deliberate stdlib opt-out: the macro walks the holes with a stdlib `Iterator`.
        . stdlib.iterator

      def serialize(html: Html): Seq[Expr[Node]] = html match
        case Fragment(children*) => children.flatMap(serialize(_))

        case Element(label, attributes, children, foreign) =>
          val exprs = attributes.toList.map: (key, value) =>
            ' {
                ( ${Expr(key)},
                  $ {
                      if value == "\u0000".tt then iterator.next().asExprOf[Optional[Text]]
                      else if value == Unset then '{Unset}
                      else Expr[Text](value.asInstanceOf[Text])
                    } )
              }

            . asExprOf[(Text, Optional[Text])]

          val attrs = '{Attributes(${Expr.ofList(exprs)}*)}
          // Cast-erased: the per-element `Expr` types are fresh-decorated, which an
          // outer seal cannot reach.
          val elements =
            '{Array(${Expr.ofList(children.flatMap(serialize(_)).asInstanceOf[Array[Expr[Node]]^{}].readable.toList)}*)}

          List('{Element(${Expr(label)}, $attrs, $elements, ${Expr(foreign)})})

        case Doctype(text) =>
          if text.contains(t"\u0000")
          then halt(m"cannot substitute into a document type declaration")
          else List('{Doctype(${Expr(text)})})

        case Comment(text) =>
          // Deliberate stdlib opt-out: `recur` below walks the stdlib `List` the quotes API uses.
          val parts = text.cut(t"\u0000").stdlib.map(_.s)

          def recur(parts: List[String], expr: Expr[String]): Expr[String] = parts match
            case Nil => expr

            case head :: tail =>
              recur(tail, '{$expr+${iterator.next().asExprOf[Text]}+${Expr(head)}})

          val content = recur(parts.tail, Expr(parts.head))

          List('{Comment($content.tt)})

        case TextNode("\u0000") =>
          List(iterator.next().asExprOf[Node])

        case TextNode(text) =>
          // Deliberate stdlib opt-out: `recur` below walks the stdlib `List` the quotes API uses.
          val parts = text.cut(t"\u0000").stdlib.map(_.s)

          def recur(parts: List[String], expr: Expr[String]): Expr[String] = parts match
            case Nil => expr

            case head :: tail =>
              recur(tail, '{$expr+${iterator.next().asExprOf[Text]}+${Expr(head)}})

          val content = recur(parts.tail, Expr(parts.head))

          List('{TextNode($content.tt)})

      def resultType(html: Html): scala.collection.immutable.Set[String] = html match
        case TextNode(_)           => scala.collection.immutable.Set("#text")
        case Element(tag, _, _, _) => scala.collection.immutable.Set(tag.s)
        case Fragment(values*)     => values.toSet.flatMap(resultType(_))
        case Comment(_)            => scala.collection.immutable.Set()
        case Doctype(_)            => scala.collection.immutable.Set()

      resultType(html)
      . map: label => ConstantType(StringConstant(label))
      . foldLeft(TypeRepr.of[Nothing]): (left, right) => OrType(left, right)
      . asType
      . absolve match
        case '[type topic <: Label; topic] =>
          ' {
              $ {
                  serialize(html).absolve match
                    case List(one: Expr[?]) => html.absolve match
                      case _: TextNode        => one.asExprOf[TextNode]
                      case _: Element         => one.asExprOf[Element]
                      case _: Comment         => one.asExprOf[Comment]
                      case _: Doctype         => one.asExprOf[Doctype]

                    case many               => '{Fragment(${Expr.ofList(many)}*)}
                }

              . of[topic]
              . in[Whatwg]
            }


  def attributes[result: Type, thisType <: Tag to result: Type]
    ( tag: Expr[Tag], presets: Expr[Map[Text, Text]], attributes0: Expr[Seq[(String, Any)]] )
  :   Macro[result] =

    import quotes.reflect.*

    val arguments = attributes0.absolve match
      case Varargs(arguments) => arguments

    val attributes: Seq[Expr[Optional[(Text, Optional[Text])]]] =
      Type.of[thisType].absolve match
        case
          ' [
              type topic <: Label
              type form
              Tag { type Topic = topic; type Form = form }
            ] =>

          arguments.map: argument =>
            argument.absolve match
              case '{($key, $value: value)} =>
                TypeRepr.of[topic].literal[String].let: topic =>
                  key.asTerm match
                    case Literal(StringConstant(key)) =>
                      if key == "" then halt(m"HTML tag attributes must be named")
                      else ConstantType(StringConstant(key)).asType.absolve match
                        case '[type key <: Label; key] =>
                          Expr.summon[key is Attribute in form on (? >: topic)]
                          . orElse(Expr.summon[key is Attribute in form]) match
                            case Some('{type result; $expr: Attribute { type Topic = result }}) =>
                              Expr.summon[(? >: value) is Attributive to result] match
                                case Some('{$converter: Attributive}) =>
                                  '{$converter.attribute(${Expr(key.tt)}, $value)}

                                case _ => halt:
                                  m"""
                                    $key has attribute type ${TypeRepr.of[result].show}, but
                                    ${TypeRepr.of[value].show} cannot be attributed as a
                                    ${TypeRepr.of[result].show} without a contextual instance of
                                    ${TypeRepr.of[value is Attributive to result].show}
                                  """

                            case _ =>
                              halt(m"the attribute $key cannot be used on the element <$topic>")

                    case _ =>
                      halt(m"unable to determine attribute key type")

                . or(halt(m"unexpected type"))

    // The presets are widened to the attribute map's value type so that the concatenation's
    // two operands agree; it is right-biased, so a supplied attribute overrides a preset.
    val presets2 = '{$presets: Map[Text, Optional[Text]]}
    val supplied = '{(${Expr.ofList(attributes)}.compact).to(Map)}
    val attrsExpr = '{Attributes.from($presets2 + $supplied)}
    '{$tag.node($attrsExpr)}.asExprOf[result]
