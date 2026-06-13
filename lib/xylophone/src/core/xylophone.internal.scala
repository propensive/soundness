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
package xylophone

import language.dynamics

import java.lang as jl

import scala.collection.immutable.ListMap
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
import vacuous.*
import zephyrine.*

object internal:
  def extractor[parts <: Tuple: Type, origins <: Tuple: Type]
    ( scrutinee: Expr[Xml] )
  :   Macro[Extrapolation[Xml]] =

    import quotes.reflect.*

    def recur[tuple: Type](strings: List[String]): List[String] = Type.of[tuple] match
      case '[head *: tail] => recur[tail](TypeRepr.of[head].literal[String].vouch :: strings)
      case _               => strings

    val parts = recur[parts](Nil)

    def intersect(parts: List[String], repr: TypeRepr = TypeRepr.of[Nothing]): TypeRepr =
      parts match
        case head :: tail => intersect(tail, OrType(repr, ConstantType(StringConstant(head))))
        case Nil          => repr

    abortive:
      var holes: Map[Ordinal, Xml.Hole] = Map()
      def capture(ordinal: Ordinal, hole: Xml.Hole) = holes = holes.updated(ordinal, hole)

      given XmlSchema = XmlSchema.Freeform

      val generic: Tag = Tag.root(Set())
      val xml: Xml = Xml.parse(Iterator(parts.mkString("\u0000").tt), generic, capture(_, _))

      val holes2 = holes.to(List).sortBy(_(0)).map(_(1))
      val iterator = holes2.to(Iterator)
      var index: Int = -1

      var types: List[TypeRepr] = Nil

      def checkText(array: Expr[Array[Any]], pattern: TextNode, scrutinee: Expr[TextNode])
      :   Expr[Boolean] =

        '{${Expr(pattern.text)} == $scrutinee.text}

      def checkComment(array: Expr[Array[Any]], pattern: Comment, scrutinee: Expr[Comment])
      :   Expr[Boolean] =

        '{${Expr(pattern.text)} == $scrutinee.text}

      def checkCdata(array: Expr[Array[Any]], pattern: Cdata, scrutinee: Expr[Cdata])
      :   Expr[Boolean] =

        '{${Expr(pattern.text)} == $scrutinee.text}

      def checkPi
        ( array:     Expr[Array[Any]],
          pattern:   ProcessingInstruction,
          scrutinee: Expr[ProcessingInstruction] )
      :   Expr[Boolean] =

        ' {
            ${Expr(pattern.target)} == $scrutinee.target
            && ${Expr(pattern.data)} == $scrutinee.data
          }

      def checkHeader(array: Expr[Array[Any]], pattern: Header, scrutinee: Expr[Header])
      :   Expr[Boolean] =

        '{${Expr(pattern.version)} == $scrutinee.version} // FIXME: Check encoding/standalone too

      def checkFragment(array: Expr[Array[Any]], pattern: Fragment, scrutinee: Expr[Fragment])
      :   Expr[Boolean] =

        val children = '{$scrutinee.nodes}

        def elements(index: Int)(expr: Expr[Boolean]): Expr[Boolean] =
          if index == pattern.nodes.length then expr else
            val expr2 =
              descend(array, pattern.nodes(index), '{$children(${Expr(index)})}, '{true})

            elements(index + 1)('{$expr && $expr2})


        elements(0):
          '{$scrutinee.nodes.length == ${Expr(pattern.nodes.length)}}

      def checkElement(array: Expr[Array[Any]], pattern: Element, scrutinee: Expr[Element])
      :   Expr[Boolean] =

        def attributes(todo: List[Text])(expr: Expr[Boolean]): Expr[Boolean] = todo match
          case Nil => expr

          case "\u0000" :: tail =>
            index += 1
            types ::= TypeRepr.of[Map[Text, Text]]
            iterator.next()
            val others = Expr.ofList(pattern.attributes.keys.to(List).map(Expr(_)))

            ' {
                $expr
                && { $array(${Expr(index)}) = (${scrutinee}.attributes -- $others).toMap; true }
              }

          case head :: tail =>
            attributes(tail):
              val boolean: Expr[Boolean] = pattern.attributes(head).s.absolve match
                case "\u0000" =>
                  index += 1
                  types ::= TypeRepr.of[Text]
                  iterator.next()
                  '{$array(${Expr(index)}) = $scrutinee.attributes(${Expr(head)}); true}

                case text: Text =>
                  '{$scrutinee.attributes(${Expr(head)}) == ${Expr(text)}}

              '{$expr && $boolean}

        val attributesChecked = attributes(pattern.attributes.toList.map(_(0)))('{true})

        val children = '{$scrutinee.children}

        def elements(index: Int)(expr: Expr[Boolean]): Expr[Boolean] =
          if index == pattern.children.length then expr else
            val expr2 =
              descend(array, pattern.children(index), '{$children(${Expr(index)})}, '{true})

            elements(index + 1)('{$expr && $expr2})

        val elementsChecked = elements(0):
          ' {
              ${Expr(pattern.label)} == $scrutinee.label
              && $scrutinee.children.length == ${Expr(pattern.children.length)}
            }

        '{$attributesChecked && $elementsChecked}

      def descend(array: Expr[Array[Any]], pattern: Xml, scrutinee: Expr[Xml], expr: Expr[Boolean])
      :   Expr[Boolean] =

        pattern match
          case Comment("\u0000") =>
            index += 1
            iterator.next()
            types ::= TypeRepr.of[Text]

            ' {
                $expr && $scrutinee.isInstanceOf[Comment]
                &&
                  {
                    $array(${Expr(index)}) = $scrutinee.asInstanceOf[Comment].text
                    true
                  }
              }

          case ProcessingInstruction("\u0000", t"") =>
            index += 1
            iterator.next()
            types ::= TypeRepr.of[ProcessingInstruction]

            ' {
                $expr && $scrutinee.isInstanceOf[ProcessingInstruction]
                &&
                {
                  $array(${Expr(index)}) = $scrutinee.asInstanceOf[ProcessingInstruction].data
                  true
                }
              }

          case Cdata("\u0000") =>
            index += 1
            iterator.next()
            types ::= TypeRepr.of[Cdata]

            ' {
                $expr && $scrutinee.isInstanceOf[Cdata]
                && { $array(${Expr(index)}) = $scrutinee.asInstanceOf[Cdata].text; true }
              }

          case TextNode("\u0000") =>
            index += 1

            iterator.next() match
              case Xml.Hole.Node(label) =>
                types ::= TypeRepr.of[Node]

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

          case cdata@Cdata(content) =>
            if content.contains("\u0000") then halt:
              m"""
                only the entire CDATA content can be matched; write the extractor as
                ${t"<![CDATA[$$text]]>"}
              """

            val checked = checkCdata(array, cdata, '{$scrutinee.asInstanceOf[Cdata]})

            '{$expr && $scrutinee.isInstanceOf[Cdata] && $checked}

          case pi@ProcessingInstruction(target, data) =>
            if data.contains("\u0000") || target.contains("\u0000")
            then halt(m"only the entire data part of a processing instruction can be matched")

            val checked = checkPi(array, pi, '{$scrutinee.asInstanceOf[ProcessingInstruction]})
            '{$expr && $scrutinee.isInstanceOf[ProcessingInstruction] && $checked}

          case Element("\u0000", _, _) =>
            index += 1

            iterator.next() match
              case Xml.Hole.Element(label) =>
                types ::= TypeRepr.of[Element]

              case _ =>
                halt(m"unexpected hole type")

            '{$expr && { $array(${Expr(index)}) = $scrutinee; true }}

          case element: Element =>
            def checked = checkElement(array, element, '{$scrutinee.asInstanceOf[Element]})
            '{$expr && $scrutinee.isInstanceOf[Element] && $checked}

          case header: Header =>
            def checked = checkHeader(array, header, '{$scrutinee.asInstanceOf[Header]})
            '{$expr && $scrutinee.isInstanceOf[Header] && $checked}

          case fragment@Fragment(nodes*) =>
            val checked = checkFragment(array, fragment, '{$scrutinee.asInstanceOf[Fragment]})
            '{$expr && $scrutinee.isInstanceOf[Fragment] && $checked}

          case Doctype(_) =>
            halt(m"DOCTYPE patterns are not supported in extractors")


      val result: Expr[Extrapolation[Xml]] =
        ' {
            val extracts = new Array[Any](${Expr(holes.size)})
            val matches: Boolean = ${descend('extracts, xml, scrutinee, '{true})}

            $ {
                if holes.size == 0 then '{matches}
                else if holes.size == 1
                then '{if !matches then None else Some(extracts(0).asInstanceOf[Xml])}
                else '{if !matches then None else Some(Tuple.fromArray(extracts))}
              }
          }

      types.length match
        case 0 => '{$result.asInstanceOf[Boolean]}

        case 1 =>
          types.head.asType.absolve match
            case '[type result <: Xml; result] =>
              '{$result.asInstanceOf[Option[result]]}

        case _ =>
          AppliedType(defn.TupleClass(types.length).info.typeSymbol.typeRef, types.reverse)
          . asType
          . absolve match
            case '[type result <: Tuple; result] =>
              '{$result.asInstanceOf[Option[result]]}

  def interpolator[parts <: Tuple: Type, origins <: Tuple: Type]
    ( insertions0: Expr[Seq[Any]] )
  :   Macro[Xml] =

    import quotes.reflect.*
    import Xml.Hole

    def recur[tuple: Type](strings: List[String]): List[String] = Type.of[tuple] match
      case '[head *: tail] => recur[tail](TypeRepr.of[head].literal[String].vouch :: strings)
      case _               => strings

    val parts = recur[parts](Nil)

    // Decode Origins into a List[(Int, Int)] of (start, end) source offsets per part.
    def recurOrigins[tuple: Type](acc: List[(Int, Int)]): List[(Int, Int)] =
      Type.of[tuple] match
        case '[head *: tail] =>
          val pair = TypeRepr.of[head].dealias match
            case AppliedType(_, List(ConstantType(IntConstant(s)), ConstantType(IntConstant(e)))) =>
              (s, e)

            case _ =>
              (0, 0)

          recurOrigins[tail](pair :: acc)

        case _ =>
          acc.reverse

    val partOrigins: List[(Int, Int)] = recurOrigins[origins](Nil)

    // Map a parser char-offset (within the joined input) back to a source-file
    // Position. Uses Interpolation.buildMapping to translate value-offset to
    // source-offset within each part, so escape sequences in the source
    // (\n, \t, \uHHHH, etc.) correctly resolve to the longer source span.
    val sourceFile = Position.ofMacroExpansion.sourceFile
    val macroPos = Position.ofMacroExpansion

    val sourceContent: Optional[String] = sourceFile.content match
      case Some(s: String) => s
      case _               => Unset

    // Pre-compute, per part, the value→source mapping and the *actual* source
    // end. The Scala compiler's lit.pos.end is unreliable for parts containing
    // `$$` (it reports the value length, not the source length); walking the
    // source forward with escape rules gives us the truth.
    val perPart: IndexedSeq[((String, Int), Int => Int)] =
      parts.zip(partOrigins).map: (part, origin) =>
        val (srcStart, _) = origin

        val mapping: Int => Int = sourceContent.lay[Int => Int](identity): content =>
          if srcStart > 0 && srcStart < content.length then
            // Generous upper bound: each value char is at most 6 source chars
            // (\u####), plus a small buffer.
            val upper = (srcStart + part.length * 6 + 16).min(content.length)
            val sourceText = content.substring(srcStart, upper).nn
            Interpolation.buildMapping(sourceText, part)
          else
            (i: Int) => i

        ((part, srcStart), mapping)

      . toIndexedSeq

    def translateOffset(parserOff: Int, len: Int): Position =
      var acc = 0
      var i = 0

      while i < perPart.length do
        val ((part, srcStart), mapping) = perPart(i)
        val partLen = part.length

        if parserOff < acc + partLen && srcStart > 0 then
          val inPart = parserOff - acc
          val endIn = (inPart + len.max(1)).min(part.length)
          val rawStart = (srcStart + mapping(inPart)).max(srcStart)
          val rawEnd = (srcStart + mapping(endIn)).max(rawStart + 1)
          return Position(sourceFile, rawStart, rawEnd)

        acc += partLen + 1
        i += 1

      macroPos

    val insertions: Seq[Expr[Any]] = insertions0.absolve match
      case Varargs(insertions) => insertions

    var holes: Map[Ordinal, Xml.Hole] = Map()
    def capture(ordinal: Ordinal, hole: Hole) = holes = holes.updated(ordinal, hole)

    given XmlSchema = XmlSchema.Freeform

    // Custom HaltTactic: when Xml.parse raises ParseError, translate the parser
    // offset/length to a source-file Position and pass it to halt, so editors
    // underline the precise span inside the literal.
    val xml: Xml =
      given diagnostics: Diagnostics = Diagnostics.omit

      given parseTactic: HaltTactic[ParseError, Xml] = new HaltTactic[ParseError, Xml]:
        override def abort(error: Diagnostics ?=> ParseError): Nothing =
          val pe = error
          val off = pe.position.offset.or(0)
          val length = pe.position.length.or(0)
          halt(pe.labelled, translateOffset(off, length))

      Xml.parse(Iterator(parts.mkString("\u0000").tt), XmlSchema.generic, capture(_, _))

    abortive:

      val iterator: Iterator[Expr[Any]] =
        holes.to(List).sortBy(_(0)).map(_(1)).zip(insertions).map: (hole, expr) =>
          expr.absolve match
            case '{$expr: value} => hole match
              case Hole.Attribute(tag, attribute) =>
                ConstantType(StringConstant(tag.s)).asType.absolve match
                  case '[tag] => ConstantType(StringConstant(attribute.s)).asType.absolve match
                    case '[attribute] =>
                      Expr.summon[attribute is Xml.XmlAttribute] match
                        case
                          Some
                            ( ' {
                                  type result
                                  $typeclass: Xml.XmlAttribute { type Topic = result }
                                } ) =>

                          Expr.summon[(? >: value) is Attributive to result] match
                            case Some('{$attributive}) =>
                              '{$attributive.attribute(${Expr(attribute)}, $expr).let(_(1))}

                            case _ =>
                              halt
                                ( m"""
                                    ${TypeRepr.of[value].show} cannot be attributed to an attribute
                                    of ${Syntax(TypeRepr.of[result]).show}
                                  """,
                                  expr.asTerm.underlyingArgument.pos )

                        case _ =>
                          expr match
                            case '{$expr: Text} =>
                              expr

                            case _ =>
                              halt
                                ( m"the attribute $attribute cannot be used on the element <$tag>",
                                  expr.asTerm.underlyingArgument.pos )

              case Hole.Element(tag) =>
                ConstantType(StringConstant(tag.s)).asType.absolve match
                  case '[tag] => Expr.summon[(? >: value) is Encodable in Xml] match
                    case Some('{$encodable: Encodable}) =>
                      '{$encodable.encode($expr)}

                    case _ =>
                      halt
                        ( m"""
                            a value of ${TypeRepr.of[value].show} is not encodable inside a <$tag>
                            element
                          """,
                          expr.asTerm.underlyingArgument.pos )

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
                          halt
                            ( m"""
                                a value of ${TypeRepr.of[value].show} is not renderable or showable
                                inside a <$tag> element
                              """,
                              expr.asTerm.underlyingArgument.pos )

              case Hole.Comment => Expr.summon[(? >: value) is Showable] match
                case Some(showable) =>
                  '{$showable.text($expr)}

                case None =>
                  halt
                    ( m"a ${TypeRepr.of[value is Showable].show} is required",
                      expr.asTerm.underlyingArgument.pos )

              case Hole.Text => Expr.summon[(? >: value) is Showable] match
                case Some(showable) =>
                  '{$showable.text($expr)}

                case None =>
                  halt
                    ( m"a ${TypeRepr.of[value is Showable].show} is required",
                      expr.asTerm.underlyingArgument.pos )

              case Hole.Tagbody => Type.of[value] match
                case '[Map[Text, Text]] =>
                  expr

                case _ =>
                  halt
                    ( m"only a ${TypeRepr.of[Map[Text, Text]].show} can be applied in a tag body",
                      expr.asTerm.underlyingArgument.pos )

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
          val exprs = attributes.toList.map: (key, value) =>
            ' {
                ( ${Expr(key)},
                  $ {
                      if value == "\u0000".tt then iterator.next().asExprOf[Text]
                      else Expr[Text](value)
                    } )
              }

            . asExprOf[(Text, Text)]

          val map = '{Map(${Expr.ofList(exprs)}*)}
          val elements = '{IArray(${Expr.ofList(children.flatMap(serialize(_)))}*)}

          List('{Element(${Expr(label)}, Attributes.from($map), $elements)})

        case Comment(text) =>
          val parts = text.cut(t"\u0000").map(_.s)

          def recur(parts: List[String], expr: Expr[String]): Expr[String] = parts match
            case Nil => expr

            case head :: tail =>
              recur(tail, '{$expr+${iterator.next().asExprOf[Text]}+${Expr(head)}})

          val content = recur(parts.tail, Expr(parts.head))

          List('{Comment($content.tt)})

        case Cdata(text) =>
          val parts = text.cut(t"\u0000").map(_.s)

          def recur(parts: List[String], expr: Expr[String]): Expr[String] = parts match
            case Nil => expr

            case head :: tail =>
              recur(tail, '{$expr+${iterator.next().asExprOf[Text]}+${Expr(head)}})

          val content = recur(parts.tail, Expr(parts.head))

          List('{Cdata($content.tt)})

        case ProcessingInstruction(target, data0) =>
          val parts = data0.cut(t"\u0000").map(_.s)

          def recur(parts: List[String], expr: Expr[String]): Expr[String] = parts match
            case Nil => expr

            case head :: tail =>
              recur(tail, '{$expr+${iterator.next().asExprOf[Text]}+${Expr(head)}})

          val data = recur(parts.tail, Expr(parts.head))

          List('{ProcessingInstruction(${Expr(target)}, $data.tt)})

        case TextNode("\u0000") =>
          List(iterator.next().asExprOf[Node])

        case TextNode(text) =>
          val parts = text.cut(t"\u0000").map(_.s)

          def recur(parts: List[String], expr: Expr[String]): Expr[String] = parts match
            case Nil => expr

            case head :: tail =>
              recur(tail, '{$expr+${iterator.next().asExprOf[Text]}+${Expr(head)}})

          val content = recur(parts.tail, Expr(parts.head))

          List('{TextNode($content.tt)})

        case Doctype(text) =>
          List('{Doctype(${Expr(text)})})

      def resultType(xml: Xml): Set[String] = xml match
        case TextNode(_)        => Set("#text")
        case Element(tag, _, _) => Set(tag.s)
        case Fragment(values*)  => values.to(Set).flatMap(resultType(_))
        case _                  => Set()

      resultType(xml)
      . map: label => ConstantType(StringConstant(label))
      . foldLeft(TypeRepr.of[Nothing]): (left, right) => OrType(left, right)
      . asType
      . absolve match
        case '[type topic <: Label; topic] =>
          ' {
              $ {
                  serialize(xml).absolve match
                    case List(one: Expr[?]) => one.asExprOf[Xml]
                    case many               => '{Fragment(${Expr.ofList(many)}*)}
                }

              . of[topic]
            }


  def attributes[result: Type, thisType <: Tag to result: Type]
    ( tag: Expr[Tag], attributes0: Expr[Seq[(String, Any)]] )
  :   Macro[result] =

    import quotes.reflect.*

    val arguments = attributes0.absolve match
      case Varargs(arguments) => arguments

    val attributes: Seq[Expr[Optional[(Text, Text)]]] =
      Type.of[thisType].absolve match
        case
          ' [
              type topic <: Label;
              type form;
              Tag { type Topic = topic; type Form = form }
            ] =>

          arguments.map: argument =>
            argument.absolve match
              case '{($key, $value: value)} =>
                TypeRepr.of[topic].literal[String].let: topic =>
                  key.asTerm match
                    case Literal(StringConstant(key)) =>
                      if key == "" then panic(m"Empty key")
                      else ConstantType(StringConstant(key)).asType.absolve match
                        case '[type key <: Label; key] =>
                          Expr.summon[key is Xml.XmlAttribute in form on (? >: topic)]
                          . orElse(Expr.summon[key is Xml.XmlAttribute in form]) match
                            case
                              Some
                                ( ' {
                                      type result
                                      $expr: Xml.XmlAttribute { type Topic = result }
                                    } ) =>

                              Expr.summon[(? >: value) is Attributive to result] match
                                case Some('{$converter: Attributive}) =>
                                  '{$converter.attribute(${Expr(key.tt)}, $value)}

                                case _ =>
                                  halt:
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

    '{$tag.node(Attributes.from(${Expr.ofList(attributes)}.compact.to(Map)))}.asExprOf[result]

  opaque type Attributes = IArray[String]

  object Attributes:
    val empty: Attributes = IArray.empty[String]

    def apply(pairs: (Text, Text)*): Attributes =
      if pairs.isEmpty then empty else
        val n = pairs.length
        val arr = new Array[String](n*2)
        var i = 0

        pairs.foreach: pair =>
          arr(i*2) = pair._1.s
          arr(i*2 + 1) = pair._2.s
          i += 1

        arr.immutable(using Unsafe)

    def from(map: Map[Text, Text]): Attributes =
      if map.isEmpty then empty else
        val n = map.size
        val arr = new Array[String](n*2)
        var i = 0

        map.foreach: (k, v) =>
          arr(i*2) = k.s
          arr(i*2 + 1) = v.s
          i += 1

        arr.immutable(using Unsafe)

    // Construct an `Attributes` directly from an interleaved `IArray`. The
    // caller guarantees the array's length is even and that every key slot
    // (even index) holds a non-null `String`. Used by the parser, which
    // assembles the interleaved array as it tokenizes attributes.
    private[xylophone] inline def fromInterleaved(array: IArray[String]): Attributes = array

    // Unwrap to the raw `Array[String]` for hot-path internal access. Safe
    // within the package: the storage is shared but never mutated outside
    // construction.
    private[xylophone] inline def storage(attrs: Attributes): Array[String] =
      attrs.asInstanceOf[Array[String]]

    extension (attrs: Attributes)
      inline def size: Int = attrs.length/2
      inline def isEmpty: Boolean = attrs.length == 0
      inline def nonEmpty: Boolean = attrs.length > 0
      inline def nil: Boolean = attrs.length == 0

      def apply(key: Text): Text =
        val a = storage(attrs)
        val keyStr: String = key.s
        val n = a.length
        var i = 0

        while i < n do
          if a(i) == keyStr then return a(i + 1).asInstanceOf[Text]
          i += 2

        throw new NoSuchElementException(s"key not found: $key")

      def at(key: Text): Optional[Text] =
        val a = storage(attrs)
        val keyStr: String = key.s
        val n = a.length
        var i = 0

        while i < n do
          if a(i) == keyStr then return a(i + 1).asInstanceOf[Text]
          i += 2

        Unset

      def contains(key: Text): Boolean =
        val a = storage(attrs)
        val keyStr: String = key.s
        val n = a.length
        var i = 0

        while i < n do
          if a(i) == keyStr then return true
          i += 2

        false

      def keys: Iterator[Text] =
        val a = storage(attrs)

        new Iterator[Text]:
          private var i: Int = 0
          def hasNext: Boolean = i < a.length

          def next(): Text =
            val k = a(i).asInstanceOf[Text]
            i += 2
            k

      def values: Iterator[Text] =
        val a = storage(attrs)

        new Iterator[Text]:
          private var i: Int = 1
          def hasNext: Boolean = i < a.length

          def next(): Text =
            val v = a(i).asInstanceOf[Text]
            i += 2
            v

      def iterator: Iterator[(Text, Text)] =
        val a = storage(attrs)

        new Iterator[(Text, Text)]:
          private var i: Int = 0
          def hasNext: Boolean = i < a.length

          def next(): (Text, Text) =
            val pair = (a(i).asInstanceOf[Text], a(i + 1).asInstanceOf[Text])
            i += 2
            pair

      def toMap: Map[Text, Text] =
        val a = storage(attrs)

        if a.length == 0 then ListMap.empty else
          val b = ListMap.newBuilder[Text, Text]
          var i = 0

          while i < a.length do
            b += ((a(i).asInstanceOf[Text], a(i + 1).asInstanceOf[Text]))
            i += 2

          b.result()

      def toList: List[(Text, Text)] =
        val a = storage(attrs)
        val b = List.newBuilder[(Text, Text)]
        var i = 0

        while i < a.length do
          b += ((a(i).asInstanceOf[Text], a(i + 1).asInstanceOf[Text]))
          i += 2

        b.result()

      def each(action: (Text, Text) => Unit): Unit =
        val a = storage(attrs)
        var i = 0

        while i < a.length do
          action(a(i).asInstanceOf[Text], a(i + 1).asInstanceOf[Text])
          i += 2

      def foreach(action: (Text, Text) => Unit): Unit = each(action)

      def map[B](f: ((Text, Text)) => B): Iterable[B] =
        val a = storage(attrs)
        val b = List.newBuilder[B]
        var i = 0

        while i < a.length do
          b += f((a(i).asInstanceOf[Text], a(i + 1).asInstanceOf[Text]))
          i += 2

        b.result()

      def removed(key: Text): Attributes =
        val a = storage(attrs)
        val keyStr: String = key.s
        val n = a.length
        var idx = -1
        var i = 0

        while idx < 0 && i < n do
          if a(i) == keyStr then idx = i
          i += 2

        if idx < 0 then attrs else
          val nu = new Array[String](n - 2)
          if idx > 0 then jl.System.arraycopy(a, 0, nu, 0, idx)
          if idx < n - 2 then jl.System.arraycopy(a, idx + 2, nu, idx, n - 2 - idx)
          nu.immutable(using Unsafe)

      inline def `-`(key: Text): Attributes = removed(key)

      def `--`(others: Iterable[Text]): Attributes =
        if isEmpty then attrs else
          var result: Attributes = attrs
          others.foreach: k => result = result.removed(k)
          result

      def updated(key: Text, value: Text): Attributes =
        val a = storage(attrs)
        val keyStr: String = key.s
        val n = a.length
        var idx = -1
        var i = 0

        while idx < 0 && i < n do
          if a(i) == keyStr then idx = i
          i += 2

        if idx >= 0 then
          val nu = new Array[String](n)
          jl.System.arraycopy(a, 0, nu, 0, n)
          nu(idx + 1) = value.s
          nu.immutable(using Unsafe)
        else
          val nu = new Array[String](n + 2)
          jl.System.arraycopy(a, 0, nu, 0, n)
          nu(n) = keyStr
          nu(n + 1) = value.s
          nu.immutable(using Unsafe)

      def `++`(other: Attributes): Attributes =
        val a = storage(attrs)
        val b = storage(other)

        if b.length == 0 then attrs
        else if a.length == 0 then other
        else
          val total = a.length + b.length
          val nu = new Array[String](total)
          var written = 0
          var i = 0

          while i < a.length do
            val k = a(i)
            var bi = 0
            var found = -1

            while found < 0 && bi < b.length do
              if b(bi) == k then found = bi
              bi += 2

            nu(written) = k
            nu(written + 1) = if found >= 0 then b(found + 1) else a(i + 1)
            written += 2
            i += 2

          var j = 0

          while j < b.length do
            val k = b(j)
            var ai = 0
            var found = false

            while !found && ai < a.length do
              if a(ai) == k then found = true
              ai += 2

            if !found then
              nu(written) = k
              nu(written + 1) = b(j + 1)
              written += 2

            j += 2

          if written == total then nu.immutable(using Unsafe)
          else
            val tu = new Array[String](written)
            jl.System.arraycopy(nu, 0, tu, 0, written)
            tu.immutable(using Unsafe)

      def `++`(other: Map[Text, Text]): Attributes =
        if other.isEmpty then attrs else attrs ++ Attributes.from(other)

      // Same set of (key, value) pairs (order-insensitive). Iterates the left,
      // looks up each key in the right.
      def equalsAttributes(other: Attributes): Boolean =
        val a = storage(attrs)
        val b = storage(other)
        val n = a.length

        if n != b.length then false else
          var i = 0
          var ok = true

          while ok && i < n do
            val k = a(i)
            val va = a(i + 1)
            var j = 0
            var found = -1

            while found < 0 && j < n do
              if b(j) == k then found = j
              j += 2

            if found < 0 then ok = false
            else if va != b(found + 1) then ok = false

            i += 2

          ok

      def hashAttributes: Int =
        // Order-independent: XOR of (key.hash * 31 ^ value.hash).
        val a = storage(attrs)
        var h = 0
        var i = 0

        while i < a.length do
          h = h ^ (a(i).hashCode*31 ^ a(i + 1).hashCode)
          i += 2

        h
