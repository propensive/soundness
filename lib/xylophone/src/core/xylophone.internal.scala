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

import scala.language.dynamics

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
      val xml: Xml = Xml.parse(Iterator(parts.scala.mkString("\u0000").tt), generic, capture(_, _))

      val holes2 = holes.to[List].sortBy(_(0)).map(_(1))
      val iterator = holes2.iterator
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
            val others = '{List.from(${Expr.ofList(pattern.attributes.keys.to(List).map(Expr(_)).scala)})}

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
          AppliedType(defn.TupleClass(types.length).info.typeSymbol.typeRef, types.reverse.scala)
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

        val mapping: Int => Int = sourceContent.lay((i: Int) => i): content =>
          if srcStart > 0 && srcStart < content.length then
            // Generous upper bound: each value char is at most 6 source chars
            // (\u####), plus a small buffer.
            val upper = (srcStart + part.length * 6 + 16).min(content.length)
            val sourceText = content.substring(srcStart, upper).nn
            Interpolation.buildMapping(sourceText, part)
          else
            (i: Int) => i

        ((part, srcStart), mapping)

      . scala.toIndexedSeq

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

      Xml.parse(Iterator(parts.scala.mkString("\u0000").tt), XmlSchema.generic, capture(_, _))

    abortive:

      val iterator: Iterator[Expr[Any]] =
        holes.to[List].sortBy(_(0)).map(_(1)).zip(List.from(insertions)).map: (hole, expr) =>
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

          List('{Header(${Expr(version)}, $encoding2, $standalone2)}).scala

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

          val map = '{Map(${Expr.ofList(exprs.scala)}*)}
          val elements = '{IArray(${Expr.ofList(children.flatMap(serialize(_)).to[Seq])}*)}

          List('{Element(${Expr(label)}, Attributes.from($map), $elements)}).scala

        case Comment(text) =>
          val parts = text.cut(t"\u0000").map(_.s)

          def recur(parts: List[String], expr: Expr[String]): Expr[String] = parts match
            case Nil => expr

            case head :: tail =>
              recur(tail, '{$expr+${iterator.next().asExprOf[Text]}+${Expr(head)}})

          val content = recur(parts.tail, Expr(parts.head))

          List('{Comment($content.tt)}).scala

        case Cdata(text) =>
          val parts = text.cut(t"\u0000").map(_.s)

          def recur(parts: List[String], expr: Expr[String]): Expr[String] = parts match
            case Nil => expr

            case head :: tail =>
              recur(tail, '{$expr+${iterator.next().asExprOf[Text]}+${Expr(head)}})

          val content = recur(parts.tail, Expr(parts.head))

          List('{Cdata($content.tt)}).scala

        case ProcessingInstruction(target, data0) =>
          val parts = data0.cut(t"\u0000").map(_.s)

          def recur(parts: List[String], expr: Expr[String]): Expr[String] = parts match
            case Nil => expr

            case head :: tail =>
              recur(tail, '{$expr+${iterator.next().asExprOf[Text]}+${Expr(head)}})

          val data = recur(parts.tail, Expr(parts.head))

          List('{ProcessingInstruction(${Expr(target)}, $data.tt)}).scala

        case TextNode("\u0000") =>
          List(iterator.next().asExprOf[Node]).scala

        case TextNode(text) =>
          val parts = text.cut(t"\u0000").map(_.s)

          def recur(parts: List[String], expr: Expr[String]): Expr[String] = parts match
            case Nil => expr

            case head :: tail =>
              recur(tail, '{$expr+${iterator.next().asExprOf[Text]}+${Expr(head)}})

          val content = recur(parts.tail, Expr(parts.head))

          List('{TextNode($content.tt)}).scala

        case Doctype(text) =>
          List('{Doctype(${Expr(text)})}).scala

      def resultType(xml: Xml): Set[String] = xml match
        case TextNode(_)        => Set("#text")
        case Element(tag, _, _) => Set(tag.s)
        case Fragment(values*)  => values.to(Set).flatMap(resultType(_))
        case _                  => Set()

      resultType(xml)
      . map { label => ConstantType(StringConstant(label)) }
      . foldLeft(TypeRepr.of[Nothing]) { (left, right) => OrType(left, right) }
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

    '{$tag.node(Attributes.from(List.from(${Expr.ofList(attributes)}).compact.to[Map]))}.asExprOf[result]
