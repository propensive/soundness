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
┃    Soundness, version 0.63.0.                                                                    ┃
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

import scala.collection.immutable.Seq
import scala.collection.immutable.IndexedSeq

import scala.{annotation, caps}

import proscenium.compat.*

import scala.language.dynamics

import java.lang as jl

import scala.collection.immutable.ListMap
import scala.collection.immutable.{List, Nil, ::}
import scala.quoted.*

import anticipation.*
import contextual.*
import contingency.*
import denominative.*
import distillate.*
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
      var holes: scala.collection.immutable.Map[Ordinal, Xml.Hole] =
        scala.collection.immutable.Map()
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

        '{${Expr(pattern.target)} == $scrutinee.target && ${Expr(pattern.data)} == $scrutinee.data}

      def checkHeader(array: Expr[Array[Any]], pattern: Header, scrutinee: Expr[Header])
      :   Expr[Boolean] =

        val encoding: Expr[Boolean] =
          if pattern.encoding == Unset then '{$scrutinee.encoding == Unset}
          else '{$scrutinee.encoding == ${Expr(pattern.encoding.asInstanceOf[Text])}}

        val standalone: Expr[Boolean] =
          if pattern.standalone == Unset then '{$scrutinee.standalone == Unset}
          else '{$scrutinee.standalone == ${Expr(pattern.standalone.asInstanceOf[Boolean])}}

        '{${Expr(pattern.version)} == $scrutinee.version && $encoding && $standalone}

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
                $expr &&
                  { $array(${Expr(index)}) = (${scrutinee}.attributes -- $others).toMap; true }
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
              ${Expr(pattern.label)} == $scrutinee.label &&
                $scrutinee.children.length == ${Expr(pattern.children.length)}
            }

        '{$attributesChecked && $elementsChecked}

      def descend(array: Expr[Array[Any]], pattern: Xml, scrutinee: Expr[Xml], expr: Expr[Boolean])
      :   Expr[Boolean] =

        pattern match
          case Comment("\u0000") =>
            // Comment, CDATA and processing-instruction holes are not emitted
            // by the parser's callback, so they have no `holes`/`iterator`
            // entry; only `index` advances to address the `extracts` slot.
            index += 1
            types ::= TypeRepr.of[Text]

            ' {
                $expr && $scrutinee.isInstanceOf[Comment] &&
                  {
                    $array(${Expr(index)}) = $scrutinee.asInstanceOf[Comment].text
                    true
                  }
              }

          case ProcessingInstruction("\u0000", t"") =>
            index += 1
            types ::= TypeRepr.of[ProcessingInstruction]

            ' {
                $expr && $scrutinee.isInstanceOf[ProcessingInstruction] &&
                  { $array(${Expr(index)}) = $scrutinee; true }
              }

          case Cdata("\u0000") =>
            index += 1
            types ::= TypeRepr.of[Text]

            ' {
                $expr && $scrutinee.isInstanceOf[Cdata] &&
                  { $array(${Expr(index)}) = $scrutinee.asInstanceOf[Cdata].text; true }
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


      // Every `$`-substitution in the pattern is one captured value, whether or
      // not the parser emitted a structural `Hole` for it (comment/CDATA/PI
      // holes are not captured). `holes.size` undercounts those, so the slot
      // count is the number of string parts minus one.
      val holeCount = parts.length - 1

      val result: Expr[Extrapolation[Xml]] =
        ' {
            val extracts = new Array[Any](${Expr(holeCount)})
            val matches: Boolean = ${descend('extracts, xml, scrutinee, '{true})}

            $ {
                if holeCount == 0 then '{matches}
                else if holeCount == 1
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
              halt:
                m"""
                  a single text value cannot be captured on its own; capture it together with
                  another value (so the result is a tuple), or capture the enclosing node
                """

        case _ =>
          AppliedType(defn.TupleClass(types.length).info.typeSymbol.typeRef, types.reverse)
          . asType
          . absolve match
            case '[type result <: Tuple; result] =>
              '{$result.asInstanceOf[Option[result]]}

  // Reuses `XPath`'s own `Decodable` for validation: the literal is decoded at
  // macro-expansion time and, if it fails, the `XPathError`'s offset is mapped
  // back to a source position so the error points exactly at the offending
  // character. Mirrors `jacinta.internal.jsonPointer`.
  def xpath[parts <: Tuple: Type, origins <: Tuple: Type](insertions: Expr[Seq[Any]])
  :   Macro[XPath] =

    import quotes.reflect.*

    def recur[tuple: Type](strings: List[String]): List[String] = Type.of[tuple] match
      case '[head *: tail] => recur[tail](TypeRepr.of[head].literal[String].vouch :: strings)
      case _               => strings

    def firstOrigin[tuple: Type]: Int = Type.of[tuple] match
      case '[head *: tail] => TypeRepr.of[head].dealias match
        case AppliedType(_, ConstantType(IntConstant(start)) :: _) => start
        case _                                                     => 0

      case _ => 0

    val parts = recur[parts](Nil)
    if parts.length != 1 then halt(m"an XPath literal cannot have substitutions")
    val raw: String = parts.head
    val start: Int = firstOrigin[origins]

    try unsafely(raw.tt.as[XPath]) catch
      case error: XPathError =>
        val sourceFile = Position.ofMacroExpansion.sourceFile

        val position = sourceFile.content match
          case Some(content: String) if start > 0 && start < content.length =>
            val upper = (start + raw.length*6 + 16).min(content.length)
            val mapping = Interpolation.buildMapping(content.substring(start, upper).nn, raw)
            val at = (start + mapping(error.offset.min(raw.length))).min(content.length - 1)
            Position(sourceFile, at, (at + 1).min(content.length))

          case _ =>
            Position.ofMacroExpansion

        halt(error.message, position)

    '{unsafely(${Expr(raw)}.tt.as[XPath])}

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
    val perPart: IndexedSeq[((String, Int), Int -> Int)] =
      parts.zip(partOrigins).map: (part, origin) =>
        val (srcStart, _) = origin

        val mapping: Int -> Int = sourceContent.lay[Int -> Int](identity): content =>
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

    var holes: scala.collection.immutable.Map[Ordinal, Xml.Hole] =
        scala.collection.immutable.Map()
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
                          // Reflective subtype check rather than a `'{$x: Text}` quoted pattern: CC
                          // stamps `^` on the matched `Expr` types of quoted patterns.
                          if expr.asTerm.tpe <:< TypeRepr.of[Text]
                          then expr.asExprOf[Text & value]
                          else
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
                    case Some(renderable) =>
                      '{$renderable.render($expr)}.asExprOf[Any]

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

              case Hole.Tagbody =>
                if TypeRepr.of[value] <:< TypeRepr.of[Map[Text, Text]] then expr.asExprOf[Any]
                else
                  halt
                    ( m"only a ${TypeRepr.of[Map[Text, Text]].show} can be applied in a tag body",
                      expr.asTerm.underlyingArgument.pos )

        . iterator

      def serialize(xml: Xml): Seq[Expr[Node]] = xml match
        case Fragment(children*) => children.flatMap(serialize(_))

        case Header(version, encoding, standalone, _) =>
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
          val elements =
            val serialized = scala.collection.immutable.ArraySeq
            . unsafeWrapArray(children.asInstanceOf[Array[Node]]).flatMap(serialize(_)).toList

            '{IArray(${Expr.ofList(serialized)}*)}

          List('{Element(${Expr(label)}, Attributes.from($map), $elements)})

        case Comment(text) =>
          val parts = text.cut(t"\u0000").stdlib.map(_.s)

          def recur(parts: List[String], expr: Expr[String]): Expr[String] = parts match
            case Nil => expr

            case head :: tail =>
              recur(tail, '{$expr+${iterator.next().asExprOf[Text]}+${Expr(head)}})

          val content = recur(parts.tail, Expr(parts.head))

          List('{Comment($content.tt)})

        case Cdata(text) =>
          val parts = text.cut(t"\u0000").stdlib.map(_.s)

          def recur(parts: List[String], expr: Expr[String]): Expr[String] = parts match
            case Nil => expr

            case head :: tail =>
              recur(tail, '{$expr+${iterator.next().asExprOf[Text]}+${Expr(head)}})

          val content = recur(parts.tail, Expr(parts.head))

          List('{Cdata($content.tt)})

        case ProcessingInstruction(target, data0) =>
          val parts = data0.cut(t"\u0000").stdlib.map(_.s)

          def recur(parts: List[String], expr: Expr[String]): Expr[String] = parts match
            case Nil => expr

            case head :: tail =>
              recur(tail, '{$expr+${iterator.next().asExprOf[Text]}+${Expr(head)}})

          val data = recur(parts.tail, Expr(parts.head))

          List('{ProcessingInstruction(${Expr(target)}, $data.tt)})

        case TextNode("\u0000") =>
          List(iterator.next().asExprOf[Node])

        case TextNode(text) =>
          val parts = text.cut(t"\u0000").stdlib.map(_.s)

          def recur(parts: List[String], expr: Expr[String]): Expr[String] = parts match
            case Nil => expr

            case head :: tail =>
              recur(tail, '{$expr+${iterator.next().asExprOf[Text]}+${Expr(head)}})

          val content = recur(parts.tail, Expr(parts.head))

          List('{TextNode($content.tt)})

        case Doctype(text) =>
          List('{Doctype(${Expr(text)})})

      def resultType(xml: Xml): scala.collection.immutable.Set[String] = xml match
        case TextNode(_)        => scala.collection.immutable.Set("#text")
        case Element(tag, _, _) => scala.collection.immutable.Set(tag.s)
        case Fragment(values*)  => values.toSet.flatMap(resultType(_))
        case _                  => scala.collection.immutable.Set()

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

    '{$tag.node(Attributes.from(Map.from(${Expr.ofList(attributes)}.compact)))}.asExprOf[result]

  opaque type Attributes <: IArray[String] = IArray[String]

  object Attributes:
    val empty: Attributes = IArray.empty[String]

    // `Attributes` is a `Text`-keyed map, so it indexes through the shared `at` (giving
    // `Optional`).
    given indexable: Attributes is Indexable:
      type Operand = Text
      type Result = Text

      def contains(attrs: Attributes, key: Text): Boolean = attrs.contains(key)
      def access(attrs: Attributes, key: Text): Text = attrs(key)

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
      val entries = map.stdlib
      if entries.isEmpty then empty else
        val n = entries.size
        val arr = new Array[String](n*2)
        var i = 0

        entries.foreach: (k, v) =>
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

      // `at` without the `Indexable` detour, whose resolution is ambiguous
      // against the `IArray` instance under the opaque bound — used by
      // staged parsers' generated `@attribute` steps.
      def fetch(key: Text): Optional[Text] =
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

      def toMap: Map[Text, Text] = Map.of:
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

      // Named `eachPair` (not `each`): rudiments' generic one-parameter `each` extension
      // otherwise wins resolution and rejects the two-parameter lambda.
      def eachPair(action: (Text, Text) => Unit): Unit =
        val a = storage(attrs)
        var i = 0

        while i < a.length do
          action(a(i).asInstanceOf[Text], a(i + 1).asInstanceOf[Text])
          i += 2

      def foreachPair(action: (Text, Text) => Unit): Unit = eachPair(action)

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
        if other.stdlib.isEmpty then attrs else attrs ++ Attributes.from(other)

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

  // ── Staged parser generation ──────────────────────────────────────────────
  // Generates a monomorphic `Xml.Parsable` for a case class: field values
  // live in typed locals, child elements dispatch through packed-`Long`
  // literal comparisons (with a linear text step for unpackable names),
  // builtin primitives read through direct static calls, `@attribute` fields
  // fill from the open tag before the child loop, and the record is built by
  // a direct constructor call — no `Array[Any]` buffer, no `Mirror`, no
  // per-field boxing. Field types beyond the builtins resolve through
  // `Xml.Field` instances (summoned at expansion, initialized lazily so
  // recursive references stay deferred), so semantics — wire names,
  // gathering of repeatable fields, first-match-wins duplicates, defaults,
  // absents, error foci — are identical to `ParsableDerivation`. Like the
  // derived engine (and unlike jacinta's staged parser), no `Tactic` or
  // `Foci` is captured at expansion: both come from the reader at the read
  // site. The body is assembled from reflection trees with only small,
  // immediately-scoped quotes: chained quotes carrying `Type` bindings
  // through closures are unpicklable.

  private enum StagedKind:
    case IntK, LongK, DoubleK, FloatK, BooleanK, TextK, StringK, InstanceK

  def stagedParsable[value: Type](renames: Expr[Map[Text, Text]])(using Quotes)
  :   Expr[value is Xml.Parsable] =

    import quotes.reflect.*
    import StagedKind.*

    val tpe = TypeRepr.of[value].dealias

    val classSymbol = tpe.classSymbol.getOrElse:
      report.errorAndAbort("xylophone: staged parsing requires a case class")

    if !classSymbol.flags.is(Flags.Case) then
      report.errorAndAbort
        ("xylophone: staged parsing requires a case class; sums and other types use "+
          "`Xml.Parsable.derived`")

    if classSymbol.owner.isTerm then
      report.errorAndAbort
        ("xylophone: staged parsing requires a top-level or object-nested case class; "+
          "method-local classes use `Xml.Parsable.derived`")

    val ctor = classSymbol.primaryConstructor

    if ctor.paramSymss.filterNot(_.exists(_.isTypeParam)).length != 1 then
      report.errorAndAbort
        ("xylophone: staged parsing requires a single parameter list; use "+
          "`Xml.Parsable.derived`")

    val fields = classSymbol.caseFields
    val arity = fields.length
    val fieldNames: List[String] = fields.map(_.name)
    val fieldTypes: List[TypeRepr] = fields.map { field => tpe.memberType(field).dealias }

    def kindOf(fieldType: TypeRepr): StagedKind =
      if fieldType =:= TypeRepr.of[Int] then IntK
      else if fieldType =:= TypeRepr.of[Long] then LongK
      else if fieldType =:= TypeRepr.of[Double] then DoubleK
      else if fieldType =:= TypeRepr.of[Float] then FloatK
      else if fieldType =:= TypeRepr.of[Boolean] then BooleanK
      else if fieldType =:= TypeRepr.of[Text] then TextK
      else if fieldType =:= TypeRepr.of[String] then StringK
      else InstanceK

    val kinds: List[StagedKind] = fieldTypes.map(kindOf)

    def annotationsOf(index: Int): List[Term] =
      val params = ctor.paramSymss.flatten.filterNot(_.isTypeParam)
      params(index).annotations ++ fields(index).annotations

    // `@attribute` fields fill from the open tag and never match a child.
    val attrFlags: List[Boolean] = List.range(0, arity).map: index =>
      annotationsOf(index).exists { annotation => annotation.tpe <:< TypeRepr.of[Xml.attribute] }

    // Names compile to literal packed-word comparisons when no `@name`
    // annotation can rename them (renames resolve at runtime, so annotated
    // classes keep the linear text step for every child). A field name that
    // cannot pack still parses: it always arrives as `NameOpaque` and takes
    // the general text step, which matches all fields by string.
    val literalKeys: Boolean =
      !List.range(0, arity).exists: index =>
        annotationsOf(index).exists { annotation =>
          annotation.tpe <:< TypeRepr.of[adversaria.name[?]] }

    def packedName(index: Int): Option[(Long, Long)] =
      val name = fieldNames(index)
      val length = name.length

      val packs = length > 0 && length <= 16 &&
        name.forall { char => char >= '!' && char < 127 }

      if !packs then None else
        var low = 0L
        var high = 0L
        var position = 0

        while position < length do
          val byte = name.charAt(position).toLong & 0xFF
          if position < 8 then low |= byte << (position*8)
          else high |= byte << ((position - 8)*8)
          position += 1

        Some((low, high))

    val packedNames: List[Option[(Long, Long)]] = List.range(0, arity).map(packedName)

    def summonField(index: Int): Expr[Xml.Field | Null] =
      if kinds(index) != InstanceK then '{null}
      else fieldTypes(index).asType match
        case '[fieldType] =>
          Expr.summon[fieldType is Xml.Field].getOrElse:
            report.errorAndAbort
              (s"xylophone: no Xml.Field instance for field ${fieldNames(index)}: "+
                fieldTypes(index).show)

    def declaredDefault(index: Int): Expr[Any] = fieldTypes(index).asType match
      case '[fieldType] =>
        '{ wisteria.internal.default[value, fieldType](${Expr(index)}): Any }

    def zero(fieldType: TypeRepr): Term =
      if fieldType =:= TypeRepr.of[Int] then Literal(IntConstant(0))
      else if fieldType =:= TypeRepr.of[Long] then Literal(LongConstant(0L))
      else if fieldType =:= TypeRepr.of[Double] then Literal(DoubleConstant(0.0))
      else if fieldType =:= TypeRepr.of[Float] then Literal(FloatConstant(0.0f))
      else if fieldType =:= TypeRepr.of[Boolean] then Literal(BooleanConstant(false))
      else fieldType.asType match
        case '[fieldType] => '{ null.asInstanceOf[fieldType] }.asTerm

    def construct(arguments: List[Term]): Term =
      val typeArguments = tpe match
        case AppliedType(_, applied) => applied
        case _                       => Nil

      val newTerm = Select(New(Inferred(tpe)), ctor)

      val applied =
        if typeArguments.isEmpty then newTerm
        else TypeApply(newTerm, typeArguments.map { argument => Inferred(argument) })

      Apply(applied, arguments)

    def body
      ( reader:      Expr[XmlReader],
        keys:        Expr[IArray[String]],
        attrs:       Expr[IArray[Boolean]],
        instances:   Expr[IArray[Xml.Field | Null]],
        repeatables: Expr[IArray[Boolean]],
        fallbacks:   Expr[IArray[Any]] )
    :   Expr[value] =

      val owner = Symbol.spliceOwner
      val bufferType = TypeRepr.of[scala.collection.mutable.ListBuffer[Any] | Null]

      // The read-site capabilities, bound once per record.
      val fociSymbol =
        Symbol.newVal(owner, "foci", TypeRepr.of[Foci[Xml.Focus]], Flags.EmptyFlags,
          Symbol.noSymbol)

      val tacticSymbol =
        Symbol.newVal(owner, "tactic", TypeRepr.of[Tactic[XmlError]], Flags.EmptyFlags,
          Symbol.noSymbol)

      val fociDef = ValDef(fociSymbol, Some('{ $reader.foci }.asTerm))
      val tacticDef = ValDef(tacticSymbol, Some('{ $reader.errorTactic }.asTerm))
      val foci = Ref(fociSymbol).asExprOf[Foci[Xml.Focus]]
      val tactic = Ref(tacticSymbol).asExprOf[Tactic[XmlError]]

      val slots = List.range(0, arity).map: index =>
        Symbol.newVal(owner, "slot"+index, fieldTypes(index), Flags.Mutable, Symbol.noSymbol)

      val seens = List.range(0, arity).map: index =>
        Symbol.newVal(owner, "seen"+index, TypeRepr.of[Boolean], Flags.Mutable, Symbol.noSymbol)

      // Occurrence buffers for the fields that may gather (repeatable
      // instances), allocated lazily on the first occurrence.
      val buffers: List[Option[Symbol]] = List.range(0, arity).map: index =>
        if kinds(index) != InstanceK then None else
          Some(Symbol.newVal(owner, "gather"+index, bufferType, Flags.Mutable, Symbol.noSymbol))

      val slotDefs = List.range(0, arity).map: index =>
        ValDef(slots(index), Some(zero(fieldTypes(index))))

      val seenDefs = List.range(0, arity).map: index =>
        ValDef(seens(index), Some(Literal(BooleanConstant(false))))

      val bufferDefs = List.range(0, arity).flatMap: index =>
        buffers(index).map: symbol =>
          ValDef(symbol, Some('{ null }.asTerm))

      val unit = Literal(UnitConstant())

      // `@attribute` fields first: they are available from the element's
      // open tag, before any child is consumed.
      val attributesSymbol =
        Symbol.newVal(owner, "attributes", TypeRepr.of[Attributes], Flags.EmptyFlags,
          Symbol.noSymbol)

      val attributesDef = ValDef(attributesSymbol, Some('{ $reader.attributes() }.asTerm))
      val attributes = Ref(attributesSymbol).asExprOf[Attributes]

      val attributeSteps: List[Term] = List.range(0, arity).flatMap: index =>
        if !attrFlags(index) then None else fieldTypes(index).asType match
          case '[fieldType] =>
            val keyText: Expr[Text] = '{ $keys(${Expr(index)}).tt }

            val valueSymbol =
              Symbol.newVal(owner, "attr"+index, TypeRepr.of[Optional[Text]], Flags.EmptyFlags,
                Symbol.noSymbol)

            val valueDef =
              ValDef(valueSymbol, Some('{ Attributes.fetch($attributes)($keyText) }.asTerm))

            val valueRef = Ref(valueSymbol).asExprOf[Optional[Text]]

            val read: Expr[fieldType] = kinds(index) match
              case IntK     => '{ Xml.intParsable.attribute($valueRef.vouch)(using $tactic, $foci) }
                               . asExprOf[fieldType]
              case LongK    => '{ Xml.longParsable.attribute($valueRef.vouch)(using $tactic, $foci) }
                               . asExprOf[fieldType]
              case DoubleK  => '{ Xml.doubleParsable.attribute($valueRef.vouch)(using $tactic, $foci) }
                               . asExprOf[fieldType]
              case FloatK   => '{ Xml.floatParsable.attribute($valueRef.vouch)(using $tactic, $foci) }
                               . asExprOf[fieldType]
              case BooleanK => '{ Xml.booleanParsable.attribute($valueRef.vouch)(using $tactic, $foci) }
                               . asExprOf[fieldType]
              case TextK    => '{ $valueRef.vouch }.asExprOf[fieldType]
              case StringK  => '{ $valueRef.vouch.s }.asExprOf[fieldType]

              case InstanceK =>
                '{
                  $instances(${Expr(index)}).asInstanceOf[fieldType is Xml.Field]
                  . attribute($valueRef.vouch)(using $tactic, $foci)
                }

            val focused: Term =
              '{ Xml.Parsable.focusing($foci, $keyText)($read) }.asTerm

            Some:
              Block
                ( List(valueDef),
                  If
                    ( '{ $valueRef.present }.asTerm,
                      Block
                        ( List
                            ( Assign(Ref(slots(index)), focused),
                              Assign(Ref(seens(index)), Literal(BooleanConstant(true))) ),
                          unit ),
                      unit ) )

      // One dispatch arm per child-matching field: read the value (with
      // focus bookkeeping), honoring the derived engine's semantics — a
      // repeatable field gathers every occurrence, a non-repeatable one
      // keeps its first and skips the rest.
      val arms = List.range(0, arity).map: index =>
        val keyText: Expr[Text] = '{ $keys(${Expr(index)}).tt }

        def firstWins(read: Term): Term =
          If
            ( Ref(seens(index)),
              '{ $reader.skipElement() }.asTerm,
              Block
                ( List
                    ( Assign(Ref(slots(index)), read),
                      Assign(Ref(seens(index)), Literal(BooleanConstant(true))) ),
                  unit ) )

        val rhs: Term = fieldTypes(index).asType match
          case '[fieldType] =>
            kinds(index) match
              case IntK =>
                firstWins:
                  '{ Xml.Parsable.focusing($foci, $keyText)(Xml.intParsable.parse($reader)) }
                  . asTerm

              case LongK =>
                firstWins:
                  '{ Xml.Parsable.focusing($foci, $keyText)(Xml.longParsable.parse($reader)) }
                  . asTerm

              case DoubleK =>
                firstWins:
                  '{ Xml.Parsable.focusing($foci, $keyText)(Xml.doubleParsable.parse($reader)) }
                  . asTerm

              case FloatK =>
                firstWins:
                  '{ Xml.Parsable.focusing($foci, $keyText)(Xml.floatParsable.parse($reader)) }
                  . asTerm

              case BooleanK =>
                firstWins:
                  '{ Xml.Parsable.focusing($foci, $keyText)(Xml.booleanParsable.parse($reader)) }
                  . asTerm

              case TextK =>
                firstWins:
                  '{
                    Xml.Parsable.focusing($foci, $keyText):
                      $reader.text().or { $reader.fault(); t"" }
                  }.asTerm

              case StringK =>
                firstWins:
                  '{
                    Xml.Parsable.focusing($foci, $keyText):
                      ($reader.text().or { $reader.fault(); t"" }).s
                  }.asTerm

              case InstanceK =>
                val bufferRef = Ref(buffers(index).get)

                val bufferExpr =
                  bufferRef.asExprOf[scala.collection.mutable.ListBuffer[Any] | Null]

                val ensure: Term =
                  If
                    ( '{ $bufferExpr == null }.asTerm,
                      Assign
                        ( bufferRef,
                          '{ scala.collection.mutable.ListBuffer.empty[Any] }.asTerm ),
                      unit )

                val append: Term =
                  '{
                    $bufferExpr.asInstanceOf[scala.collection.mutable.ListBuffer[Any]].addOne
                      ( Xml.Parsable.focusing($foci, $keyText):
                          Xml.Parsable.parseElement
                            ( $instances(${Expr(index)}).asInstanceOf[Xml.Parsing], $reader ) )
                  }.asTerm

                val read: Term =
                  '{
                    Xml.Parsable.focusing($foci, $keyText):
                      $instances(${Expr(index)}).asInstanceOf[fieldType is Xml.Field]
                      . parse($reader)
                  }.asTerm

                If
                  ( '{ $repeatables(${Expr(index)}) }.asTerm,
                    Block(List(ensure, append), unit),
                    firstWins(read) )

        CaseDef(Literal(IntConstant(index)), None, rhs)

      val fallthrough = CaseDef(Wildcard(), None, '{ $reader.skipElement() }.asTerm)

      // The child loop. With literal names, each step compares the packed
      // words against the field names as immediate constants, resolving an
      // opaque name through the linear text step; otherwise (runtime
      // renames) every child resolves through the text step.
      val run = Symbol.newVal(owner, "run", TypeRepr.of[Boolean], Flags.Mutable, Symbol.noSymbol)
      val word = Symbol.newVal(owner, "word", TypeRepr.of[Long], Flags.EmptyFlags, Symbol.noSymbol)
      val high = Symbol.newVal(owner, "high", TypeRepr.of[Long], Flags.EmptyFlags, Symbol.noSymbol)
      val found = Symbol.newVal(owner, "found", TypeRepr.of[Int], Flags.EmptyFlags, Symbol.noSymbol)
      val wordRef = Ref(word).asExprOf[Long]
      val highRef = Ref(high).asExprOf[Long]

      def chain(index: Int): Term =
        if index == arity then Literal(IntConstant(-1))
        else if attrFlags(index) then chain(index + 1)
        else packedNames(index) match
          case None => chain(index + 1)

          case Some((low, highWord)) =>
            If
              ( '{ $wordRef == ${Expr(low)} && $highRef == ${Expr(highWord)} }.asTerm,
                Literal(IntConstant(index)),
                chain(index + 1) )

      val textStep: Term =
        '{ Xml.Parsable.childIndex($keys, $attrs, $reader.childLabel) }.asTerm

      val resolve: Term =
        if literalKeys then
          If
            ( '{ $wordRef == XmlReader.NameOpaque }.asTerm,
              textStep,
              Block(List(ValDef(high, Some('{ $reader.childWordHigh }.asTerm))), chain(0)) )
        else textStep

      val step: Term =
        Block
          ( List(ValDef(word, Some('{ $reader.childWord() }.asTerm))),
            If
              ( '{ $wordRef == XmlReader.NameEnd }.asTerm,
                Assign(Ref(run), Literal(BooleanConstant(false))),
                Block
                  ( List(ValDef(found, Some(resolve))),
                    Match(Ref(found), arms :+ fallthrough) ) ) )

      val loop: List[Statement] =
        List(ValDef(run, Some(Literal(BooleanConstant(true)))), While(Ref(run), step))

      // Fields whose names never arrived — and repeatable fields, whose
      // collection is always built from the gathered occurrences (zero
      // occurrences build the empty collection; a repeatable field never
      // consults the declared default), exactly as the derived engine does.
      val absents: List[Term] = List.range(0, arity).map: index =>
        fieldTypes(index).asType match
          case '[fieldType] =>
            val keyText: Expr[Text] = '{ $keys(${Expr(index)}).tt }

            val onAbsent: Expr[fieldType] = kinds(index) match
              case InstanceK =>
                '{
                  $instances(${Expr(index)}).asInstanceOf[fieldType is Xml.Field]
                  . absent()(using $tactic, $foci)
                }

              case IntK     => '{ Xml.Parsable.missing[Int](0)(using $tactic) }
                               . asExprOf[fieldType]
              case LongK    => '{ Xml.Parsable.missing[Long](0L)(using $tactic) }
                               . asExprOf[fieldType]
              case DoubleK  => '{ Xml.Parsable.missing[Double](0.0)(using $tactic) }
                               . asExprOf[fieldType]
              case FloatK   => '{ Xml.Parsable.missing[Float](0.0f)(using $tactic) }
                               . asExprOf[fieldType]
              case BooleanK => '{ Xml.Parsable.missing[Boolean](false)(using $tactic) }
                               . asExprOf[fieldType]
              case TextK    => '{ Xml.Parsable.missing[Text](t"")(using $tactic) }
                               . asExprOf[fieldType]
              case StringK  => '{ Xml.Parsable.missing[String]("")(using $tactic) }
                               . asExprOf[fieldType]

            val resolveAbsent: Term =
              Assign
                ( Ref(slots(index)),
                  '{
                    val declared = $fallbacks(${Expr(index)}).asInstanceOf[Optional[fieldType]]

                    if !declared.absent then declared.asInstanceOf[fieldType]
                    else Xml.Parsable.focusing($foci, $keyText)($onAbsent)
                  }.asTerm )

            val whenUnseen: Term =
              If('{ !${Ref(seens(index)).asExprOf[Boolean]} }.asTerm, resolveAbsent, unit)

            kinds(index) match
              case InstanceK =>
                val bufferExpr =
                  Ref(buffers(index).get)
                  . asExprOf[scala.collection.mutable.ListBuffer[Any] | Null]

                val gatherFinish: Term =
                  Assign
                    ( Ref(slots(index)),
                      '{
                        Xml.Parsable.focusing($foci, $keyText):
                          Xml.Parsable.gathered[fieldType]
                            ( $instances(${Expr(index)}).asInstanceOf[Xml.Parsing],
                              $bufferExpr match
                                case null   => proscenium.Nil
                                case buffer => proscenium.List.of(buffer.toList) )
                      }.asTerm )

                If('{ $repeatables(${Expr(index)}) }.asTerm, gatherFinish, whenUnseen)

              case _ =>
                whenUnseen

      Block
        ( fociDef :: tacticDef :: slotDefs ::: seenDefs ::: bufferDefs
            ::: (attributesDef :: attributeSteps)
            ::: loop
            ::: absents,
          construct(slots.map { slot => Ref(slot) }) )
      . asExprOf[value]

    // The absent-build: what a missing (or wrong-shape) occurrence of this
    // record yields when no user-supplied `Default` collapses it — every
    // sub-field takes its declared default or raises at its own focus,
    // mirroring the derived engine's `absentBuild`.
    def absentBody(tactic: Expr[Tactic[XmlError]], foci: Expr[Foci[Xml.Focus]])
      ( keys:        Expr[IArray[String]],
        instances:   Expr[IArray[Xml.Field | Null]],
        repeatables: Expr[IArray[Boolean]],
        fallbacks:   Expr[IArray[Any]] )
    :   Expr[value] =

      val arguments: List[Term] = List.range(0, arity).map: index =>
        fieldTypes(index).asType match
          case '[fieldType] =>
            val keyText: Expr[Text] = '{ $keys(${Expr(index)}).tt }

            val onAbsent: Expr[fieldType] = kinds(index) match
              case InstanceK =>
                '{
                  $instances(${Expr(index)}).asInstanceOf[fieldType is Xml.Field]
                  . absent()(using $tactic, $foci)
                }

              case IntK     => '{ Xml.Parsable.missing[Int](0)(using $tactic) }
                               . asExprOf[fieldType]
              case LongK    => '{ Xml.Parsable.missing[Long](0L)(using $tactic) }
                               . asExprOf[fieldType]
              case DoubleK  => '{ Xml.Parsable.missing[Double](0.0)(using $tactic) }
                               . asExprOf[fieldType]
              case FloatK   => '{ Xml.Parsable.missing[Float](0.0f)(using $tactic) }
                               . asExprOf[fieldType]
              case BooleanK => '{ Xml.Parsable.missing[Boolean](false)(using $tactic) }
                               . asExprOf[fieldType]
              case TextK    => '{ Xml.Parsable.missing[Text](t"")(using $tactic) }
                               . asExprOf[fieldType]
              case StringK  => '{ Xml.Parsable.missing[String]("")(using $tactic) }
                               . asExprOf[fieldType]

            val declared: Expr[fieldType] =
              '{
                val declared = $fallbacks(${Expr(index)}).asInstanceOf[Optional[fieldType]]

                if !declared.absent then declared.asInstanceOf[fieldType]
                else Xml.Parsable.focusing($foci, $keyText)($onAbsent)
              }

            val argument: Expr[fieldType] = kinds(index) match
              case InstanceK =>
                '{
                  if $repeatables(${Expr(index)}) then
                    Xml.Parsable.focusing($foci, $keyText):
                      Xml.Parsable.gathered[fieldType]
                        ( $instances(${Expr(index)}).asInstanceOf[Xml.Parsing], proscenium.Nil )
                  else $declared
                }

              case _ =>
                declared

            argument.asTerm

      construct(arguments).asExprOf[value]

    val nameExprs = fieldNames.map { name => Expr(name) }
    val attrExprs = attrFlags.map { flag => Expr(flag) }
    val instanceExprs = List.range(0, arity).map(summonField)
    val fallbackExprs = List.range(0, arity).map(declaredDefault)

    // A user-supplied `Default[value]` collapses a missing nested value to a
    // single error, exactly as the derived engine's `fallback` does.
    val defaultExpr: Expr[Optional[() => value]] =
      Expr.summon[Default[value]] match
        case Some(default) => '{ Optional({ () => $default() }: () => value) }
        case None          => '{ Unset }

    '{
      // Sealed per the codec-thunk pattern, like the derived instances: the
      // field parsers the instance array resolves may capture resolution-
      // scoped capabilities (the AST bridge does). The instance and default
      // arrays are single lazy vals, so recursive self-references stay
      // deferred until the first parse.
      caps.unsafe.unsafeAssumePure:
        val keys: IArray[String] =
          Xml.Parsable.wireNames(IArray[String](${Varargs(nameExprs)}*), $renames)

        val attrs: IArray[Boolean] = IArray[Boolean](${Varargs(attrExprs)}*)

        lazy val instances: IArray[Xml.Field | Null] = IArray(${Varargs(instanceExprs)}*)

        lazy val repeatables: IArray[Boolean] =
          instances.map { instance => instance != null && Xml.Parsable.repeats(instance) }

        lazy val fallbacks: IArray[Any] = IArray[Any](${Varargs(fallbackExprs)}*)
        val fallback: Optional[() => value] = $defaultExpr

        new Xml.Parsable:
          type Self = value

          def parse(reader: XmlReader^): value =
            ${
              body
                ( '{reader}, '{keys}, '{attrs}, '{instances}, '{repeatables}, '{fallbacks} )
            }

          override def absent()(using tactic: Tactic[XmlError], foci: Foci[Xml.Focus]): value =
            raise(XmlError())

            fallback.lay
              ( ${
                  absentBody('{tactic}, '{foci})
                    ('{keys}, '{instances}, '{repeatables}, '{fallbacks})
                } )
              { instantiate => instantiate() }
    }
