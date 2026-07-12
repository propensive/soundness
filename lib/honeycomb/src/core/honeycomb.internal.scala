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
package honeycomb

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
    ( scrutinee: Expr[Html] )
  :   Macro[Extrapolation[Html]] =

    import quotes.reflect.*
    import doms.html.whatwg

    def recur[tuple: Type](strings: List[String]): List[String] = Type.of[tuple] match
      case '[head *: tail] => recur[tail](TypeRepr.of[head].literal[String].vouch :: strings)
      case _               => strings

    val parts = recur[parts](Nil)

    def intersect(parts: List[String], repr: TypeRepr = TypeRepr.of[Nothing]): TypeRepr =
      parts match
        case head :: tail => intersect(tail, OrType(repr, ConstantType(StringConstant(head))))
        case Nil          => repr

    abortive:
      var holes: Map[Ordinal, Html.Hole] = Map()
      def capture(ordinal: Ordinal, hole: Html.Hole) = holes = holes.updated(ordinal, hole)

      val html: Html =
        Html.parse(Iterator(parts.mkString("\u0000").tt), whatwg.generic, capture(_, _))

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
            types ::= TypeRepr.of[Map[Text, Optional[Text]]]
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

      def descend
        ( array: Expr[Array[Any]], pattern: Html, scrutinee: Expr[Html], expr: Expr[Boolean] )
      :   Expr[Boolean] =

        pattern match
          case Comment("\u0000") =>
            index += 1
            iterator.next()
            types ::= TypeRepr.of[Text]

            ' {
                $expr &&
                  $scrutinee.isInstanceOf[Comment] &&
                  { $array(${Expr(index)}) = $scrutinee.asInstanceOf[Comment].text; true }
              }

          case TextNode("\u0000") =>
            index += 1

            iterator.next() match
              case Html.Hole.Node(label) =>
                types ::= whatwg.elements(label).lay(TypeRepr.of[Node]): tag =>
                  intersect(tag.admissible.map(_.s).to(List)).asType.absolve match
                    case '[type children <: Label; children] => TypeRepr.of[Node of children]

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
            halt(m"cannot match against a document type declaration")

          case Element("\u0000", _, _, _) =>
            index += 1

            iterator.next() match
              case Html.Hole.Element(label) =>
                types ::= whatwg.elements(label).lay(TypeRepr.of[Element]): tag =>
                  intersect(tag.admissible.map(_.s).to(List)).asType.absolve match
                    case '[type children <: Label; children] => TypeRepr.of[Element of children]

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
            val extracts = new Array[Any](${Expr(holes.size)})
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

  def interpolator[parts <: Tuple: Type, origins <: Tuple: Type]
    ( insertions0: Expr[Seq[Any]] )
  :   Macro[Html] =

    import quotes.reflect.*
    import doms.html.whatwg
    import Html.Hole

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
    // Position. Uses Interpolation.buildMapping so escape sequences in the
    // source (\n, \t, \uHHHH, $$, etc.) correctly resolve to the longer
    // source span. The Scala compiler's lit.pos.end is unreliable for parts
    // containing $$, so we walk forward from lit.pos.start instead.
    val sourceFile = Position.ofMacroExpansion.sourceFile
    val macroPos = Position.ofMacroExpansion

    val sourceContent: Optional[String] = sourceFile.content match
      case Some(s: String) => s
      case _               => Unset

    val perPart: IndexedSeq[((String, Int), Int -> Int)] =
      parts.zip(partOrigins).map: (part, origin) =>
        val (srcStart, _) = origin

        // Sealed: the mapping closes over only strings, but its inferred fresh
        // capture would leak into the collected sequence.
        val mapping: Int -> Int = caps.unsafe.unsafeAssumePure:
          sourceContent.lay[Int => Int](identity(_)): content =>
            if srcStart > 0 && srcStart < content.length then
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

    var holes: Map[Ordinal, Html.Hole] = Map()
    def capture(ordinal: Ordinal, hole: Hole) = holes = holes.updated(ordinal, hole)

    // Custom HaltTactic: translate parser ParseError positions to source-file ranges.
    val html: Html =
      given diagnostics: Diagnostics = Diagnostics.omit

      given parseTactic: HaltTactic[ParseError, Html] = new HaltTactic[ParseError, Html]:
        override def abort(error: Diagnostics ?=> ParseError): Nothing =
          val pe = error
          val off = pe.position.offset.or(0)
          val length = pe.position.length.or(0)
          halt(pe.labelled, translateOffset(off, length))

      Html.parse(Iterator(parts.mkString("\u0000").tt), whatwg.generic, capture(_, _))

    abortive:

      val iterator: Iterator[Expr[Any]] =
        holes.to(List).sortBy(_(0)).map(_(1)).zip(insertions).map: (hole, expr) =>
          expr.absolve match
            case '{$expr: value} => hole match
              case Hole.Attribute(tag, attribute) =>
                ConstantType(StringConstant(tag.s)).asType.absolve match
                  case '[tag] => ConstantType(StringConstant(attribute.s)).asType.absolve match
                    case '[attribute] =>
                      Expr.summon[attribute is Attribute in Whatwg on (? >: tag)]
                      . orElse(Expr.summon[attribute is Attribute in Whatwg]) match
                        case
                          Some
                            ( ' {
                                  type result
                                  $typeclass: Attribute { type Topic = result }
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
                          halt
                            ( m"the attribute $attribute cannot be used on the element <$tag>",
                              expr.asTerm.underlyingArgument.pos )

              case Hole.Element(tag) =>
                ConstantType(StringConstant(tag.s)).asType.absolve match
                  case '[tag] => Expr.summon[(? >: value) is Renderable in (? >: tag)] match
                    case Some('{$renderable: Renderable}) =>
                      // Widened eagerly: joining the branch types under capture
                      // checking decorates the summoned evidence's skolem, which
                      // then fails to unify (the macro only needs `Expr[Any]`).
                      ('{$renderable.render($expr)}: Expr[Any])

                    case _ =>
                      halt
                        ( m"""
                            a value of ${TypeRepr.of[value].show} is not renderable inside a <$tag>
                            element
                          """,
                          expr.asTerm.underlyingArgument.pos )

              case Hole.Node(tag) =>
                ConstantType(StringConstant(tag.s)).asType.absolve match
                  case '[tag] => Expr.summon[(? >: value) is Renderable in (? >: tag)] match
                    case Some('{$renderable: Renderable}) =>
                      // Widened eagerly: joining the branch types under capture
                      // checking decorates the summoned evidence's skolem, which
                      // then fails to unify (the macro only needs `Expr[Any]`).
                      ('{$renderable.render($expr)}: Expr[Any])

                    case _ =>
                      Expr.summon[(? >: value) is Showable] match
                        case Some('{$showable: Showable}) =>
                          '{TextNode($showable.text($expr))}

                        case _ =>
                          halt
                            ( m"""
                                a value of ${TypeRepr.of[value].show} is not renderable or
                                showable inside a <$tag> element
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
                // A reflection-level test rather than a quoted type pattern
                // (`case '[Map[Text, Optional[Text]]]`), which fails to unify
                // against the capture-decorated scrutinee under capture checking.
                if TypeRepr.of[value] <:< TypeRepr.of[Map[Text, Optional[Text]]] then expr else
                  halt
                    ( m"""
                        only a ${TypeRepr.of[Map[Text, Optional[Text]]].show} can be applied in a
                        tag body
                      """,
                      expr.asTerm.underlyingArgument.pos )

        . iterator

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
            '{IArray(${Expr.ofList(children.flatMap(serialize(_)).asInstanceOf[IArray[Expr[Node]]].to(List))}*)}

          List('{Element(${Expr(label)}, $attrs, $elements, ${Expr(foreign)})})

        case Doctype(text) =>
          if text.contains(t"\u0000")
          then halt(m"cannot substitute into a document type declaration")
          else List('{Doctype(${Expr(text)})})

        case Comment(text) =>
          val parts = text.cut(t"\u0000").map(_.s)

          def recur(parts: List[String], expr: Expr[String]): Expr[String] = parts match
            case Nil => expr

            case head :: tail =>
              recur(tail, '{$expr+${iterator.next().asExprOf[Text]}+${Expr(head)}})

          val content = recur(parts.tail, Expr(parts.head))

          List('{Comment($content.tt)})

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

      def resultType(html: Html): Set[String] = html match
        case TextNode(_)           => Set("#text")
        case Element(tag, _, _, _) => Set(tag.s)
        case Fragment(values*)     => values.to(Set).flatMap(resultType(_))
        case Comment(_)            => Set()
        case Doctype(_)            => Set()

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
                                  // Lift the key as a `String` and reconstruct the `Text` at
                                  // runtime (`.tt`): splicing a lifted opaque `Text` literal into a
                                  // capture-checked consumer would demand a spurious `Text^` on it.
                                  '{$converter.attribute(${Expr(key)}.tt, $value)}

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

    val attrsExpr = '{Attributes.from($presets ++ ${Expr.ofList(attributes)}.compact.to(Map))}
    '{$tag.node($attrsExpr)}.asExprOf[result]

  opaque type Attributes = IArray[String | Null]

  object Attributes:
    // Sealed: fresh `IArray`s are immutable; fresh-ness is the opaque-Array
    // artifact, which would otherwise decorate every constructed `Attributes`
    // (and, through `Element`'s constructor, every `Tag`).
    val empty: Attributes = caps.unsafe.unsafeAssumePure(IArray.empty[String | Null])

    def apply(pairs: (Text, Optional[Text])*): Attributes =
      if pairs.isEmpty then empty else
        val n = pairs.length
        val arr = new Array[String | Null](n*2)
        var i = 0

        pairs.foreach: pair =>
          arr(i*2) = pair._1.s
          arr(i*2 + 1) = pair._2.lay(null: String | Null)(_.s)
          i += 1

        // Sealed: see `empty` — the opaque-Array artifact.
        caps.unsafe.unsafeAssumePure(arr.immutable(using Unsafe))

    def from(map: Map[Text, Optional[Text]]): Attributes =
      if map.isEmpty then empty else
        val n = map.size
        val arr = new Array[String | Null](n*2)
        var i = 0

        map.foreach: (k, v) =>
          arr(i*2) = k.s
          arr(i*2 + 1) = v.lay(null: String | Null)(_.s)
          i += 1

        // Sealed: see `empty` — the opaque-Array artifact.
        caps.unsafe.unsafeAssumePure(arr.immutable(using Unsafe))

    // Construct an `Attributes` directly from an interleaved `IArray`. The
    // caller guarantees the array's length is even and that every key slot
    // (even index) holds a non-null `String`. Used by the parser, which
    // assembles the interleaved array as it tokenizes attributes.
    private[honeycomb] inline def fromInterleaved(array: IArray[String | Null]): Attributes = array

    // Unwrap to the raw `Array[String | Null]` for hot-path internal access.
    // Safe within the package: the storage is shared but never mutated outside
    // construction.
    private[honeycomb] inline def storage(attrs: Attributes): Array[String | Null] =
      attrs.asInstanceOf[Array[String | Null]]

    extension (attrs: Attributes)
      inline def size: Int = attrs.length/2
      inline def isEmpty: Boolean = attrs.length == 0
      inline def nonEmpty: Boolean = attrs.length > 0
      inline def nil: Boolean = attrs.length == 0

      def apply(key: Text): Optional[Text] =
        val a = storage(attrs)
        val keyStr: String = key.s
        val n = a.length
        var i = 0

        while i < n do
          if a(i) == keyStr then
            val value = a(i + 1)
            return if value == null then Unset else value.asInstanceOf[Text]

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

        // Sealed: the iterator reads immutable storage through a read-only view.
        caps.unsafe.unsafeAssumePure:
          new Iterator[Text]:
            // Untracked: a plain index over immutable storage.
            @caps.unsafe.untrackedCaptures
            private var i: Int = 0
            def hasNext: Boolean = i < a.length

            def next(): Text =
              val k = a(i).asInstanceOf[Text]
              i += 2
              k

      def values: Iterator[Optional[Text]] =
        val a = storage(attrs)

        // Sealed: the iterator reads immutable storage through a read-only view.
        caps.unsafe.unsafeAssumePure:
          new Iterator[Optional[Text]]:
            // Untracked: a plain index over immutable storage.
            @caps.unsafe.untrackedCaptures
            private var i: Int = 1
            def hasNext: Boolean = i < a.length

            def next(): Optional[Text] =
              val v = a(i)
              i += 2
              if v == null then Unset else v.asInstanceOf[Text]

      def iterator: Iterator[(Text, Optional[Text])] =
        val a = storage(attrs)

        // Sealed: the iterator reads immutable storage through a read-only view.
        caps.unsafe.unsafeAssumePure:
          new Iterator[(Text, Optional[Text])]:
            // Untracked: a plain index over immutable storage.
            @caps.unsafe.untrackedCaptures
            private var i: Int = 0
            def hasNext: Boolean = i < a.length

            def next(): (Text, Optional[Text]) =
              val k = a(i).asInstanceOf[Text]
              val v = a(i + 1)
              i += 2
              (k, if v == null then Unset else v.asInstanceOf[Text])

      def toList: List[(Text, Optional[Text])] =
        val a = storage(attrs)
        val b = List.newBuilder[(Text, Optional[Text])]
        var i = 0

        while i < a.length do
          val v = a(i + 1)
          b += ((a(i).asInstanceOf[Text], if v == null then Unset else v.asInstanceOf[Text]))
          i += 2

        b.result()

      def toMap: Map[Text, Optional[Text]] =
        val a = storage(attrs)

        if a.length == 0 then ListMap.empty else
          val b = ListMap.newBuilder[Text, Optional[Text]]
          var i = 0

          while i < a.length do
            val v = a(i + 1)
            b += ((a(i).asInstanceOf[Text], if v == null then Unset else v.asInstanceOf[Text]))
            i += 2

          b.result()

      def map[B](f: ((Text, Optional[Text])) => B): Iterable[B] =
        val a = storage(attrs)
        val b = List.newBuilder[B]
        var i = 0

        while i < a.length do
          val v = a(i + 1)
          b += f((a(i).asInstanceOf[Text], if v == null then Unset else v.asInstanceOf[Text]))
          i += 2

        b.result()

      def foreach[U](f: ((Text, Optional[Text])) => U): Unit =
        val a = storage(attrs)
        var i = 0

        while i < a.length do
          val v = a(i + 1)
          f((a(i).asInstanceOf[Text], if v == null then Unset else v.asInstanceOf[Text]))
          i += 2

      def each(action: (Text, Optional[Text]) => Unit): Unit =
        val a = storage(attrs)
        var i = 0

        while i < a.length do
          val v = a(i + 1)
          action(a(i).asInstanceOf[Text], if v == null then Unset else v.asInstanceOf[Text])
          i += 2

      // O(N): finds and excludes the matching index, returning a new `Attributes`
      // whose backing IArray is two slots shorter. If the key is absent, returns
      // `attrs` unchanged so callers don't allocate gratuitously.
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
          val nu = new Array[String | Null](n - 2)
          if idx > 0 then jl.System.arraycopy(a, 0, nu, 0, idx)
          if idx < n - 2 then jl.System.arraycopy(a, idx + 2, nu, idx, n - 2 - idx)
          nu.immutable(using Unsafe)

      inline def `-`(key: Text): Attributes = removed(key)

      def `--`(others: Iterable[Text]): Attributes =
        if isEmpty then attrs else
          var result: Attributes = attrs

          others.foreach: k =>
            result = result.removed(k)

          result

      // Updates an existing key in place (preserving order) or appends a new pair
      // at the end. Always returns a new `Attributes` (the IArray is immutable).
      def updated(key: Text, value: Optional[Text]): Attributes =
        val a = storage(attrs)
        val keyStr: String = key.s
        val n = a.length
        var idx = -1
        var i = 0

        while idx < 0 && i < n do
          if a(i) == keyStr then idx = i
          i += 2

        if idx >= 0 then
          val nu = new Array[String | Null](n)
          jl.System.arraycopy(a, 0, nu, 0, n)
          nu(idx + 1) = value.lay(null: String | Null)(_.s)
          nu.immutable(using Unsafe)
        else
          val nu = new Array[String | Null](n + 2)
          jl.System.arraycopy(a, 0, nu, 0, n)
          nu(n) = keyStr
          nu(n + 1) = value.lay(null: String | Null)(_.s)
          nu.immutable(using Unsafe)

      // Combines two `Attributes`, with the right-hand side overriding duplicate
      // keys (matching `Map ++` semantics). Order: left's keys first (preserving
      // their order), then any new keys from the right.
      def `++`(other: Attributes): Attributes =
        val a = storage(attrs)
        val b = storage(other)

        if b.length == 0 then attrs
        else if a.length == 0 then other
        else
          val total = a.length + b.length
          val nu = new Array[String | Null](total)
          var written = 0
          var i = 0

          while i < a.length do
            val k = a(i).asInstanceOf[String]
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
            val k = b(j).asInstanceOf[String]
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
            val tu = new Array[String | Null](written)
            jl.System.arraycopy(nu, 0, tu, 0, written)
            tu.immutable(using Unsafe)

      def `++`(other: Map[Text, Optional[Text]]): Attributes =
        if other.isEmpty then attrs else attrs ++ Attributes.from(other)

      // Structural equality: same key/value pairs in the same order. Provided
      // explicitly because `Object.equals` on the underlying `IArray` would
      // give reference equality.
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
            val k = a(i).asInstanceOf[String]
            val va = a(i + 1)
            // Locate key in `b`.
            var j = 0
            var found = -1

            while found < 0 && j < n do
              if b(j) == k then found = j
              j += 2

            if found < 0 then ok = false
            else
              val vb = b(found + 1)
              if va != vb then ok = false

            i += 2

          ok

      def hashAttributes: Int =
        // Order-independent: XOR of (key.hash * 31 ^ value.hash) — matches the
        // previous `Attributes.hashCode` contract.
        val a = storage(attrs)
        var h = 0
        var i = 0

        while i < a.length do
          val k = a(i)
          val v = a(i + 1)
          val kh = if k == null then 0 else k.hashCode
          val vh = if v == null then Unset.hashCode else v.hashCode
          h = h ^ (kh*31 ^ vh)
          i += 2

        h

      def showAttributes: String =
        val a = storage(attrs)
        val sb = new jl.StringBuilder("Attributes(")
        var i = 0

        while i < a.length do
          if i > 0 then sb.append(", ")
          sb.append(a(i).asInstanceOf[String])
          sb.append(" -> ")
          val v = a(i + 1)
          sb.append(if v == null then Unset.toString else v.toString)
          i += 2

        sb.append(")").toString
