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
package stratiform

import scala.compiletime.*
import scala.quoted.*

import anticipation.*
import contextual.*
import contingency.*
import fulminate.*
import gigantism.*
import prepositional.*
import rudiments.*
import vacuous.*

// Compile-time machinery for the `tel"…"` interpolator and extractor.
// Mirrors jacinta.internal in shape: the static parts of a StringContext
// are joined with a marker character, parsed at compile time using the
// runtime TelParser, and the parsed AST is rebuilt as an Expr[Tel] with
// the marker positions filled by runtime hole values.
//
// Phase-2 scope: hole substitution is supported only at the atom-text
// position of a compound — `tel"name $alice"` works because `$alice`
// appears immediately after the `name` keyword's separating space. A
// compound-position spread or object/list spread is out of scope for
// this commit (and tracked in doc/spec-notes.md).

object internal:
  // The marker character interleaved between the static parts. Chosen so
  // it never appears in legitimate TEL source (U+0001 SOH is not a valid
  // sigil and is unlikely to occur in human-authored TEL).
  private final val Marker: Char = ''
  private final val MarkerString: String = Marker.toString

  private def hasMarker(text: Text): Boolean =
    var i = 0
    val s = text.s
    while i < s.length do
      if s.charAt(i) == Marker then return true
      i += 1

    false

  def interpolator[parts <: Tuple: Type, origins <: Tuple: Type]
       (insertions0: Expr[Seq[Any]])
  :     Macro[Tel] =
    import quotes.reflect.*

    // Tuple-type iteration: the contextual framework presents parts in
    // reverse-source order, so accumulating with cons gives source order
    // directly — no final reverse needed (mirrors jacinta.internal).
    def collectParts[tuple: Type](acc: List[String]): List[String] = Type.of[tuple] match
      case '[head *: tail] => collectParts[tail](TypeRepr.of[head].literal[String].vouch :: acc)
      case _               => acc

    val parts = collectParts[parts](Nil)
    val source: String = parts.mkString(MarkerString)
    val data: Data = IArray.from(source.getBytes("UTF-8").nn.iterator)

    val insertions: Seq[Expr[Any]] = insertions0.absolve match
      case Varargs(insertions) => insertions

    // Parse the assembled source at compile-time to validate syntax. If
    // parsing fails, halt the macro with the error code as the message.
    val document: Tel.Document =
      given Diagnostics = Diagnostics.omit
      given HaltTactic[TelError, Tel.Document] = new HaltTactic[TelError, Tel.Document]:
        override def abort(error: Diagnostics ?=> TelError): Nothing =
          halt(m"the tel\"…\" literal is invalid: ${error.message}")

      TelParser.parse(data)

    abortive:
      var holeIndex: Int = 0

      def consumeHole(): Expr[Any] =
        val expr = insertions(holeIndex)
        holeIndex += 1
        expr

      def encodeAtomText(expr: Expr[Any]): Expr[Text] = expr.absolve match
        case '{ $value: tpe } =>
          Expr.summon[(? >: tpe) is Encodable in Tel] match
            case Some('{ $enc: Encodable }) =>
              '{ $enc.encode($value).primaryAtom }

            case _ =>
              halt
                ( m"a value of ${TypeRepr.of[tpe].show} is not Encodable in Tel",
                  expr.asTerm.underlyingArgument.pos )

      // Replace marker occurrences in an atom's Text with the runtime
      // encoded value of the corresponding hole. The hole's encoded form
      // is the first inline atom's text of the produced Tel.
      def substituteMarker(text: Text): Expr[Text] =
        if !hasMarker(text) then '{ ${Expr(text.s)}.tt }
        else
          val s = text.s
          val pieces = s.split(MarkerString, -1).nn
          var result: Expr[String] = Expr(pieces(0).nn)
          var i = 1
          while i < pieces.length do
            val fragment = encodeAtomText(consumeHole())
            val partExpr = Expr(pieces(i).nn)
            result = '{ $result + $fragment.s + $partExpr }
            i += 1

          '{ $result.tt }

      def emitAtom(atom: Tel.Atom): Expr[Tel.Atom] = atom match
        case Tel.Atom.Inline(text, precedingSpaces) =>
          val textExpr = substituteMarker(text)
          val psExpr = Expr(precedingSpaces)
          '{ Tel.Atom.Inline($textExpr, $psExpr) }

        case Tel.Atom.Source(text) =>
          val textExpr = substituteMarker(text)
          '{ Tel.Atom.Source($textExpr) }

        case Tel.Atom.Literal(delimiter, text) =>
          val delimExpr = Expr(delimiter.s)
          val textExpr = substituteMarker(text)
          '{ Tel.Atom.Literal($delimExpr.tt, $textExpr) }

      def emitAtomsArray(atoms: IArray[Tel.Atom]): Expr[IArray[Tel.Atom]] =
        val list = atoms.toList.map(emitAtom)
        '{ IArray.from(${Expr.ofList(list)}) }

      def emitComment(c: Tel.Comment): Expr[Tel.Comment] =
        '{ Tel.Comment(${Expr(c.text.s)}.tt) }

      def emitTabulation(t: Tel.Tabulation): Expr[Tel.Tabulation] =
        val markers = Expr(t.markerOffsets.toList)
        val headings = Expr(t.headings.toList.map(_.s))
        '{ Tel.Tabulation(IArray.from(${markers}), IArray.from(${headings}.map(_.tt))) }

      def emitCompound(c: Tel.Compound): Expr[Tel.Compound] =
        val keywordExpr = Expr(c.keyword.s)
        val atomsExpr = emitAtomsArray(c.atoms)
        val remarkExpr: Expr[Optional[Text]] = c.remark match
          case unset: Unset.type => '{ Unset }
          case text: Text        => '{ ${Expr(text.s)}.tt: Optional[Text] }

        val childrenExpr = emitBlocks(c.children)
        '{ Tel.Compound(${keywordExpr}.tt, $atomsExpr, $remarkExpr, $childrenExpr) }

      def emitBlock(b: Tel.Block): Expr[Tel.Block] =
        val comments = '{ IArray.from(${Expr.ofList(b.comments.toList.map(emitComment))}) }
        val tab: Expr[Optional[Tel.Tabulation]] = b.tabulation match
          case unset: Unset.type   => '{ Unset }
          case t: Tel.Tabulation   => '{ ${emitTabulation(t)}: Optional[Tel.Tabulation] }

        val compounds =
          '{ IArray.from(${Expr.ofList(b.compounds.toList.map(emitCompound))}) }

        val tbl = Expr(b.trailingBlankLines)
        '{ Tel.Block($comments, $tab, $compounds, $tbl) }

      def emitBlocks(blocks: IArray[Tel.Block]): Expr[IArray[Tel.Block]] =
        '{ IArray.from(${Expr.ofList(blocks.toList.map(emitBlock))}) }

      val directiveExpr: Expr[Optional[Text]] = document.interpreterDirective match
        case unset: Unset.type => '{ Unset }
        case text: Text        => '{ ${Expr(text.s)}.tt: Optional[Text] }

      val pragmaExpr: Expr[Optional[Tel.Pragma]] = document.pragma match
        case unset: Unset.type =>
          '{ Unset }

        case p: Tel.Pragma =>
          val versionExpr = '{ (${Expr(p.version._1)}, ${Expr(p.version._2)}) }
          val schemaExpr: Expr[Optional[Text]] = p.schema match
            case unset: Unset.type => '{ Unset }
            case text: Text        => '{ ${Expr(text.s)}.tt: Optional[Text] }

          val sigilExpr: Expr[Optional[Char]] = p.sigil match
            case unset: Unset.type => '{ Unset }
            case c: Char           => '{ ${Expr(c)}: Optional[Char] }

          '{ Tel.Pragma($versionExpr, $schemaExpr, $sigilExpr): Optional[Tel.Pragma] }

      val lineEndingsExpr: Expr[Tel.LineEndings] = document.lineEndings match
        case Tel.LineEndings.Lf   => '{ Tel.LineEndings.Lf }
        case Tel.LineEndings.Crlf => '{ Tel.LineEndings.Crlf }

      val childrenExpr = emitBlocks(document.children)

      '{ Tel.make(Tel.Document($directiveExpr, $pragmaExpr, $lineEndingsExpr, $childrenExpr)) }
