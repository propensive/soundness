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

import scala.quoted.*

import anticipation.*
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
    ( insertions0: Expr[Seq[Any]] )
  :   Macro[Tel] =

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
        case '{$value: tpe} =>
          Expr.summon[(? >: tpe) is Encodable in Tel] match
            case Some('{$enc: Encodable}) =>
              '{$enc.encode($value).primaryAtom}

            case _ =>
              halt
                ( m"a value of ${TypeRepr.of[tpe].show} is not Encodable in Tel",
                  expr.asTerm.underlyingArgument.pos )

      // Replace marker occurrences in an atom's Text with the runtime
      // encoded value of the corresponding hole. The hole's encoded form
      // is the first inline atom's text of the produced Tel.
      def substituteMarker(text: Text): Expr[Text] =
        if !hasMarker(text) then '{${Expr(text.s)}.tt}
        else
          val s = text.s
          val pieces = s.split(MarkerString, -1).nn
          var result: Expr[String] = Expr(pieces(0).nn)
          var i = 1

          while i < pieces.length do
            val fragment = encodeAtomText(consumeHole())
            val partExpr = Expr(pieces(i).nn)
            result = '{$result + $fragment.s + $partExpr}
            i += 1

          '{$result.tt}

      def emitAtom(atom: Tel.Atom): Expr[Tel.Atom] = atom match
        case Tel.Atom.Inline(text, precedingSpaces) =>
          val textExpr = substituteMarker(text)
          val psExpr = Expr(precedingSpaces)
          '{Tel.Atom.Inline($textExpr, $psExpr)}

        case Tel.Atom.Source(text) =>
          val textExpr = substituteMarker(text)
          '{Tel.Atom.Source($textExpr)}

        case Tel.Atom.Literal(delimiter, text) =>
          val delimExpr = Expr(delimiter.s)
          val textExpr = substituteMarker(text)
          '{Tel.Atom.Literal($delimExpr.tt, $textExpr)}

      def emitAtomsArray(atoms: IArray[Tel.Atom]): Expr[IArray[Tel.Atom]] =
        val list = atoms.toList.map(emitAtom)
        '{IArray.from(${Expr.ofList(list)})}

      def emitComment(c: Tel.Comment): Expr[Tel.Comment] =
        '{Tel.Comment(${Expr(c.text.s)}.tt)}

      def emitTabulation(t: Tel.Tabulation): Expr[Tel.Tabulation] =
        val markers = Expr(t.markerOffsets.toList)
        val headings = Expr(t.headings.toList.map(_.s))
        '{Tel.Tabulation(IArray.from(${markers}), IArray.from(${headings}.map(_.tt)))}

      def emitCompound(c: Tel.Compound): Expr[Tel.Compound] =
        val keywordExpr = Expr(c.keyword.s)
        val atomsExpr = emitAtomsArray(c.atoms)

        val remarkExpr: Expr[Optional[Text]] = c.remark match
          case text: Text => '{${Expr(text.s)}.tt: Optional[Text]}
          case _          => '{Unset}

        val childrenExpr = emitBlocks(c.children)
        '{Tel.Compound(${keywordExpr}.tt, $atomsExpr, $remarkExpr, $childrenExpr)}

      def emitBlock(b: Tel.Block): Expr[Tel.Block] =
        val comments = '{IArray.from(${Expr.ofList(b.comments.toList.map(emitComment))})}

        val tab: Expr[Optional[Tel.Tabulation]] = b.tabulation match
          case t: Tel.Tabulation => '{${emitTabulation(t)}: Optional[Tel.Tabulation]}
          case _                 => '{Unset}

        val compounds =
          '{IArray.from(${Expr.ofList(b.compounds.toList.map(emitCompound))})}

        val tbl = Expr(b.trailingBlankLines)
        '{Tel.Block($comments, $tab, $compounds, $tbl)}

      def emitBlocks(blocks: IArray[Tel.Block]): Expr[IArray[Tel.Block]] =
        '{IArray.from(${Expr.ofList(blocks.toList.map(emitBlock))})}

      val directiveExpr: Expr[Optional[Text]] = document.interpreterDirective match
        case text: Text => '{${Expr(text.s)}.tt: Optional[Text]}
        case _          => '{Unset}

      val pragmaExpr: Expr[Optional[Tel.Pragma]] = document.pragma match
        case p: Tel.Pragma =>
          val versionExpr = '{(${Expr(p.version._1)}, ${Expr(p.version._2)})}

          val schemaExpr: Expr[Optional[Text]] = p.schema match
            case text: Text => '{${Expr(text.s)}.tt: Optional[Text]}
            case _          => '{Unset}

          val sigilExpr: Expr[Optional[Char]] = p.sigil match
            case c: Char => '{${Expr(c)}: Optional[Char]}
            case _       => '{Unset}

          '{Tel.Pragma($versionExpr, $schemaExpr, $sigilExpr): Optional[Tel.Pragma]}

        case _ =>
          '{Unset}


      val lineEndingsExpr: Expr[Tel.LineEndings] = document.lineEndings match
        case Tel.LineEndings.Lf   => '{Tel.LineEndings.Lf}
        case Tel.LineEndings.Crlf => '{Tel.LineEndings.Crlf}

      val childrenExpr = emitBlocks(document.children)

      '{Tel.make(Tel.Document($directiveExpr, $pragmaExpr, $lineEndingsExpr, $childrenExpr))}

  // The extractor counterpart to `interpolator`. Parses the pattern at
  // compile time and produces a function that matches a runtime Tel
  // value against the structural shape, binding marker-containing atom
  // texts to the corresponding hole positions.
  //
  // Returns per contextual.Extrapolation[Tel]:
  //   - Boolean        for 0 holes
  //   - Option[Tel]    for 1 hole (the captured atom-as-scalar Tel)
  //   - Option[Tuple]  for 2+ holes (tuple of captured scalar Tels)
  def extractor[parts <: Tuple: Type, origins <: Tuple: Type]
    ( scrutinee: Expr[Tel] )
  :   Macro[Boolean | Option[Tuple | Tel]] =

    import quotes.reflect.*

    def collectParts[tuple: Type](acc: List[String]): List[String] = Type.of[tuple] match
      case '[head *: tail] => collectParts[tail](TypeRepr.of[head].literal[String].vouch :: acc)
      case _               => acc

    val parts = collectParts[parts](Nil)
    val source: String = parts.mkString(MarkerString)
    val holeCount = parts.length - 1

    // Parse the pattern at compile time to validate syntax (and to halt
    // the macro with a clean source-positioned error if it's malformed).
    locally:
      given Diagnostics = Diagnostics.omit

      given HaltTactic[TelError, Tel.Document] = new HaltTactic[TelError, Tel.Document]:
        override def abort(error: Diagnostics ?=> TelError): Nothing =
          halt(m"the tel\"…\" pattern is invalid: ${error.message}")

      TelParser.parse(IArray.from(source.getBytes("UTF-8").nn.iterator))

    // At runtime the matcher re-parses the assembled pattern source from
    // an embedded byte literal. We could emit the pre-parsed AST as an
    // Expr but that's a substantial amount of code; re-parsing once per
    // match-site invocation is cheap enough for the macro's purpose.
    val patternBytesExpr: Expr[Data] =
      '{${Expr(source.getBytes("UTF-8").nn.toSeq)}.toArray.asInstanceOf[IArray[Byte]]}

    val markerExpr: Expr[Char] = Expr(Marker)

    val matchResult: Expr[Option[List[Tel]]] =
      ' {
          val pattern: Tel.Document =
            contingency.unsafely(TelParser.parse($patternBytesExpr))

          stratiform.internal.matchDocument(pattern, $scrutinee, $markerExpr)
        }

    if holeCount == 0 then '{$matchResult.isDefined: Boolean}
    else if holeCount == 1 then '{$matchResult.map(_.head): Option[Tel]}
    else
      val telType = TypeRepr.of[Tel]

      val tupleType =
        AppliedType
          ( defn.TupleClass(holeCount).info.typeSymbol.typeRef, List.fill(holeCount)(telType) )

      tupleType.asType.absolve match
        case '[type result <: Tuple; result] =>
          ' {
              $matchResult.map: captures =>
                val arr: Array[Object] = captures.toArray.asInstanceOf[Array[Object]]
                scala.runtime.Tuples.fromArray(arr).asInstanceOf[result]
            }

  // Runtime matcher: returns Some(captures) if input structurally matches
  // pattern (allowing marker characters in pattern atom-texts as capture
  // sites), None otherwise. Captures are emitted in document order.
  def matchDocument
    ( pattern: Tel.Document, input: Tel, marker: Char )
  :   Option[List[Tel]] =

    val captures = scala.collection.mutable.ListBuffer.empty[Tel]

    if matchBlocks(pattern.children, input.subtree.children, marker, captures)
    then Some(captures.toList)
    else None

  private def matchBlocks
    ( pattern: IArray[Tel.Block],
     input:   IArray[Tel.Block],
     marker:  Char,
     out:     scala.collection.mutable.ListBuffer[Tel] )
  :   Boolean =

    if pattern.length != input.length then false
    else
      var i = 0

      while i < pattern.length do
        if !matchBlock(pattern(i), input(i), marker, out) then return false
        i += 1

      true

  private def matchBlock
    ( pattern: Tel.Block,
     input:   Tel.Block,
     marker:  Char,
     out:     scala.collection.mutable.ListBuffer[Tel] )
  :   Boolean =

    if pattern.compounds.length != input.compounds.length then false
    else
      var i = 0

      while i < pattern.compounds.length do
        if !matchCompound(pattern.compounds(i), input.compounds(i), marker, out) then
          return false

        i += 1

      true

  private def matchCompound
    ( pattern: Tel.Compound,
     input:   Tel.Compound,
     marker:  Char,
     out:     scala.collection.mutable.ListBuffer[Tel] )
  :   Boolean =

    if pattern.keyword != input.keyword then false
    else if pattern.atoms.length != input.atoms.length then false
    else
      var i = 0

      while i < pattern.atoms.length do
        if !matchAtom(pattern.atoms(i), input.atoms(i), marker, out) then return false
        i += 1

      matchBlocks(pattern.children, input.children, marker, out)

  private def matchAtom
    ( pattern: Tel.Atom,
     input:   Tel.Atom,
     marker:  Char,
     out:     scala.collection.mutable.ListBuffer[Tel] )
  :     Boolean = pattern match
    case Tel.Atom.Inline(patText, _) =>
      input match
        case Tel.Atom.Inline(inText, _) => matchAtomText(patText, inText, marker, out)
        case _                          => false

    case Tel.Atom.Source(patText) =>
      input match
        case Tel.Atom.Source(inText) => matchAtomText(patText, inText, marker, out)
        case _                       => false

    case Tel.Atom.Literal(patDelim, patText) =>
      input match
        case Tel.Atom.Literal(inDelim, inText) if patDelim == inDelim =>
          matchAtomText(patText, inText, marker, out)

        case _ => false

  // Match a pattern atom text against an input atom text. The pattern
  // is split by the marker character into N+1 literal segments
  // separated by N hole markers (N >= 0); a successful match
  // satisfies:
  //   - the input starts with segment(0) (prefix)
  //   - the input ends with segment(N) (suffix)
  //   - for each interior marker, the next occurrence of the
  //     following segment is found left-to-right, and the substring
  //     between consumed segments is captured as a `Tel.scalar`
  //
  // Patterns with zero markers degenerate to a literal equality
  // check. Captures are appended to `out` in left-to-right pattern
  // order.
  private def matchAtomText
    ( pattern: Text,
     input:   Text,
     marker:  Char,
     out:     scala.collection.mutable.ListBuffer[Tel] )
  :   Boolean =

    import scala.language.unsafeNulls
    val p: String = pattern.s
    val s: String = input.s

    // Split pattern at every marker; pieces.length == markerCount + 1.
    val pieces = scala.collection.mutable.ArrayBuffer.empty[String]
    var start = 0
    var i = 0

    while i < p.length do
      if p.charAt(i) == marker then
        pieces += p.substring(start, i)
        start = i + 1

      i += 1

    pieces += p.substring(start)

    if pieces.length == 1 then p == s
    else
      val prefix = pieces(0)
      val suffix = pieces(pieces.length - 1)

      if !s.startsWith(prefix) then false
      else if !s.endsWith(suffix) then false
      else if s.length < prefix.length + suffix.length then false
      else
        // Left-to-right scan the interior segments. The captures
        // are appended to a local buffer so we can roll back on a
        // mid-pattern mismatch without leaving partial captures in
        // `out`.
        val local = scala.collection.mutable.ListBuffer.empty[Tel]
        var pos = prefix.length
        val end = s.length - suffix.length
        var idx = 1
        var ok = true

        while ok && idx < pieces.length - 1 do
          val seg = pieces(idx)

          val found =
            if seg.isEmpty then pos else s.indexOf(seg, pos)

          if found < 0 || found > end then ok = false
          else
            local += Tel.scalar(Text(s.substring(pos, found)))
            pos = found + seg.length
            idx += 1

        if !ok then false
        else
          local += Tel.scalar(Text(s.substring(pos, end)))
          out ++= local
          true
