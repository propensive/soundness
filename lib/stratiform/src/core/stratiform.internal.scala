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
package stratiform

import scala.collection.immutable.Seq

import scala.{annotation, caps}

import scala.collection.immutable.{List, Nil, ::}
import scala.quoted.*

import anticipation.*
import contingency.*
import fulminate.*
import gigantism.*
import gossamer.*
import prepositional.*
import denominative.*
import rudiments.*
import vacuous.*
import symbolism.*

// Compile-time machinery for the `tel"…"` interpolator and extractor.
// Mirrors jacinta.internal in shape: the static parts of a StringContext
// are joined with a marker character, parsed at compile time using the
// runtime Tel.Parser, and the parsed AST is rebuilt as an Expr[Tel] with
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
  private final val Marker: Char = '\u0001'
  private final val MarkerString: String = Marker.toString

  private def hasMarker(text: Text): Boolean =
    text.spot { index => text(index) == Marker }.present

  // Translate a `Tel.Error`'s span — a 0-based line with byte-oriented column and length into
  // the marker-joined `source` — to a source-file position, so the compiler's caret lands on
  // the offending TEL inside the literal. The markers are one character wide, matching
  // `sourcePosition`'s substitution gaps.
  private def errorPosition[origins <: Tuple: Type](using Quotes)
    ( error: Tel.Error, source: String, parts: List[String] )
  :   quotes.reflect.Position =

    val utf8 = java.nio.charset.StandardCharsets.UTF_8
    val bytes = source.getBytes(utf8).nn
    val line = error.span.startLine.lay(0)(_.n0)
    val column = error.span.startColumn.lay(0)(_.n0)
    val spanLength = error.span.length.or(1).max(1)

    def lineStart(remaining: Int, from: Int): Int =
      if remaining == 0 || from >= bytes.length then from else
        var index = from
        while index < bytes.length && bytes(index) != '\n'.toByte do index += 1
        lineStart(remaining - 1, (index + 1).min(bytes.length))

    val byteStart = (lineStart(line, 0) + column).min(bytes.length)
    val byteEnd = (byteStart + spanLength).min(bytes.length)
    val charStart = String(bytes, 0, byteStart, utf8).length
    val charEnd = String(bytes, 0, byteEnd, utf8).length

    contextual.Interpolation.sourcePosition
      ( parts.to(proscenium.List), contextual.Interpolation.decodeOrigins[origins],
        1, charStart, (charEnd - charStart).max(1) )

  def interpolator[parts <: Tuple: Type, origins <: Tuple: Type]
    ( insertions0: Expr[Seq[Any]] )
  :   Macro[Tel] =

    import quotes.reflect.*

    // Tuple-type iteration: the contextual framework presents parts in
    // reverse-source order, so accumulating with cons gives source order
    // directly — no final reverse needed (mirrors jacinta.internal).
    def collectParts[tuple: Type](acc: List[String]): List[String] = Type.of[tuple] match
      case '[head *: tail] => collectParts[tail](TypeRepr.of[head].literal[String].or(halt(m"an interpolator's parts are string-literal types")) :: acc)
      case _               => acc

    val parts = collectParts[parts](Nil)
    val source: String = parts.mkString(MarkerString)
    val data: Data = Array.from(source.getBytes("UTF-8").nn.iterator)

    val insertions: Seq[Expr[Any]] = insertions0.absolve match
      case Varargs(insertions) => insertions

    // Parse the assembled source at compile-time to validate syntax. If
    // parsing fails, halt the macro with the error code as the message.
    val document: Tel.Document =
      given Diagnostics = Diagnostics.omit

      // `record` is overridden too: the parser reports recoverable errors (e.g. odd
      // indentation) by raising rather than aborting, and both must land positioned.
      given HaltTactic[Tel.Error, Tel.Document] = new HaltTactic[Tel.Error, Tel.Document]:
        private def fail(telError: Tel.Error): Nothing =
          halt
            ( m"the tel\"…\" literal is invalid: ${telError.message}",
              errorPosition[origins](telError, source, parts) )

        override def record(error: Diagnostics ?=> Tel.Error): Unit = fail(error)
        override def abort(error: Diagnostics ?=> Tel.Error): Nothing = fail(error)

      Tel.Parser.parse(data)

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

      // The casts happen on the `Expr`s, outside the quotes: any frozen-array-typed term
      // inside a quote picks up a fresh `any.rd` capability that cannot flow into `^{}`.
      // The emitted arrays are fresh and never written, so the frozen form is sound.
      def emitAtomsArray(atoms: Array[Tel.Atom]^{}): Expr[Array[Tel.Atom]^{}] =
        val list = atoms.readable.toList.map(emitAtom)
        '{Array.from(${Expr.ofList(list)})}.asInstanceOf[Expr[Array[Tel.Atom]^{}]]

      def emitComment(c: Tel.Comment): Expr[Tel.Comment] =
        '{Tel.Comment(${Expr(c.text.s)}.tt)}

      def emitTabulation(t: Tel.Tabulation): Expr[Tel.Tabulation] =
        val markers = Expr(t.markerOffsets.readable.toList)
        val headings = Expr(t.headings.readable.toList.map(_.s))
        '{Tel.Tabulation(Array.from(${markers}), Array.from(${headings}.map(_.tt)))}

      def emitCompound(c: Tel.Compound): Expr[Tel.Compound] =
        val keywordExpr = Expr(c.keyword.s)
        val atomsExpr = emitAtomsArray(c.atoms)

        val remarkExpr: Expr[Optional[Text]] = c.remark match
          case text: Text => '{${Expr(text.s)}.tt: Optional[Text]}
          case _          => '{Unset}

        val childrenExpr = emitBlocks(c.children)
        '{Tel.Compound(${keywordExpr}.tt, $atomsExpr, $remarkExpr, $childrenExpr)}

      def emitBlock(b: Tel.Block): Expr[Tel.Block] =
        val comments =
          '{Array.from(${Expr.ofList(b.comments.readable.toList.map(emitComment))})}
          . asInstanceOf[Expr[Array[Tel.Comment]^{}]]

        val tab: Expr[Optional[Tel.Tabulation]] = b.tabulation match
          case t: Tel.Tabulation => '{${emitTabulation(t)}: Optional[Tel.Tabulation]}
          case _                 => '{Unset}

        val compounds =
          '{Array.from(${Expr.ofList(b.compounds.readable.toList.map(emitCompound))})}
          . asInstanceOf[Expr[Array[Tel.Compound]^{}]]

        val tbl = Expr(b.trailingBlankLines)
        '{Tel.Block($comments, $tab, $compounds, $tbl)}

      def emitBlocks(blocks: Array[Tel.Block]^{}): Expr[Array[Tel.Block]^{}] =
        '{Array.from(${Expr.ofList(blocks.readable.toList.map(emitBlock))})}
        . asInstanceOf[Expr[Array[Tel.Block]^{}]]

      val directiveExpr: Expr[Optional[Text]] = document.interpreterDirective match
        case text: Text => '{${Expr(text.s)}.tt: Optional[Text]}
        case _          => '{Unset}

      val pragmaExpr: Expr[Optional[Tel.Pragma]] = document.pragma match
        case p: Tel.Pragma =>
          val versionExpr = '{(${Expr(p.version._1)}, ${Expr(p.version._2)})}

          val referenceExpr: Expr[Optional[Tel.Pragma.Reference]] = p.reference match
            case r: Tel.Pragma.Reference =>
              val selectorExpr: Expr[Optional[Tel.Pragma.Reference.Selector]] =
                r.selector match
                  case Tel.Pragma.Reference.Selector.Version(major, minor, patch) =>
                    val version: Expr[Tel.Pragma.Reference.Selector] =
                      '{Tel.Pragma.Reference.Selector.Version(${Expr(major)}, ${Expr(minor)}, ${Expr(patch)})}
                    '{$version: Optional[Tel.Pragma.Reference.Selector]}

                  case Tel.Pragma.Reference.Selector.Tag(name) =>
                    val tag: Expr[Tel.Pragma.Reference.Selector] =
                      '{Tel.Pragma.Reference.Selector.Tag(${Expr(name.s)}.tt)}
                    '{$tag: Optional[Tel.Pragma.Reference.Selector]}

                  case _ =>
                    '{Unset}

              val ref: Expr[Tel.Pragma.Reference] =
                '{Tel.Pragma.Reference(${Expr(r.domain.s)}.tt, ${Expr(r.name.s)}.tt, $selectorExpr)}

              '{$ref: Optional[Tel.Pragma.Reference]}

            case _ =>
              '{Unset}

          // `Expr.ofList` is a quotes-reflection API and takes the stdlib list, so the
          // layer names cross to the stdlib view here and back to `proscenium.List`
          // inside the quote.
          val layersExpr: Expr[proscenium.List[Text]] =
            '{(${Expr.ofList(p.layers.stdlib.map { layer => '{${Expr(layer.s)}.tt} })}).to(proscenium.List)}

          val signatureExpr: Expr[Optional[Text]] = p.signature match
            case text: Text => '{${Expr(text.s)}.tt: Optional[Text]}
            case _          => '{Unset}

          val sigilExpr: Expr[Optional[Char]] = p.sigil match
            case c: Char => '{${Expr(c)}: Optional[Char]}
            case _       => '{Unset}

          val pragma: Expr[Tel.Pragma] =
            '{Tel.Pragma($versionExpr, $referenceExpr, $layersExpr, $signatureExpr, $sigilExpr)}

          '{$pragma: Optional[Tel.Pragma]}

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
      case '[head *: tail] => collectParts[tail](TypeRepr.of[head].literal[String].or(halt(m"an interpolator's parts are string-literal types")) :: acc)
      case _               => acc

    val parts = collectParts[parts](Nil)
    val source: String = parts.mkString(MarkerString)
    val holeCount = parts.length - 1

    // Parse the pattern at compile time to validate syntax (and to halt
    // the macro with a clean source-positioned error if it's malformed).
    locally:
      given Diagnostics = Diagnostics.omit

      given HaltTactic[Tel.Error, Tel.Document] = new HaltTactic[Tel.Error, Tel.Document]:
        private def fail(telError: Tel.Error): Nothing =
          halt
            ( m"the tel\"…\" pattern is invalid: ${telError.message}",
              errorPosition[origins](telError, source, parts) )

        override def record(error: Diagnostics ?=> Tel.Error): Unit = fail(error)
        override def abort(error: Diagnostics ?=> Tel.Error): Nothing = fail(error)

      Tel.Parser.parse(Array.from(source.getBytes("UTF-8").nn.iterator))

    // At runtime the matcher re-parses the assembled pattern source from
    // an embedded byte literal. We could emit the pre-parsed AST as an
    // Expr but that's a substantial amount of code; re-parsing once per
    // match-site invocation is cheap enough for the macro's purpose.
    val patternBytesExpr: Expr[Data] =
      // In-quote cast so the expanded tree is `Data`-typed; the outer `Expr` cast erases
      // the fresh `any.rd` the checker puts on any frozen-array-typed quote.
      '{${Expr(source.getBytes("UTF-8").nn.toSeq)}.toArray.asInstanceOf[Data]}
      . asInstanceOf[Expr[Data]]

    val markerExpr: Expr[Char] = Expr(Marker)

    val matchResult: Expr[Option[List[Tel]]] =
      ' {
          val pattern: Tel.Document =
            contingency.unsafely(Tel.Parser.parse($patternBytesExpr))

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
                val arr: scala.Array[Object] = captures.toArray.asInstanceOf[scala.Array[Object]]
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
    ( pattern: Array[Tel.Block]^{},
     input:   Array[Tel.Block]^{},
     marker:  Char,
     out:     scala.collection.mutable.ListBuffer[Tel] )
  :   Boolean =

    pattern.length == input.length && pattern.spot: index =>
      !input.at(index).lay(false)(matchBlock(pattern.at(index), _, marker, out))
    . absent

  private def matchBlock
    ( pattern: Tel.Block,
     input:   Tel.Block,
     marker:  Char,
     out:     scala.collection.mutable.ListBuffer[Tel] )
  :   Boolean =

    if pattern.compounds.length != input.compounds.length then false
    else
      val left = pattern.compounds
      val right = input.compounds

      left.spot: index =>
        !right.at(index).lay(false)(matchCompound(left.at(index), _, marker, out))
      . absent

  private def matchCompound
    ( pattern: Tel.Compound,
     input:   Tel.Compound,
     marker:  Char,
     out:     scala.collection.mutable.ListBuffer[Tel] )
  :   Boolean =

    if pattern.keyword != input.keyword then false
    else if pattern.atoms.length != input.atoms.length then false
    else
      val left = pattern.atoms
      val right = input.atoms

      val atoms = left.spot: index =>
        !right.at(index).lay(false)(matchAtom(left.at(index), _, marker, out))
      . absent

      atoms && matchBlocks(pattern.children, input.children, marker, out)

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
        pieces += p.substring(start, i).nn
        start = i + 1

      i += 1

    pieces += p.substring(start).nn

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
            local += Tel.scalar(Text(s.substring(pos, found).nn))
            pos = found + seg.length
            idx += 1

        if !ok then false
        else
          local += Tel.scalar(Text(s.substring(pos, end).nn))
          out ++= local
          true

  // ── Staged parser generation ──────────────────────────────────────────────
  // Generates a monomorphic `Tel.Parsable` for a case class: field values
  // live in typed locals, keywords dispatch through packed-`Long` literal
  // comparisons (with a linear text step for unpackable keywords), builtin
  // primitives read inline off the reader, and the record is built by a
  // direct constructor call — no `Array[Any]` buffer, no `Mirror`, no
  // per-field boxing. Field types beyond the builtins resolve through
  // `Tel.Field` instances (summoned at expansion, initialized lazily so
  // recursive references stay deferred), so semantics — wire keywords,
  // gathering of repeatable fields, first-match-wins duplicates, defaults,
  // absents, error foci — are identical to `ParsableDerivation`. The body is
  // assembled from reflection trees with only small, immediately-scoped
  // quotes: chained quotes carrying `Type` bindings through closures are
  // unpicklable.

  private enum StagedKind:
    case IntK, LongK, BooleanK, TextK, StringK, InstanceK

  def stagedParsable[value: Type](renames: Expr[Map[Text, Text]])(using Quotes)
  :   Expr[value is Tel.Parsable] =

    import quotes.reflect.*
    import StagedKind.*

    val tpe = TypeRepr.of[value].dealias

    val classSymbol = tpe.classSymbol.getOrElse:
      report.errorAndAbort("stratiform: staged parsing requires a case class")

    if !classSymbol.flags.is(Flags.Case) then
      report.errorAndAbort
        ("stratiform: staged parsing requires a case class; sums and other types use " +
          "`Tel.Parsable.derived`")

    if classSymbol.owner.isTerm then
      report.errorAndAbort
        ("stratiform: staged parsing requires a top-level or object-nested case class; " +
          "method-local classes use `Tel.Parsable.derived`")

    val ctor = classSymbol.primaryConstructor

    if ctor.paramSymss.filterNot(_.exists(_.isTypeParam)).length != 1 then
      report.errorAndAbort
        ("stratiform: staged parsing requires a single parameter list; use " +
          "`Tel.Parsable.derived`")

    val fields = classSymbol.caseFields
    val arity = fields.length
    val fieldNames: List[String] = fields.map(_.name)
    val fieldTypes: List[TypeRepr] = fields.map { field => tpe.memberType(field).dealias }

    def kindOf(fieldType: TypeRepr): StagedKind =
      if fieldType =:= TypeRepr.of[Int] then IntK
      else if fieldType =:= TypeRepr.of[Long] then LongK
      else if fieldType =:= TypeRepr.of[Boolean] then BooleanK
      else if fieldType =:= TypeRepr.of[Text] then TextK
      else if fieldType =:= TypeRepr.of[String] then StringK
      else InstanceK

    val kinds: List[StagedKind] = fieldTypes.map(kindOf)

    // Keywords compile to literal packed-word comparisons when no `@name`
    // annotation can rename them (renames resolve at runtime, so annotated
    // classes keep the linear text step for every keyword). The literals use
    // the same camel→kebab mapping `Tel.Parsable.wireKeywords` applies at
    // runtime. A wire keyword that cannot pack (longer than eight bytes)
    // still parses: it always arrives as `KeywordOpaque` and takes the
    // general text step, which matches all fields by string.
    val literalKeys: Boolean =
      val annotated = ctor.paramSymss.flatten.filterNot(_.isTypeParam).flatMap(_.annotations)
        ++ fields.flatMap(_.annotations)

      !annotated.exists { annotation => annotation.tpe <:< TypeRepr.of[adversaria.name[?]] }

    val wireNames: List[String] = fieldNames.map { name => Tel.camelToKebab(name).s }

    def packedKeyword(index: Int): Option[Long] =
      val name = wireNames(index)
      val length = name.length

      val packs = length > 0 && length <= 8 && name.forall: char => char >= '!' && char <= '~'

      if !packs then None else
        var word = 0L
        var position = 0

        while position < length do
          word |= (name.charAt(position).toLong & 0xFF) << (position*8)
          position += 1

        Some(word)

    val packedKeywords: List[Option[Long]] = List.range(0, arity).map(packedKeyword)

    def summonField(index: Int): Expr[Tel.Field | Null] =
      if kinds(index) != InstanceK then '{null}
      else fieldTypes(index).asType match
        case '[fieldType] =>
          Expr.summon[fieldType is Tel.Field].getOrElse:
            report.errorAndAbort
              (s"stratiform: no Tel.Field instance for field ${fieldNames(index)}: " +
                fieldTypes(index).show)

    def declaredDefault(index: Int): Expr[Any] = fieldTypes(index).asType match
      case '[fieldType] =>
        '{ wisteria.internal.default[value, fieldType](${Expr(index)}): Any }

    def zero(fieldType: TypeRepr): Term =
      if fieldType =:= TypeRepr.of[Int] then Literal(IntConstant(0))
      else if fieldType =:= TypeRepr.of[Long] then Literal(LongConstant(0L))
      else if fieldType =:= TypeRepr.of[Boolean] then Literal(BooleanConstant(false))
      else fieldType.asType match
        case '[fieldType] => '{ null.asInstanceOf[fieldType] }.asTerm

    def body
      ( reader:      Expr[TelReader],
        indent:      Expr[Int],
        foci:        Expr[Foci[Tel.Focus]],
        tactic:      Expr[Tactic[Tel.Error]],
        keys:        Expr[Array[String]^{}],
        instances:   Expr[Array[Tel.Field | Null]^{}],
        repeatables: Expr[Array[Boolean]^{}],
        fallbacks:   Expr[Array[Any]^{}],
        table:       Expr[AnyRef],
        lineAtoms:   Option[Expr[Array[Tel.Atom]^{}]] )
    :   Expr[value] =

      val owner = Symbol.spliceOwner
      val bufferType = TypeRepr.of[scala.collection.mutable.ListBuffer[Any] | Null]

      val slots = List.range(0, arity).map: index =>
        Symbol.newVal(owner, "slot"+index, fieldTypes(index), Flags.Mutable, Symbol.noSymbol)

      val seens = List.range(0, arity).map: index =>
        Symbol.newVal(owner, "seen"+index, TypeRepr.of[Boolean], Flags.Mutable, Symbol.noSymbol)

      // Whether a field's slot was filled by a positionally-assigned atom
      // rather than a keyword child — a later same-keyword child then fills
      // a non-repeatable member twice (§20.2 step 5c).
      val atomFilleds = List.range(0, arity).map: index =>
        Symbol.newVal(owner, "atom"+index, TypeRepr.of[Boolean], Flags.Mutable, Symbol.noSymbol)

      // Occurrence buffers for the fields that may gather (repeatable
      // instances), allocated lazily on the first occurrence.
      val buffers: List[Option[Symbol]] = List.range(0, arity).map: index =>
        if kinds(index) != InstanceK then None else
          Some(Symbol.newVal(owner, "gather"+index, bufferType, Flags.Mutable, Symbol.noSymbol))

      val slotDefs = List.range(0, arity).map: index =>
        ValDef(slots(index), Some(zero(fieldTypes(index))))

      val seenDefs = List.range(0, arity).map: index =>
        ValDef(seens(index), Some(Literal(BooleanConstant(false))))

      val atomFilledDefs = List.range(0, arity).map: index =>
        ValDef(atomFilleds(index), Some(Literal(BooleanConstant(false))))

      val bufferDefs = List.range(0, arity).flatMap: index =>
        buffers(index).map: symbol => ValDef(symbol, Some('{ null }.asTerm))

      val unit = Literal(UnitConstant())

      // One dispatch arm per field: read the value (with focus bookkeeping),
      // honoring the derived engine's semantics — a repeatable field gathers
      // every occurrence, a non-repeatable one keeps its first and skips the
      // rest.
      val arms = List.range(0, arity).map: index =>
        val keyText: Expr[Text] = '{ $keys.readable(${Expr(index)}).tt }

        def firstWins(read: Term): Term =
          If
            ( Ref(seens(index)),
              Block
                ( List
                    ( If
                        ( Ref(atomFilleds(index)),
                          '{ Tel.Parsable.duplicateFill()(using $tactic) }.asTerm,
                          unit ) ),
                  '{ $reader.skipEntry($indent) }.asTerm ),
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
                  '{
                    Tel.Parsable.focusing($foci, $reader, $keyText):
                      $reader.int().lay(Tel.Parsable.scalarFault($reader, t"Int", 0))(identity)
                  }.asTerm

              case LongK =>
                firstWins:
                  '{
                    Tel.Parsable.focusing($foci, $reader, $keyText):
                      $reader.long().lay(Tel.Parsable.scalarFault($reader, t"Long", 0L))(identity)
                  }.asTerm

              case BooleanK =>
                firstWins:
                  '{
                    Tel.Parsable.focusing($foci, $reader, $keyText):
                      $reader.boolean()
                      . lay(Tel.Parsable.scalarFault($reader, t"Boolean", false))(identity)
                  }.asTerm

              case TextK =>
                firstWins:
                  '{
                    Tel.Parsable.focusing($foci, $reader, $keyText):
                      $reader.atom()
                      . lay { $reader.fault(Tel.Error.Reason.Absent); t"" } (identity)
                  }.asTerm

              case StringK =>
                firstWins:
                  '{
                    Tel.Parsable.focusing($foci, $reader, $keyText):
                      $reader.atom()
                      . lay { $reader.fault(Tel.Error.Reason.Absent); "" } { atom => atom.s }
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
                      ( Tel.Parsable.focusing($foci, $reader, $keyText):
                          Tel.Parsable.parseElement
                            ( $instances.readable(${Expr(index)}).asInstanceOf[Tel.Parsing],
                              $reader,
                              $indent ) )
                  }.asTerm

                val read: Term =
                  '{
                    Tel.Parsable.focusing($foci, $reader, $keyText):
                      $instances.readable(${Expr(index)}).asInstanceOf[fieldType is Tel.Field]
                      . parse($reader, $indent)
                  }.asTerm

                If
                  ( '{ $repeatables.readable(${Expr(index)}) }.asTerm,
                    Block(List(ensure, append), unit),
                    firstWins(read) )

        CaseDef(Literal(IntConstant(index)), None, rhs)

      val fallthrough = CaseDef(Wildcard(), None, '{ $reader.skipEntry($indent) }.asTerm)

      // The §19.2 atom phase over the entry line's own atoms, run before the
      // keyword loop so a repeatable field's atoms precede its same-keyword
      // children (§18.3 step 4). Generated only for the entry form — the
      // document root carries no atoms (§20.2) — and guarded at runtime, so
      // the dominant atomless shape pays a single length test.
      val prepass: List[Statement] = lineAtoms.toList.map: atoms =>
        val assignment =
          Symbol.newVal(owner, "positional", TypeRepr.of[AnyRef], Flags.EmptyFlags, Symbol.noSymbol)

        val assignmentExpr = Ref(assignment).asExprOf[AnyRef]

        val deliveries: List[Statement] = List.range(0, arity).map: index =>
          val keyText: Expr[Text] = '{ $keys.readable(${Expr(index)}).tt }
          val count: Expr[Int] = '{ Tel.Parsable.positionalCount($assignmentExpr, ${Expr(index)}) }

          val first: Expr[Text] =
            '{ Tel.Parsable.positionalText($assignmentExpr, ${Expr(index)}, 0) }

          // A slot filled from an atom is `seen`, so the keyword loop's
          // first-wins step skips a later same-keyword child (and reports
          // the duplicate fill).
          def fill(value: Term): Term =
            Block
              ( List
                  ( Assign(Ref(slots(index)), value),
                    Assign(Ref(seens(index)), Literal(BooleanConstant(true))),
                    Assign(Ref(atomFilleds(index)), Literal(BooleanConstant(true))) ),
                unit )

          val deliver: Term = fieldTypes(index).asType match
            case '[fieldType] =>
              kinds(index) match
                case IntK =>
                  fill:
                    '{
                      Tel.Parsable.focusing($foci, $reader, $keyText):
                        Tel.Parsable.atomInt($first)(using $tactic)
                    }.asTerm

                case LongK =>
                  fill:
                    '{
                      Tel.Parsable.focusing($foci, $reader, $keyText):
                        Tel.Parsable.atomLong($first)(using $tactic)
                    }.asTerm

                case BooleanK =>
                  fill:
                    '{
                      Tel.Parsable.focusing($foci, $reader, $keyText):
                        Tel.Parsable.atomBoolean($first)(using $tactic)
                    }.asTerm

                case TextK => fill(first.asTerm)

                case StringK => fill('{ $first.s }.asTerm)

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

                  // §19.2: a repeatable member takes every atom assigned to
                  // it, each becoming one gathered occurrence.
                  val gather: Term =
                    '{
                      var occurrence = 0

                      while occurrence < $count do
                        $bufferExpr.asInstanceOf[scala.collection.mutable.ListBuffer[Any]].addOne
                          ( Tel.Parsable.focusing($foci, $reader, $keyText):
                              Tel.Parsable.parseAtomElement
                                ( $instances.readable(${Expr(index)}).asInstanceOf[Tel.Parsing],
                                  Tel.Parsable.positionalText
                                    ( $assignmentExpr, ${Expr(index)}, occurrence ) )
                                ( using $tactic ) )

                        occurrence += 1
                    }.asTerm

                  val single: Term =
                    fill:
                      '{
                        val instance =
                          $instances.readable(${Expr(index)}).asInstanceOf[fieldType is Tel.Field]

                        Tel.Parsable.focusing($foci, $reader, $keyText):
                          if instance.nature == Tel.Nature.Flag
                          then instance.parseFlag()(using $tactic)
                          else instance.parseAtom($first)(using $tactic)
                      }.asTerm

                  If
                    ( '{ $repeatables.readable(${Expr(index)}) }.asTerm,
                      Block(List(ensure), gather),
                      single )

          If('{ $count > 0 }.asTerm, deliver, unit)

        If
          ( '{ $atoms.length > 0 }.asTerm,
            Block
              ( ValDef
                  ( assignment,
                    Some('{ Tel.Parsable.positionalAssign($table, $atoms)(using $tactic) }.asTerm) )
                :: deliveries,
                unit ),
            unit )

      // The keyword loop. With literal keywords, each step compares the
      // packed word against the wire keywords as immediate constants,
      // resolving an opaque keyword through the linear text step; otherwise
      // (runtime renames) every keyword resolves through the text step.
      val run = Symbol.newVal(owner, "run", TypeRepr.of[Boolean], Flags.Mutable, Symbol.noSymbol)
      val word = Symbol.newVal(owner, "word", TypeRepr.of[Long], Flags.EmptyFlags, Symbol.noSymbol)
      val found = Symbol.newVal(owner, "found", TypeRepr.of[Int], Flags.EmptyFlags, Symbol.noSymbol)
      val wordRef = Ref(word).asExprOf[Long]

      def chain(index: Int): Term =
        if index == arity then Literal(IntConstant(-1))
        else packedKeywords(index) match
          case None => chain(index + 1)

          case Some(packed) =>
            If
              ( '{ $wordRef == ${Expr(packed)} }.asTerm,
                Literal(IntConstant(index)),
                chain(index + 1) )

      val textStep: Term = '{ Tel.Parsable.keywordIndex($keys, $reader.keywordText) }.asTerm

      val resolve: Term =
        if literalKeys then
          If('{ $wordRef == TelReader.KeywordOpaque }.asTerm, textStep, chain(0))
        else textStep

      val step: Term =
        Block
          ( List(ValDef(word, Some('{ $reader.keywordWord($indent) }.asTerm))),
            If
              ( '{ $wordRef == TelReader.KeywordEnd }.asTerm,
                Assign(Ref(run), Literal(BooleanConstant(false))),
                Block
                  ( List(ValDef(found, Some(resolve))),
                    Match(Ref(found), arms :+ fallthrough) ) ) )

      val loop: List[Statement] =
        List(ValDef(run, Some(Literal(BooleanConstant(true)))), While(Ref(run), step))

      // Fields whose keywords never arrived — and repeatable fields, whose
      // collection is always built from the gathered occurrences (zero
      // occurrences build the empty collection; a repeatable field never
      // consults the declared default), exactly as the derived engine does.
      val absents: List[Term] = List.range(0, arity).map: index =>
        fieldTypes(index).asType match
          case '[fieldType] =>
            val keyText: Expr[Text] = '{ $keys.readable(${Expr(index)}).tt }

            val onAbsent: Expr[fieldType] = kinds(index) match
              case InstanceK =>
                '{
                  $instances.readable(${Expr(index)}).asInstanceOf[fieldType is Tel.Field]
                  . absent()(using $tactic)
                }

              case IntK     => '{ Tel.Parsable.missing[Int](0)(using $tactic) }.asExprOf[fieldType]
              case LongK    => '{ Tel.Parsable.missing[Long](0L)(using $tactic) }.asExprOf[fieldType]
              case TextK    => '{ Tel.Parsable.missing[Text](t"")(using $tactic) }.asExprOf[fieldType]
              case StringK  => '{ Tel.Parsable.missing[String]("")(using $tactic) }.asExprOf[fieldType]

              case BooleanK =>
                '{ Tel.Parsable.missing[Boolean](false)(using $tactic) }.asExprOf[fieldType]

            val resolveAbsent: Term =
              Assign
                ( Ref(slots(index)),
                  '{
                    val declared = $fallbacks.readable(${Expr(index)}).asInstanceOf[Optional[fieldType]]

                    if !declared.absent then declared.asInstanceOf[fieldType]
                    else Tel.Parsable.focusingUnlocated($foci, $keyText)($onAbsent)
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
                        Tel.Parsable.focusingUnlocated($foci, $keyText):
                          Tel.Parsable.gathered[fieldType]
                            ( $instances.readable(${Expr(index)}).asInstanceOf[Tel.Parsing],
                              ( $bufferExpr match
                                  case null   => Nil
                                  case buffer => buffer.toList )
                              . to(proscenium.List) )
                      }.asTerm )

                If('{ $repeatables.readable(${Expr(index)}) }.asTerm, gatherFinish, whenUnseen)

              case _ =>
                whenUnseen

      val construct: Term =
        val typeArguments = tpe match
          case AppliedType(_, arguments) => arguments
          case _                         => Nil

        val newTerm = Select(New(Inferred(tpe)), ctor)

        val applied =
          if typeArguments.isEmpty then newTerm
          else TypeApply(newTerm, typeArguments.map { argument => Inferred(argument) })

        Apply(applied, slots.map { slot => Ref(slot) })

      Block
        ( slotDefs ::: seenDefs ::: atomFilledDefs ::: bufferDefs ::: prepass ::: loop ::: absents,
          construct )
      . asExprOf[value]

    def summonOrAbort[required: Type](role: String): Expr[required] =
      Expr.summon[required].getOrElse:
        report.errorAndAbort(s"stratiform: staged parsing needs a contextual $role")

    val fociExpr = summonOrAbort[Foci[Tel.Focus]]("Foci[Tel.Focus]")
    val tacticExpr = summonOrAbort[Tactic[Tel.Error]]("Tactic[Tel.Error]")
    val nameExprs = fieldNames.map { name => Expr(name) }
    val instanceExprs = List.range(0, arity).map(summonField)
    val fallbackExprs = List.range(0, arity).map(declaredDefault)

    // A primitive field's §20.2 nature is fixed by its type here; an
    // instance-backed field's is read from the instance when the positional
    // table is built, so its entry is only a placeholder.
    val natureExprs: List[Expr[Tel.Nature]] = kinds.map:
      case BooleanK  => '{ Tel.Nature.Flag }
      case InstanceK => '{ Tel.Nature.Struct }
      case _         => '{ Tel.Nature.Scalar }

    '{
      // Sealed per the codec-thunk pattern, like the derived instances: the
      // generated parser captures the resolution-scoped tactic and foci.
      // The instance and default arrays are single lazy vals, so recursive
      // self-references stay deferred until the first parse.
      caps.unsafe.unsafeAssumePure:
        val foci: Foci[Tel.Focus] = $fociExpr
        val tactic: Tactic[Tel.Error] = $tacticExpr

        val keys: Array[String]^{} =
          Tel.Parsable.wireKeywords(Array[String](${Varargs(nameExprs)}*), $renames)

        lazy val instances: Array[Tel.Field | Null]^{} = Array(${Varargs(instanceExprs)}*)

        lazy val repeatables: Array[Boolean]^{} =
          instances.remap { instance => instance != null && Tel.Parsable.repeats(instance) }

        lazy val fallbacks: Array[Any]^{} = Array[Any](${Varargs(fallbackExprs)}*)

        lazy val natures: Array[Tel.Nature]^{} = Array[Tel.Nature](${Varargs(natureExprs)}*)

        // The §19.2 profile table: one per generated instance, built on
        // first use so recursive self-references stay deferred, like the
        // instance array it reads.
        lazy val table: AnyRef =
          Tel.Parsable.positionalTable(keys, natures, instances, fallbacks)

        new Tel.Parsable:
          type Self = value
          def shape(): Morphology = Morphology.Any

          def parse(reader: TelReader^, indent: Int): value =
            val atoms = reader.lineAtoms()
            ${
              body
                ( '{reader}, '{indent + 1}, '{foci}, '{tactic}, '{keys}, '{instances},
                  '{repeatables}, '{fallbacks}, '{table}, Some('{atoms}) )
            }

          // The document root carries no atoms (§20.2), so the whole-input
          // form generates no positional pre-pass at all.
          override def parse(reader: TelReader^): value =
            ${
              body
                ( '{reader}, '{0}, '{foci}, '{tactic}, '{keys}, '{instances},
                  '{repeatables}, '{fallbacks}, '{table}, None )
            }
    }
