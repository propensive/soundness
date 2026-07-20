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

import scala.quoted.*

import anticipation.*
import contingency.*
import fulminate.*
import gigantism.*
import gossamer.*
import prepositional.*
import rudiments.*
import vacuous.*

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

      Tel.Parser.parse(IArray.from(source.getBytes("UTF-8").nn.iterator))

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
        ("stratiform: staged parsing requires a case class; sums and other types use "+
          "`Tel.Parsable.derived`")

    if classSymbol.owner.isTerm then
      report.errorAndAbort
        ("stratiform: staged parsing requires a top-level or object-nested case class; "+
          "method-local classes use `Tel.Parsable.derived`")

    val ctor = classSymbol.primaryConstructor

    if ctor.paramSymss.filterNot(_.exists(_.isTypeParam)).length != 1 then
      report.errorAndAbort
        ("stratiform: staged parsing requires a single parameter list; use "+
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

      !annotated.exists { annotation =>
        annotation.tpe <:< TypeRepr.of[adversaria.name[?]] }

    val wireNames: List[String] = fieldNames.map { name => Tel.camelToKebab(name).s }

    def packedKeyword(index: Int): Option[Long] =
      val name = wireNames(index)
      val length = name.length

      val packs = length > 0 && length <= 8 &&
        name.forall { char => char >= '!' && char <= '~' }

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
              (s"stratiform: no Tel.Field instance for field ${fieldNames(index)}: "+
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
        tactic:      Expr[Tactic[TelError]],
        keys:        Expr[IArray[String]],
        instances:   Expr[IArray[Tel.Field | Null]],
        repeatables: Expr[IArray[Boolean]],
        fallbacks:   Expr[IArray[Any]] )
    :   Expr[value] =

      val owner = Symbol.spliceOwner
      val bufferType = TypeRepr.of[scala.collection.mutable.ListBuffer[Any] | Null]

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

      // One dispatch arm per field: read the value (with focus bookkeeping),
      // honoring the derived engine's semantics — a repeatable field gathers
      // every occurrence, a non-repeatable one keeps its first and skips the
      // rest.
      val arms = List.range(0, arity).map: index =>
        val keyText: Expr[Text] = '{ $keys(${Expr(index)}).tt }

        def firstWins(read: Term): Term =
          If
            ( Ref(seens(index)),
              '{ $reader.skipEntry($indent) }.asTerm,
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
                    Tel.Parsable.focusing($foci, $keyText):
                      $reader.int().lay(Tel.Parsable.scalarFault($reader, t"Int", 0))(identity)
                  }.asTerm

              case LongK =>
                firstWins:
                  '{
                    Tel.Parsable.focusing($foci, $keyText):
                      $reader.long().lay(Tel.Parsable.scalarFault($reader, t"Long", 0L))(identity)
                  }.asTerm

              case BooleanK =>
                firstWins:
                  '{
                    Tel.Parsable.focusing($foci, $keyText):
                      $reader.boolean()
                      . lay(Tel.Parsable.scalarFault($reader, t"Boolean", false))(identity)
                  }.asTerm

              case TextK =>
                firstWins:
                  '{
                    Tel.Parsable.focusing($foci, $keyText):
                      $reader.atom()
                      . lay { $reader.fault(TelError.Reason.Absent); t"" } (identity)
                  }.asTerm

              case StringK =>
                firstWins:
                  '{
                    Tel.Parsable.focusing($foci, $keyText):
                      $reader.atom()
                      . lay { $reader.fault(TelError.Reason.Absent); "" } { atom => atom.s }
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
                      ( Tel.Parsable.focusing($foci, $keyText):
                          Tel.Parsable.parseElement
                            ( $instances(${Expr(index)}).asInstanceOf[Tel.Parsing],
                              $reader,
                              $indent ) )
                  }.asTerm

                val read: Term =
                  '{
                    Tel.Parsable.focusing($foci, $keyText):
                      $instances(${Expr(index)}).asInstanceOf[fieldType is Tel.Field]
                      . parse($reader, $indent)
                  }.asTerm

                If
                  ( '{ $repeatables(${Expr(index)}) }.asTerm,
                    Block(List(ensure, append), unit),
                    firstWins(read) )

        CaseDef(Literal(IntConstant(index)), None, rhs)

      val fallthrough = CaseDef(Wildcard(), None, '{ $reader.skipEntry($indent) }.asTerm)

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
            val keyText: Expr[Text] = '{ $keys(${Expr(index)}).tt }

            val onAbsent: Expr[fieldType] = kinds(index) match
              case InstanceK =>
                '{
                  $instances(${Expr(index)}).asInstanceOf[fieldType is Tel.Field]
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
                    val declared = $fallbacks(${Expr(index)}).asInstanceOf[Optional[fieldType]]

                    if !declared.absent then declared.asInstanceOf[fieldType]
                    else Tel.Parsable.focusing($foci, $keyText)($onAbsent)
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
                        Tel.Parsable.focusing($foci, $keyText):
                          Tel.Parsable.gathered[fieldType]
                            ( $instances(${Expr(index)}).asInstanceOf[Tel.Parsing],
                              $bufferExpr match
                                case null   => Nil
                                case buffer => buffer.toList )
                      }.asTerm )

                If('{ $repeatables(${Expr(index)}) }.asTerm, gatherFinish, whenUnseen)

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

      Block(slotDefs ::: seenDefs ::: bufferDefs ::: loop ::: absents, construct)
      . asExprOf[value]

    def summonOrAbort[required: Type](role: String): Expr[required] =
      Expr.summon[required].getOrElse:
        report.errorAndAbort(s"stratiform: staged parsing needs a contextual $role")

    val fociExpr = summonOrAbort[Foci[Tel.Focus]]("Foci[Tel.Focus]")
    val tacticExpr = summonOrAbort[Tactic[TelError]]("Tactic[TelError]")
    val nameExprs = fieldNames.map { name => Expr(name) }
    val instanceExprs = List.range(0, arity).map(summonField)
    val fallbackExprs = List.range(0, arity).map(declaredDefault)

    '{
      // Sealed per the codec-thunk pattern, like the derived instances: the
      // generated parser captures the resolution-scoped tactic and foci.
      // The instance and default arrays are single lazy vals, so recursive
      // self-references stay deferred until the first parse.
      caps.unsafe.unsafeAssumePure:
        val foci: Foci[Tel.Focus] = $fociExpr
        val tactic: Tactic[TelError] = $tacticExpr

        val keys: IArray[String] =
          Tel.Parsable.wireKeywords(IArray[String](${Varargs(nameExprs)}*), $renames)

        lazy val instances: IArray[Tel.Field | Null] = IArray(${Varargs(instanceExprs)}*)

        lazy val repeatables: IArray[Boolean] =
          instances.map { instance => instance != null && Tel.Parsable.repeats(instance) }

        lazy val fallbacks: IArray[Any] = IArray[Any](${Varargs(fallbackExprs)}*)

        new Tel.Parsable:
          type Self = value
          def shape(): Morphology = Morphology.Any

          def parse(reader: TelReader^, indent: Int): value =
            reader.finishLine()
            ${
              body
                ( '{reader}, '{indent + 1}, '{foci}, '{tactic}, '{keys}, '{instances},
                  '{repeatables}, '{fallbacks} )
            }

          override def parse(reader: TelReader^): value =
            ${
              body
                ( '{reader}, '{0}, '{foci}, '{tactic}, '{keys}, '{instances},
                  '{repeatables}, '{fallbacks} )
            }
    }
