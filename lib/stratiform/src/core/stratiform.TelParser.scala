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

import scala.language.unsafeNulls

import java.nio.charset.StandardCharsets

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import vacuous.*

import TelError.Reason

// Phase 1 presentation-only parser. Single-pass recursive descent over the
// byte input, producing the §17 presentation AST. Schema-driven type
// assignment and full error-recovery semantics arrive in phase 3.
//
// Hot-path optimisations (see `lib/jacinta/src/core/jacinta.JsonParser.scala`
// for the JSON analogue):
//
//   - The source is read as raw `Array[Byte]`; no upfront `String(bytes,
//     "UTF-8")` decode happens. ASCII syntax characters (LF, CR, space,
//     `#`, …) are byte-compared directly; multi-byte UTF-8 sequences
//     within keywords and atoms are decoded once at materialisation time
//     by `bytesSubstring`.
//   - `Line` carries only `(start, end, leadingSpaces, blank)` byte
//     offsets into the source.  Per-line substring allocations are
//     avoided.
//   - `parseCompoundLine` tracks an inclusive byte range per inline atom
//     and decodes it as UTF-8 only at commit time — no `StringBuilder` is
//     ever allocated for the atom-building loop.
//   - Keywords are interned through a 64-slot open-addressed cache keyed
//     by two-Long packed bytes (≤16-byte keywords; for ASCII this is up
//     to a 16-character keyword) so repeated keywords reuse the same
//     `Text` instance.
//   - The compound-loop indent check is hoisted above `isCommentLine` /
//     `isTabulationLineAt` via body-only variants (`isCommentBody` /
//     `isTabulationBody`).
//   - Ancestor stack push/pop is skipped entirely in no-schema mode.

object TelParser:

  def parse(input: Data): Tel.Document raises TelError =
    TelParser(input, Unset).parseDocument()

  // Schema-aware parse: when an odd-indented line is encountered the
  // parser uses §19.5's schema-aware E107 recovery to disambiguate
  // shallower vs deeper. Without a schema the original shallower-wins
  // rule still applies.
  def parse(input: Data, schema: Tels): Tel.Document raises TelError =
    TelParser(input, schema: Optional[Tels]).parseDocument()

  private final class Line
     ( val leadingSpaces: Int,
       val blank:         Boolean,
       val start:         Int,
       val end:           Int )

  // Byte literals for ASCII syntax characters, factored out so the hot
  // loops compare against named constants rather than magic numbers.
  private inline val ByteLf       = 0x0A
  private inline val ByteCr       = 0x0D
  private inline val ByteSpace    = 0x20
  private inline val ByteHash     = 0x23  // default sigil
  private inline val ByteSlash    = 0x2F
  private inline val ByteColon    = 0x3A
  private inline val ByteBang     = 0x21
  private inline val ByteDot      = 0x2E
  private inline val ByteT        = 0x74  // 't'
  private inline val ByteE        = 0x65  // 'e'
  private inline val ByteL        = 0x6C  // 'l'
  // UTF-8 BOM: EF BB BF.
  private inline val ByteBom0 = 0xEF
  private inline val ByteBom1 = 0xBB
  private inline val ByteBom2 = 0xBF

private final class TelParser(input: Data, schema: Optional[Tels]):
  import TelParser.*

  // The raw input bytes. We use the input's underlying `IArray[Byte]`
  // (cast to `Array[Byte]`) directly — no defensive copy, no upfront
  // `String` decode. The cost of `String(bytes, "UTF-8")` is two
  // memory-bandwidth passes (a Latin-1 detection scan plus an internal
  // copy); we eliminate both and decode UTF-8 lazily only at the points
  // where the parser actually materialises a `Text`.
  private val bytes: Array[Byte] = input.asInstanceOf[IArray[Byte]].asInstanceOf[Array[Byte]]

  // Unsigned byte read. Java arrays return signed bytes; we mask with
  // 0xFF so comparisons against ASCII byte constants behave as expected
  // for the full 0..255 range (multi-byte UTF-8 lead bytes are ≥0x80
  // and would compare as negative if left signed).
  private inline def byteAt(i: Int): Int = bytes(i) & 0xFF

  // Materialise the byte range `[from, until)` as a UTF-8 `String`. This
  // is the only path that does UTF-8 decoding in the parser; everything
  // else operates on raw bytes.
  private inline def bytesSubstring(from: Int, until: Int): String =
    new String(bytes, from, until - from, StandardCharsets.UTF_8)

  // Inline accessors for line content backed by `bytes`. The `start`/
  // `end` offsets are absolute byte positions into `bytes`; `end`
  // excludes a CR preceding LF in CRLF mode so the visible content
  // length is simply `end - start`.
  private inline def lineLen(line: Line): Int = line.end - line.start
  private inline def lineByteAt(line: Line, i: Int): Int = bytes(line.start + i) & 0xFF

  private inline def lineSub(line: Line, from: Int, until: Int): String =
    bytesSubstring(line.start + from, line.start + until)

  private inline def lineSubFrom(line: Line, from: Int): String =
    bytesSubstring(line.start + from, line.end)

  // Byte-level prefix check at an absolute offset into `bytes`. Returns
  // true if `prefix` matches the bytes starting at `from`. The prefix
  // is encoded as UTF-8; this works for any prefix string but is only
  // used here with ASCII literals.
  private def bytesStartsWithAt(prefix: Array[Byte], from: Int): Boolean =
    if from < 0 || from + prefix.length > bytes.length then false
    else
      var i = 0
      while i < prefix.length && bytes(from + i) == prefix(i) do i += 1
      i == prefix.length

  private inline def lineStartsWith(line: Line, prefix: Array[Byte]): Boolean =
    bytesStartsWithAt(prefix, line.start)

  // Find the first occurrence of `needle` in `bytes` at or after
  // `from`, or -1 if not found. Used by literal-atom closing-delimiter
  // search.
  private def bytesIndexOf(needle: Array[Byte], from: Int): Int =
    val n = needle.length
    if n == 0 then return from
    val maxStart = bytes.length - n
    val first = needle(0)
    var i = from
    while i <= maxStart do
      if bytes(i) == first then
        var j = 1
        while j < n && bytes(i + j) == needle(j) do j += 1
        if j == n then return i
      i += 1
    -1

  // Pre-encoded byte sequences for the few ASCII literals that the
  // parser searches for. Defined once so we don't re-encode them on
  // every call.
  private val ShebangBytes:    Array[Byte] = Array(ByteHash.toByte, ByteBang.toByte)
  private val TelKeywordBytes: Array[Byte] =
    Array(ByteT.toByte, ByteE.toByte, ByteL.toByte, ByteSpace.toByte)

  private val lineEndings: Tel.LineEndings =
    var i = 0
    while i < bytes.length && (bytes(i) & 0xFF) != ByteLf do i += 1
    if i > 0 && i < bytes.length && (bytes(i - 1) & 0xFF) == ByteCr then Tel.LineEndings.Crlf
    else Tel.LineEndings.Lf

  // Detect line-ending violations per §17 / E121. A CR that is not part
  // of a CR LF pair is a bare CR and raises E121. In CRLF mode, a bare
  // LF (one not preceded by CR) outside of literal-atom payloads also
  // raises E121.
  private def checkLineEndings(): Unit raises TelError =
    var i = 0
    val crlfMode = lineEndings == Tel.LineEndings.Crlf
    val n = bytes.length
    while i < n do
      val c = bytes(i) & 0xFF
      if c == ByteCr && (i + 1 >= n || (bytes(i + 1) & 0xFF) != ByteLf)
      then errorAtOffset(Reason.BadLineEnding, i)
      else if crlfMode && c == ByteLf && (i == 0 || (bytes(i - 1) & 0xFF) != ByteCr)
      then errorAtOffset(Reason.BadLineEnding, i)
      i += 1

  // Pre-split lines retaining leading-space count and blank flag. CR before
  // LF is stripped in CRLF mode. The final stretch of content after the
  // last LF is included only if it has content; an empty stretch after a
  // trailing LF still counts as a blank line for §17 trailing-blank-line
  // bookkeeping.
  private val lines: IArray[Line] =
    val builder = scala.collection.mutable.ArrayBuffer.empty[Line]
    val n = bytes.length
    var lineStart = 0
    var index = 0
    while index < n do
      if (bytes(index) & 0xFF) == ByteLf then
        val contentEnd =
          if index > 0 && (bytes(index - 1) & 0xFF) == ByteCr then index - 1
          else index

        builder += makeLine(lineStart, contentEnd)
        lineStart = index + 1

      index += 1

    if lineStart < n then
      builder += makeLine(lineStart, n)
    else if n > 0 && (bytes(n - 1) & 0xFF) == ByteLf then
      builder += makeLine(n, n)

    IArray.from(builder)

  private def makeLine(start: Int, end: Int): Line =
    var spaces = 0
    while start + spaces < end && (bytes(start + spaces) & 0xFF) == ByteSpace do spaces += 1
    val blank = start + spaces == end
    Line(spaces, blank, start, end)

  private var cursor: Int = 0
  private var margin: Int = 0
  // Sigil byte. Restricted to a single-byte (ASCII) character by the
  // pragma parser; supporting multi-byte sigils would require a slower
  // byte-sequence compare in the inner loops. -1 means uninitialised
  // (still defaulting to '#').
  private var sigilByte: Int = ByteHash
  private var sigilChar: Char = '#'

  // The highest line index consumed by the prologue (interpreter
  // directive or pragma). The E109 check treats anything at or before
  // this line as equivalent to "start of file" — comments immediately
  // after the prologue are valid.
  private var prologueEndLine: Int = -1

  // Ancestor stack of Struct types known for each open compound, used
  // by the schema-aware E107 recovery rule of §19.5.
  private val ancestors =
    scala.collection.mutable.ArrayBuffer.empty[Optional[Tels.Struct]]

  // Build a `TelError.Position` for a given line index (0-based internally;
  // converted to the 1-indexed line numbers callers expect).
  private inline def positionAt(lineIdx: Int, column: Int = 1): TelError.Position =
    if lineIdx < 0 || lineIdx >= lines.length then TelError.Position(1, 1)
    else TelError.Position(lineIdx + 1, column)

  // Compute a position from an absolute byte offset into `bytes`.
  private def positionAtOffset(byteOffset: Int): TelError.Position =
    var lineIdx = 0
    while lineIdx < lines.length && lines(lineIdx).end <= byteOffset do lineIdx += 1
    if lineIdx >= lines.length then
      val n = lines.length.max(1)
      TelError.Position(n, 1)
    else
      val line = lines(lineIdx)
      TelError.Position(lineIdx + 1, byteOffset - line.start + 1)

  private def errorAt(reason: Reason, lineIdx: Int, column: Int = 1)
                     (using Tactic[TelError])
  :   Nothing =

    abort(TelError(reason, positionAt(lineIdx, column)))

  private def errorAtOffset(reason: Reason, byteOffset: Int)
                           (using Tactic[TelError])
  :   Nothing =

    abort(TelError(reason, positionAtOffset(byteOffset)))

  // Reverse-lookup of a `Line` instance's index in `lines`. Only the
  // error path hits this, so the linear scan is fine.
  private def lineIndexOf(line: Line): Int =
    var i = 0
    while i < lines.length && (lines(i) ne line) do i += 1
    if i < lines.length then i else -1

  private def errorAtLine(reason: Reason, line: Line, column: Int = 1)
                         (using Tactic[TelError])
  :   Nothing =

    errorAt(reason, lineIndexOf(line), column)

  // Keyword interning cache. Keywords of length ≤16 bytes are packed
  // into two Longs (8 bytes per Long); a hash + linear-probe lookup
  // returns the same `Text` for every occurrence of the same keyword
  // bytes. Longer keywords skip the cache.
  private val keyCache:     Array[String] = new Array[String](64)
  private val keyCacheLow:  Array[Long]   = new Array[Long](64)
  private val keyCacheHigh: Array[Long]   = new Array[Long](64)

  // Look up or insert the byte range `bytes[from, until)` in the keyword
  // cache. Returns a `Text` whose underlying String is shared across
  // every line that names the same keyword. Two ≤16-byte byte sequences
  // collide in the (low, high) packing only if they have identical
  // bytes, so direct equality is sufficient — no fallback string compare
  // needed.
  private def internKeyword(from: Int, until: Int): Text =
    val len = until - from
    if len <= 0 then Text("")
    else if len > 16 then Text(bytesSubstring(from, until))
    else
      var low:  Long = 0L
      var high: Long = 0L
      var idx = 0
      while idx < len && idx < 8 do
        low |= (bytes(from + idx).toLong & 0xFF) << (idx * 8)
        idx += 1

      while idx < len do
        high |= (bytes(from + idx).toLong & 0xFF) << ((idx - 8) * 8)
        idx += 1

      val hash = ((low ^ (low >>> 32)) ^ (high ^ (high >>> 17))).toInt
      var slot = hash & 0x3F
      var probes = 0
      while probes < 4 do
        val existing = keyCache(slot)
        if existing == null then
          val str = bytesSubstring(from, until)
          keyCache(slot) = str
          keyCacheLow(slot) = low
          keyCacheHigh(slot) = high
          return Text(str)
        else if keyCacheLow(slot) == low && keyCacheHigh(slot) == high then
          return Text(existing)
        slot = (slot + 1) & 0x3F
        probes += 1

      Text(bytesSubstring(from, until))

  // Push a child compound's resolved struct onto the ancestor stack.
  private def pushAncestor(keyword: Text): Unit raises TelError =
    schema.let: s =>
      val parent: Optional[Tels.Struct] =
        if ancestors.isEmpty then s.document else ancestors(ancestors.length - 1)

      val resolved: Optional[Tels.Struct] = parent.let: p =>
        resolveKeywordStruct(p, keyword, s)

      ancestors += resolved
    .or:
      ancestors += Unset

  private def popAncestor(): Unit =
    if ancestors.nonEmpty then ancestors.remove(ancestors.length - 1)

  private def resolveKeywordStruct(parent: Tels.Struct, keyword: Text, schema: Tels)
  :     Optional[Tels.Struct] raises TelError =
    val matched: Optional[Tels.Type] =
      var found: Optional[Tels.Type] = Unset
      var i = 0
      while i < parent.members.length && found.absent do
        parent.members(i) match
          case f: Tels.Field =>
            if f.keyword == keyword then found = f.fieldType

          case s: Tels.SelectRef =>
            schema.selects.find(_.name == s.reference).foreach: selectDef =>
              selectDef.variants.find(_.keyword == keyword).foreach: variant =>
                found = variant.variantType

          case _: Tels.Exclude => ()

        i += 1

      found

    matched.let: t =>
      resolveTypeToStruct(t, schema)
    .or(Unset)

  private def resolveTypeToStruct(t: Tels.Type, schema: Tels)
  :     Optional[Tels.Struct] =
    t match
      case s: Tels.Struct =>
        s

      case Tels.Reference(name) =>
        schema.records.find(_.name == name) match
          case Some(rec) => Tels.Struct(rec.members, rec.validators)
          case None      => Unset

      case _ =>
        Unset

  private def keywordAdmissible(parent: Tels.Struct, keyword: Text, schema: Tels): Boolean =
    var i = 0
    while i < parent.members.length do
      parent.members(i) match
        case f: Tels.Field =>
          if f.keyword == keyword then return true

        case s: Tels.SelectRef =>
          schema.selects.find(_.name == s.reference).foreach: selectDef =>
            if selectDef.variants.exists(_.keyword == keyword) then return true

        case _: Tels.Exclude => ()

      i += 1

    false

  // Extract the keyword phrase from a non-blank, non-comment,
  // non-tabulation line — used by the schema-aware E107 recovery.
  private def extractKeyword(line: Line): Text =
    val len = lineLen(line)
    val start = line.leadingSpaces
    var end = start
    while end < len
      && lineByteAt(line, end) != ByteSpace
      && lineByteAt(line, end) != sigilByte
    do end += 1

    internKeyword(line.start + start, line.start + end)

  def parseDocument(): Tel.Document raises TelError =
    checkBom()
    checkLineEndings()
    val directive = parseInterpreterDirective()
    val pragma = parsePragma()

    if directive.absent && pragma.absent then determineMargin()
    else margin = 0

    val children = parseChildren(parentIndent = -1)

    Tel.Document(directive, pragma, lineEndings, children)

  private def checkBom(): Unit raises TelError =
    if bytes.length >= 3
       && (bytes(0) & 0xFF) == ByteBom0
       && (bytes(1) & 0xFF) == ByteBom1
       && (bytes(2) & 0xFF) == ByteBom2
    then errorAtOffset(Reason.BomPresent, 0)

  private def parseInterpreterDirective(): Optional[Text] =
    if cursor >= lines.length then Unset
    else if lineStartsWith(lines(cursor), ShebangBytes) then
      val payload = lineSubFrom(lines(cursor), 2)
      prologueEndLine = cursor
      cursor += 1
      Text(payload)
    else Unset

  private def parsePragma(): Optional[Tel.Pragma] raises TelError =
    val firstNonBlank = peekNextNonBlankLine()
    val result = firstNonBlank.let: idx =>
      val line = lines(idx)
      // A pragma-shaped line is either exactly "tel" or starts with
      // "tel " (after leading indentation). Detect this at the byte
      // level so we don't materialise a String per non-pragma line.
      val ls = line.leadingSpaces
      val len = lineLen(line)
      val isExactTel =
        len - ls == 3
        && lineByteAt(line, ls) == ByteT
        && lineByteAt(line, ls + 1) == ByteE
        && lineByteAt(line, ls + 2) == ByteL
      val hasTelPrefix =
        len - ls >= 4
        && lineByteAt(line, ls) == ByteT
        && lineByteAt(line, ls + 1) == ByteE
        && lineByteAt(line, ls + 2) == ByteL
        && lineByteAt(line, ls + 3) == ByteSpace
      if isExactTel || hasTelPrefix then
        if line.start >= 4096 || line.end > 4096
        then errorAt(Reason.PragmaTooLong, idx)
        prologueEndLine = idx
        cursor = idx + 1
        Optional(parsePragmaContent(lineSubFrom(line, ls), idx))
      else Unset
    .or(Unset)

    if result.absent then
      var j = firstNonBlank.or(0) + 1
      while j < lines.length do
        val ln = lines(j)
        if !ln.blank && ln.leadingSpaces == 0 then
          if lineLen(ln) == 3 && lineByteAt(ln, 0) == ByteT && lineByteAt(ln, 1) == ByteE
            && lineByteAt(ln, 2) == ByteL
          then errorAt(Reason.PragmaNotFirst, j)
          else if lineStartsWith(ln, TelKeywordBytes) then errorAt(Reason.PragmaNotFirst, j)

        j += 1

    result

  private def parsePragmaContent(content: String, lineIdx: Int): Tel.Pragma raises TelError =
    val parts = splitPhrases(content)
    if parts.head != "tel" then errorAt(Reason.PragmaNotFirst, lineIdx)
    val version =
      if parts.length >= 2 then parseVersion(parts(1), lineIdx)
      else (1, 0)

    if parts.length > 4 then errorAt(Reason.ExtraPragmaContent, lineIdx)

    val schema: Optional[Text] =
      if parts.length >= 3 then
        val s = parts(2)
        val isUrl = s.indexOf("://") >= 0
        val isHexHash = s.length == 64 && s.forall: c =>
          (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
        val isBase256 = s.exists(_.toInt > 127)
        if !isUrl && !isHexHash && !isBase256
        then errorAt(Reason.BadSchemaIdentifier, lineIdx)
        Text(s): Optional[Text]
      else Unset

    val pragmaSigil: Optional[Char] =
      if parts.length >= 4 && parts(3).length == 1 then
        val c = parts(3).charAt(0)
        // §8.3: must be a symbolic character. We additionally require
        // it be a single byte (i.e. ASCII) so the byte-level inner-loop
        // sigil compare works without a multi-byte fallback.
        if c.isLetterOrDigit || c.toInt > 127 then errorAt(Reason.BadSigil, lineIdx)
        sigilChar = c
        sigilByte = c.toInt
        c: Optional[Char]
      else Unset

    Tel.Pragma(version, schema, pragmaSigil)

  private def parseVersion(s: String, lineIdx: Int): (Int, Int) raises TelError =
    val dot = s.indexOf('.')
    if dot <= 0 || dot == s.length - 1 then errorAt(Reason.BadVersion, lineIdx)
    try
      val major = s.substring(0, dot).toInt
      val minor = s.substring(dot + 1).toInt
      if major < 0 || minor < 0 then errorAt(Reason.BadVersion, lineIdx)
      (major, minor)
    catch case _: NumberFormatException => errorAt(Reason.BadVersion, lineIdx)

  private def splitPhrases(content: String): List[String] =
    val parts = scala.collection.mutable.ListBuffer.empty[String]
    val builder = StringBuilder()
    var i = 0
    var hardSpaceMode = false
    while i < content.length do
      val ch = content.charAt(i)
      if ch == ' ' then
        var j = i
        while j < content.length && content.charAt(j) == ' ' do j += 1
        val runLength = j - i
        if !hardSpaceMode && runLength == 1 then
          if builder.nonEmpty then { parts += builder.toString; builder.clear() }
          i += 1
        else
          if !hardSpaceMode then hardSpaceMode = true
          if builder.nonEmpty then { parts += builder.toString; builder.clear() }
          i = j
      else
        builder.append(ch)
        i += 1

    if builder.nonEmpty then parts += builder.toString

    parts.toList

  private def determineMargin(): Unit =
    peekNextNonBlankLine().let: idx =>
      margin = lines(idx).leadingSpaces

  private def peekNextNonBlankLine(): Optional[Int] =
    var i = cursor
    while i < lines.length && lines(i).blank do i += 1
    if i < lines.length then i else Unset

  private def indentOf(line: Line): Int raises TelError =
    val relative = line.leadingSpaces - margin
    if relative < 0 then errorAtLine(Reason.LessThanMargin, line)
    else if relative % 2 == 0 then relative / 2
    else recoverOddIndent(line, relative)

  private def recoverOddIndent(line: Line, relative: Int): Int raises TelError =
    schema.let: s =>
      val shallower = relative / 2
      val deeper    = shallower + 1
      val keyword   = extractKeyword(line)

      val shallowerParent: Optional[Tels.Struct] =
        if shallower == 0 then s.document
        else if shallower - 1 < ancestors.length then ancestors(shallower - 1)
        else Unset

      val deeperParent: Optional[Tels.Struct] =
        if shallower < ancestors.length then ancestors(shallower)
        else Unset

      val shallowerValid =
        shallowerParent.let(p => keywordAdmissible(p, keyword, s)).or(false)

      val deeperValid =
        deeperParent.let(p => keywordAdmissible(p, keyword, s)).or(false)

      if !shallowerValid && deeperValid then deeper else shallower
    .or:
      errorAtLine(Reason.OddIndentation, line)

  private def checkTrailingSpaces(line: Line): Unit raises TelError =
    val len = lineLen(line)
    if len > 0 && lineByteAt(line, len - 1) == ByteSpace && !line.blank
    then errorAtLine(Reason.TrailingSpaces, line, len)

  private def parseChildren(parentIndent: Int): IArray[Tel.Block] raises TelError =
    val expected = parentIndent + 1

    val firstIdx = peekNextNonBlankLine()
    if firstIdx.absent then return IArray.empty[Tel.Block]
    val firstResolved = firstIdx.vouch
    val firstIndent = indentOf(lines(firstResolved))
    if firstIndent < expected then return IArray.empty[Tel.Block]

    val builder = scala.collection.mutable.ArrayBuffer.empty[Tel.Block]

    while
      val nextIdx = peekNextNonBlankLine()
      if nextIdx.absent then false
      else
        val idx = nextIdx.vouch
        if indentOf(lines(idx)) == expected then
          cursor = idx
          true
        else false
    do
      builder += parseBlock(expected)

    val rest = peekNextNonBlankLine()
    rest.let: idx =>
      val di = indentOf(lines(idx))
      if di > expected then builder.lastOption match
        case Some(last) if last.tabulation.present && last.compounds.isEmpty =>
          errorAt(Reason.RowWrongIndent, idx)

        case Some(last) if last.tabulation.present =>
          errorAt(Reason.ChildOfNonCompound, idx)

        case Some(last) if last.compounds.isEmpty =>
          errorAt(Reason.ChildOfNonCompound, idx)

        case _ => errorAt(Reason.OverIndentation, idx)

    IArray.from(builder)

  private def parseBlock(indent: Int): Tel.Block raises TelError =
    val comments = scala.collection.mutable.ArrayBuffer.empty[Tel.Comment]

    while atCommentLine(indent) do
      if cursor > 0 && cursor - 1 > prologueEndLine then
        val prev = lines(cursor - 1)
        if !prev.blank && !isCommentLine(prev, indent) && prev.leadingSpaces >= margin + indent * 2
        then errorAt(Reason.CommentNotPreceded, cursor)

      comments += parseCommentLine(lines(cursor))
      cursor += 1

    if comments.nonEmpty && cursor < lines.length && lines(cursor).blank then
      var probe = cursor
      while probe < lines.length && lines(probe).blank do probe += 1
      if probe < lines.length && indentOf(lines(probe)) == indent
        && !isCommentLine(lines(probe), indent)
      then cursor = probe

    val tabulation: Optional[Tel.Tabulation] =
      if cursor < lines.length && isTabulationLineAt(lines(cursor), indent) then
        val line = lines(cursor)
        cursor += 1
        Optional(parseTabulationLine(line))
      else Unset

    val compounds = scala.collection.mutable.ArrayBuffer.empty[Tel.Compound]

    var continueLoop = true
    while continueLoop && cursor < lines.length do
      val line = lines(cursor)
      if line.blank || indentOf(line) != indent then continueLoop = false
      else if isCommentBody(line) || isTabulationBody(line) then continueLoop = false
      else
        cursor += 1
        val parsed = parseCompoundLine(line, indent)
        tabulation.let(validateTabulatedRow(line, _))
        val extraAtom =
          if tabulation.absent then parseSourceOrLiteralAtom(line.leadingSpaces)
          else Unset

        val children =
          if extraAtom.absent && tabulation.absent then
            if schema.present then
              pushAncestor(parsed.keyword)
              try parseChildren(indent) finally popAncestor()
            else parseChildren(indent)
          else IArray.empty[Tel.Block]

        val finalAtoms = extraAtom.lay(parsed.atoms): atom =>
          parsed.atoms :+ atom

        compounds += parsed.copy(atoms = finalAtoms, children = children)

    val trailingBlankLines = consumeTrailingBlanksFor(indent)

    Tel.Block(IArray.from(comments), tabulation, IArray.from(compounds), trailingBlankLines)

  private def validateTabulatedRow(line: Line, tabulation: Tel.Tabulation)
  :     Unit raises TelError =
    val markers = tabulation.markerOffsets
    val n = lineLen(line)

    var i = line.leadingSpaces
    var columnIdx = 0
    var phraseStart = i

    while i < n do
      if lineByteAt(line, i) == ByteSpace then
        var j = i
        while j < n && lineByteAt(line, j) == ByteSpace do j += 1
        val runLen = j - i

        if runLen >= 2 then
          val isRemark =
            j < n
            && lineByteAt(line, j) == sigilByte
            && (j + 1 >= n || (lineByteAt(line, j + 1) == ByteSpace
                               && (j + 2 >= n || lineByteAt(line, j + 2) != ByteSpace)))

          if isRemark then i = n
          else
            if columnIdx >= 1 && columnIdx < markers.length - 1 then
              val phraseWidth = i - phraseStart
              val colMax = markers(columnIdx + 1) - markers(columnIdx) - 2
              if phraseWidth > colMax then
                errorAtLine(Reason.ColumnValueTooWide, line, phraseStart + 1)

            var foundIdx = -1
            var k = 1
            while k < markers.length && foundIdx < 0 do
              if markers(k) == j then foundIdx = k
              k += 1

            if foundIdx < 0 then errorAtLine(Reason.HardSpaceWrongPosition, line, j + 1)

            columnIdx = foundIdx
            phraseStart = j
            i = j
        else i = j
      else i += 1

  private def isTabulationLineAt(line: Line, indent: Int): Boolean raises TelError =
    if line.blank || indentOf(line) != indent then false
    else isTabulationBody(line)

  private inline def isTabulationBody(line: Line): Boolean =
    val start = line.leadingSpaces
    start < lineLen(line)
    && lineByteAt(line, start) == sigilByte
    && hasTabulationMarker(line, start)

  private inline def isCommentBody(line: Line): Boolean =
    val len = lineLen(line)
    val start = line.leadingSpaces
    if start >= len || lineByteAt(line, start) != sigilByte then false
    else
      val next = start + 1
      if next >= len then true
      else if lineByteAt(line, next) == ByteSpace then !hasTabulationMarker(line, start)
      else false

  private def parseTabulationLine(line: Line): Tel.Tabulation raises TelError =
    val n = lineLen(line)
    val markers = scala.collection.mutable.ArrayBuffer.empty[Int]

    val first = line.leadingSpaces
    markers += first
    var i = first + 1
    while i < n do
      if lineByteAt(line, i) == ByteSpace then
        var j = i
        while j < n && lineByteAt(line, j) == ByteSpace do j += 1
        val runLen = j - i
        if runLen >= 2 && j < n && lineByteAt(line, j) == sigilByte then
          markers += j
          i = j + 1
        else i = j
      else i += 1

    val headings = scala.collection.mutable.ArrayBuffer.empty[Text]
    var m = 0
    while m < markers.length do
      val markerPos = markers(m)
      val nextLimit = if m + 1 < markers.length then markers(m + 1) else n
      headings += extractHeading(line, markerPos + 1, nextLimit)
      m += 1

    Tel.Tabulation(IArray.from(markers), IArray.from(headings))

  private def extractHeading(line: Line, start: Int, limit: Int): Text raises TelError =
    if start >= limit then t""
    else if lineByteAt(line, start) != ByteSpace then
      errorAtLine(Reason.BadTabulationHeading, line, start + 1)
    else if start + 1 < limit && lineByteAt(line, start + 1) == ByteSpace then
      if limit - start <= 2 then t""
      else errorAtLine(Reason.BadTabulationHeading, line, start + 1)
    else
      var i = start + 1
      var stop = limit
      while i < limit - 1 do
        if lineByteAt(line, i) == ByteSpace && lineByteAt(line, i + 1) == ByteSpace then
          stop = i
          i = limit
        else
          if lineByteAt(line, i) == sigilByte then
            errorAtLine(Reason.BadTabulationHeading, line, i + 1)
          i += 1

      Text(lineSub(line, start + 1, stop))

  private def consumeTrailingBlanksFor(indent: Int): Int raises TelError =
    var count = 0
    while
      cursor < lines.length && lines(cursor).blank
      && nextNonBlankMatches(indent)
    do
      count += 1
      cursor += 1

    count

  private def nextNonBlankMatches(indent: Int): Boolean raises TelError =
    var probe = cursor
    while probe < lines.length && lines(probe).blank do probe += 1
    if probe >= lines.length then true
    else indentOf(lines(probe)) == indent

  private def atCommentLine(indent: Int): Boolean raises TelError =
    cursor < lines.length && isCommentLine(lines(cursor), indent)

  private def isCommentLine(line: Line, indent: Int): Boolean raises TelError =
    if line.blank || indentOf(line) != indent then false
    else isCommentBody(line)

  private def hasTabulationMarker(line: Line, fromIdx: Int): Boolean =
    val n = lineLen(line)
    var i = fromIdx + 1
    while i < n - 1 do
      if lineByteAt(line, i) == ByteSpace && lineByteAt(line, i + 1) == ByteSpace then
        var j = i
        while j < n && lineByteAt(line, j) == ByteSpace do j += 1
        if j < n && lineByteAt(line, j) == sigilByte then return true
        i = j
      else i += 1

    false

  private def parseSourceOrLiteralAtom(compoundLeadingSpaces: Int)
  : Optional[Tel.Atom] raises TelError =
    if cursor >= lines.length || lines(cursor).blank then Unset
    else
      val line = lines(cursor)
      val sourceIndent = compoundLeadingSpaces + 4
      val literalIndent = compoundLeadingSpaces + 6
      val first =
        if line.leadingSpaces == literalIndent then Optional(parseLiteralAtom(literalIndent))
        else if line.leadingSpaces == sourceIndent then Optional(parseSourceAtom(sourceIndent))
        else Unset

      if first.present && cursor < lines.length && !lines(cursor).blank then
        val nextLine = lines(cursor)
        if nextLine.leadingSpaces == literalIndent
        then errorAt(Reason.DuplicateLiteral, cursor)
        else if nextLine.leadingSpaces == sourceIndent
        then errorAt(Reason.DuplicateSource, cursor)

      first

  private def parseSourceAtom(sourceIndent: Int): Tel.Atom.Source =
    val captured = scala.collection.mutable.ListBuffer.empty[String]

    var done = false
    while !done && cursor < lines.length do
      val line = lines(cursor)
      if line.blank then
        var probe = cursor
        while probe < lines.length && lines(probe).blank do probe += 1
        val keep =
          probe >= lines.length
          || lines(probe).leadingSpaces >= sourceIndent

        if keep then
          while cursor < probe do
            captured += ""
            cursor += 1
        else done = true
      else if line.leadingSpaces >= sourceIndent then
        val len = lineLen(line)
        var endIdx = len
        while endIdx > sourceIndent && lineByteAt(line, endIdx - 1) == ByteSpace do endIdx -= 1
        captured += lineSub(line, sourceIndent, endIdx)
        cursor += 1
      else done = true

    val sb = StringBuilder()
    captured.foreach: c =>
      sb.append(c)
      sb.append('\n')

    Tel.Atom.Source(Text(sb.toString))

  private def parseLiteralAtom(literalIndent: Int): Tel.Atom.Literal raises TelError =
    val openingLine = lines(cursor)
    val delimiter = lineSubFrom(openingLine, literalIndent)
    val delimiterBytes = delimiter.getBytes(StandardCharsets.UTF_8)
    cursor += 1
    val payloadStart = openingLine.end + (if lineEndings == Tel.LineEndings.Crlf then 2 else 1)

    // Empty payload (§15): the opening line's terminating LF is also
    // the closing pattern's leading LF, so the search-from-payloadStart
    // path can't match — handle it explicitly.
    val emptyPatternLen = delimiterBytes.length + 1
    val isEmpty =
      payloadStart + emptyPatternLen <= bytes.length
      && {
        var i = 0
        while i < delimiterBytes.length && bytes(payloadStart + i) == delimiterBytes(i) do i += 1
        i == delimiterBytes.length
          && (bytes(payloadStart + delimiterBytes.length) & 0xFF) == ByteLf
      }

    val (rawPayload, afterClose) =
      if isEmpty then ("", payloadStart + emptyPatternLen)
      else
        // Search for `\n<delimiter>\n`.
        val pattern = new Array[Byte](delimiterBytes.length + 2)
        pattern(0) = ByteLf.toByte
        System.arraycopy(delimiterBytes, 0, pattern, 1, delimiterBytes.length)
        pattern(pattern.length - 1) = ByteLf.toByte
        val closeIdx = bytesIndexOf(pattern, payloadStart)
        if closeIdx < 0 then errorAtLine(Reason.UnclosedLiteral, openingLine)
        (bytesSubstring(payloadStart, closeIdx), closeIdx + pattern.length)

    val payload = rawPayload.replace("\r\n", "\n")

    while cursor < lines.length && lines(cursor).start < afterClose do cursor += 1

    Tel.Atom.Literal(Text(delimiter), Text(payload))

  private def parseCommentLine(line: Line): Tel.Comment =
    val len = lineLen(line)
    val start = line.leadingSpaces + 1
    val payload =
      if start >= len then ""
      else if lineByteAt(line, start) == ByteSpace then lineSubFrom(line, start + 1)
      else lineSubFrom(line, start)

    Tel.Comment(Text(payload))

  // Splits the post-indent content of an ordinary line into a keyword (the
  // first phrase), subsequent inline atoms with their preceding-space
  // counts, and an optional remark per §10.3 / §11.2. Source and literal
  // atoms are added in a separate pass.
  //
  // Atoms are accumulated as byte ranges into `bytes` (no `StringBuilder`)
  // and decoded as UTF-8 only at commit time. Embedded soft-spaces in
  // hard-space mode are naturally included in the range because we
  // don't skip them.
  private def parseCompoundLine(line: Line, indent: Int): Tel.Compound raises TelError =
    checkTrailingSpaces(line)
    val len = lineLen(line)
    val ls = line.leadingSpaces
    val base = line.start

    // First phrase = keyword (precedingSpaces = 0, never a remark).
    var i = ls
    val keywordStart = i
    while i < len && lineByteAt(line, i) != ByteSpace do i += 1
    val keyword = internKeyword(base + keywordStart, base + i)

    val atoms = scala.collection.mutable.ArrayBuffer.empty[Tel.Atom]
    // Byte-range marker for the current open atom. -1 ≡ no atom open.
    var atomStart = -1
    var precedingSpaces = 0
    var hardSpaceMode = false
    var remark: Optional[Text] = Unset

    inline def commit(endPos: Int): Unit =
      if atomStart >= 0 then
        val text = Text(bytesSubstring(base + atomStart, base + endPos))
        atoms += Tel.Atom.Inline(text, precedingSpaces)
        atomStart = -1

    while i < len && remark.absent do
      val b = lineByteAt(line, i)
      if b == ByteSpace then
        var j = i
        while j < len && lineByteAt(line, j) == ByteSpace do j += 1
        val run = j - i

        if hardSpaceMode then
          if run >= 2 then
            commit(i)
            precedingSpaces = run
            i = j
          else
            // Single soft space in hard-space mode: extends the current
            // atom (or opens one starting at this space).
            if atomStart < 0 then atomStart = i
            i = j
        else
          if run == 1 then
            commit(i)
            precedingSpaces = 1
            i = j
          else
            commit(i)
            precedingSpaces = run
            hardSpaceMode = true
            i = j
      else if b == sigilByte && atomStart < 0 then
        val afterSigil = i + 1
        val softSpaceAfter =
          afterSigil < len
          && lineByteAt(line, afterSigil) == ByteSpace
          && (afterSigil + 1 >= len || lineByteAt(line, afterSigil + 1) != ByteSpace)

        if softSpaceAfter then
          remark = Text(lineSubFrom(line, afterSigil + 1))
          i = len
        else
          atomStart = i
          i += 1
      else
        if atomStart < 0 then atomStart = i
        i += 1

    commit(i)

    Tel.Compound(keyword, IArray.from(atoms), remark, IArray.empty)
