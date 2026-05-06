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
package ypsiloid

import anticipation.*
import fulminate.*
import jacinta.Bcd
import vacuous.*
import zephyrine.*

// The parser representation of a YAML value. Modelled on Jacinta's
// `JsonAst`: an opaque union over the primitive JVM types so that
// primitive values are stored without case-class wrapping. Sequences
// and mappings are both stored as `IArray[Any]`, distinguished by
// length parity:
//
//   - mapping: even length (alternating key/value);
//   - sequence: odd length (with a trailing `arrayPad` sentinel when
//     the user-visible item count is even, so all sequences are
//     length-odd at the array level).
//
// `null` represents a YAML null. `Unset.type` marks an absent value
// (e.g. a missing field in a case-class derivation).
opaque type YamlAst =
  Long | Double | Bcd | Boolean | String | IArray[Any] | Null | Unset.type

object YamlAst extends Format:
  def name: Text = "YAML"

  case class Position
    ( line:                Int,
      column:              Int,
      override val offset: Optional[Int] = Unset,
      override val length: Optional[Int] = Unset )
  extends Format.Position:
    def describe: Text = ("line "+line+", column "+column).tt

  enum Issue extends Format.Issue:
    case DirectiveWithoutDocumentStart
    case DirectivesOutOfPlace
    case MissingDocumentStart
    case ContentAfterDocumentEnd
    case DuplicateYamlDirective
    case YamlDirectiveRequiresVersion
    case YamlDirectiveTooManyArguments
    case YamlDirectiveInvalidVersion
    case TagDirectiveRequiresHandleAndPrefix
    case TwoAnchorsOnSameNode
    case AnchorOnAlias
    case ReservedIndicatorAtNodeStart
    case DuplicateAnchorOnNode
    case DuplicateTagOnNode
    case UnterminatedVerbatimTag
    case BlockMappingOnDocumentStartLine
    case ChainedMappingValueOnSingleLine
    case MultilineImplicitKey
    case TrailingContentAfterQuotedScalar
    case UnterminatedDoubleQuotedString
    case UnterminatedEscape
    case InvalidEscapeSequence
    case TruncatedHexEscape
    case InvalidHexDigit
    case UnterminatedSingleQuotedString
    case DocumentMarkerInsideMultilineScalar
    case ScalarContinuationUnderIndented
    case UnterminatedFlowSequence
    case EmptyFlowSequenceEntry
    case FlowImplicitKeyAndColonOnDifferentLines
    case FlowSequenceExpectedCommaOrClose
    case UnterminatedFlowMapping
    case EmptyFlowMappingEntry
    case FlowMappingExpectedCommaOrClose
    case FlowContentUnderIndented
    case CommentMissingPrecedingWhitespace
    case DocumentMarkerInFlowContext
    case ReservedIndicatorAtFlowPlainScalarStart
    case BlockSequenceIndicatorNotAtLineStart
    case TabInIndentation
    case PlainScalarAtMappingIndentWithoutColon
    case ExpectedColonAfterMappingKey
    case BlockSequenceOnMappingKeyLine
    case InvalidBlockScalarIndentationIndicator
    case DuplicateBlockScalarIndentationIndicator
    case DuplicateBlockScalarChompingIndicator
    case InvalidBlockScalarHeader
    case BlockScalarHeaderCommentMissingWhitespace
    case BlockScalarLeadingBlanksOverIndented
    case UndefinedTagHandle(handle: Text)
    case UnknownAlias(name: Text)

    def describe: Message = this match
      case DirectiveWithoutDocumentStart =>
        m"a directive must be followed by a `---` document-start marker"
      case DirectivesOutOfPlace =>
        m"directives can only appear at the start of a stream or after `...`"
      case MissingDocumentStart =>
        m"a `---` document-start marker is missing between documents"
      case ContentAfterDocumentEnd =>
        m"unexpected content was found after the `...` document-end marker"
      case DuplicateYamlDirective =>
        m"the %YAML directive was given more than once"
      case YamlDirectiveRequiresVersion =>
        m"the %YAML directive requires a version argument"
      case YamlDirectiveTooManyArguments =>
        m"the %YAML directive takes a single version argument"
      case YamlDirectiveInvalidVersion =>
        m"the %YAML directive version must be `major.minor`"
      case TagDirectiveRequiresHandleAndPrefix =>
        m"the %TAG directive requires a `handle prefix` argument pair"
      case TwoAnchorsOnSameNode =>
        m"two anchors were given on the same node"
      case AnchorOnAlias =>
        m"an alias node cannot also have an anchor"
      case ReservedIndicatorAtNodeStart =>
        m"a reserved indicator was found at the start of a node"
      case DuplicateAnchorOnNode =>
        m"more than one anchor was given on a single node"
      case DuplicateTagOnNode =>
        m"more than one tag was given on a single node"
      case UnterminatedVerbatimTag =>
        m"the verbatim tag was not terminated"
      case BlockMappingOnDocumentStartLine =>
        m"a block mapping cannot start on the document-start line"
      case ChainedMappingValueOnSingleLine =>
        m"a chained mapping value is not allowed on a single line"
      case MultilineImplicitKey =>
        m"an implicit mapping key cannot span multiple lines"
      case TrailingContentAfterQuotedScalar =>
        m"unexpected trailing content was found after a quoted scalar"
      case UnterminatedDoubleQuotedString =>
        m"the double-quoted string was not terminated"
      case UnterminatedEscape =>
        m"the escape sequence was not terminated"
      case InvalidEscapeSequence =>
        m"the escape sequence was not valid"
      case TruncatedHexEscape =>
        m"the hex escape was truncated"
      case InvalidHexDigit =>
        m"a hex digit was expected"
      case UnterminatedSingleQuotedString =>
        m"the single-quoted string was not terminated"
      case DocumentMarkerInsideMultilineScalar =>
        m"a document marker was found inside a multi-line scalar"
      case ScalarContinuationUnderIndented =>
        m"a multi-line scalar continuation was insufficiently indented"
      case UnterminatedFlowSequence =>
        m"the flow sequence was not terminated"
      case EmptyFlowSequenceEntry =>
        m"a flow-sequence entry was empty"
      case FlowImplicitKeyAndColonOnDifferentLines =>
        m"the implicit mapping key and `:` must be on the same line"
      case FlowSequenceExpectedCommaOrClose =>
        m"`,` or `]` was expected in the flow sequence"
      case UnterminatedFlowMapping =>
        m"the flow mapping was not terminated"
      case EmptyFlowMappingEntry =>
        m"a flow-mapping entry was empty"
      case FlowMappingExpectedCommaOrClose =>
        m"`,` or `}` was expected in the flow mapping"
      case FlowContentUnderIndented =>
        m"the flow content was insufficiently indented"
      case CommentMissingPrecedingWhitespace =>
        m"a comment must be preceded by whitespace"
      case DocumentMarkerInFlowContext =>
        m"document markers are not allowed in flow style"
      case ReservedIndicatorAtFlowPlainScalarStart =>
        m"a reserved indicator was found at the start of a flow plain scalar"
      case BlockSequenceIndicatorNotAtLineStart =>
        m"a block-sequence indicator must start its line"
      case TabInIndentation =>
        m"a tab character was used in indentation"
      case PlainScalarAtMappingIndentWithoutColon =>
        m"a plain scalar at the mapping indent was missing its `:`"
      case ExpectedColonAfterMappingKey =>
        m"`:` was expected after the mapping key"
      case BlockSequenceOnMappingKeyLine =>
        m"a sequence cannot start on the same line as a mapping key"
      case InvalidBlockScalarIndentationIndicator =>
        m"the block-scalar indentation indicator must be 1-9"
      case DuplicateBlockScalarIndentationIndicator =>
        m"more than one block-scalar indentation indicator was given"
      case DuplicateBlockScalarChompingIndicator =>
        m"more than one block-scalar chomping indicator was given"
      case InvalidBlockScalarHeader =>
        m"the block-scalar header was not valid"
      case BlockScalarHeaderCommentMissingWhitespace =>
        m"a comment in the block-scalar header must be preceded by whitespace"
      case BlockScalarLeadingBlanksOverIndented =>
        m"the leading empty lines have more indentation than the body"
      case UndefinedTagHandle(handle) =>
        m"the tag handle $handle was not defined"
      case UnknownAlias(name) =>
        m"the alias *$name does not refer to a known anchor"

  // Byte constants used by the parser (mirrors `JsonAst.AsciiByte` from
  // Jacinta).
  object Byte:
    inline final val Tab:           9   = 9
    inline final val Newline:       10  = 10
    inline final val Return:        13  = 13
    inline final val Space:         32  = 32
    inline final val Bang:          33  = 33   // '!'
    inline final val Quote:         34  = 34   // '"'
    inline final val Hash:          35  = 35   // '#'
    inline final val Amp:           38  = 38   // '&'
    inline final val Apostrophe:    39  = 39   // '\''
    inline final val Star:          42  = 42   // '*'
    inline final val Plus:          43  = 43   // '+'
    inline final val Comma:         44  = 44
    inline final val Minus:         45  = 45   // '-'
    inline final val Period:        46  = 46
    inline final val Slash:         47  = 47
    inline final val Num0:          48  = 48
    inline final val Num1:          49  = 49
    inline final val Num2:          50  = 50
    inline final val Num3:          51  = 51
    inline final val Num4:          52  = 52
    inline final val Num5:          53  = 53
    inline final val Num6:          54  = 54
    inline final val Num7:          55  = 55
    inline final val Num8:          56  = 56
    inline final val Num9:          57  = 57
    inline final val Colon:         58  = 58
    inline final val Question:      63  = 63   // '?'
    inline final val Percent:       37  = 37   // '%'
    inline final val OpenAngle:     60  = 60   // '<'
    inline final val CloseAngle:    62  = 62   // '>'
    inline final val UpperE:        69  = 69
    inline final val UpperF:        70  = 70
    inline final val UpperX:        88  = 88
    inline final val OpenBracket:   91  = 91
    inline final val Backslash:     92  = 92
    inline final val CloseBracket:  93  = 93
    inline final val Caret:         94  = 94
    inline final val Underscore:    95  = 95
    inline final val LowerA:        97  = 97
    inline final val LowerB:        98  = 98
    inline final val LowerE:        101 = 101
    inline final val LowerF:        102 = 102
    inline final val LowerL:        108 = 108
    inline final val LowerN:        110 = 110
    inline final val LowerO:        111 = 111
    inline final val LowerR:        114 = 114
    inline final val LowerS:        115 = 115
    inline final val LowerT:        116 = 116
    inline final val LowerU:        117 = 117
    inline final val LowerV:        118 = 118
    inline final val LowerX:        120 = 120
    inline final val OpenBrace:     123 = 123
    inline final val Pipe:          124 = 124
    inline final val CloseBrace:    125 = 125
    inline final val Tilde:         126 = 126
    inline final val Greater:       62  = 62

  // Sentinel object used to pad an array whose user-visible length is
  // even so the underlying `IArray[Any]` is always odd-length and can be
  // distinguished from a mapping (always even-length).
  private[ypsiloid] val arrayPad: AnyRef = new Object

  // ── Constructors ────────────────────────────────────────────────────────

  inline def apply
              (value:
                Long | Double | Bcd | Boolean | String | IArray[Any] | Null | Unset.type )
  :   YamlAst =
    value

  val Null: YamlAst = null

  inline def Bool(value: Boolean): YamlAst = value
  inline def Integer(value: Long): YamlAst = value
  inline def Decimal(value: Double): YamlAst = value
  inline def BcdValue(value: Bcd): YamlAst = value
  inline def Str(value: Text): YamlAst = value.s

  // Wrap an `IArray[YamlAst]` of items as a sequence node. If the count
  // is even, append the `arrayPad` sentinel so the final node has odd
  // length.
  def Sequence(items: IArray[YamlAst]): YamlAst =
    val n = items.length
    if (n & 1) == 1 then items.asInstanceOf[IArray[Any]]
    else
      val padded = new Array[Any](n + 1)
      System.arraycopy(items.asInstanceOf[Array[Any]], 0, padded, 0, n)
      padded(n) = arrayPad
      padded.asInstanceOf[IArray[Any]]

  // Wrap parallel keys/values as a mapping node, flattened to alternating
  // `[k0, v0, k1, v1, ...]`. The result has even length.
  def Mapping(entries: IArray[(YamlAst, YamlAst)]): YamlAst =
    val n = entries.length
    val arr = new Array[Any](n*2)
    var i = 0
    while i < n do
      val (k, v) = entries(i)
      arr(i*2) = k.asInstanceOf[Any]
      arr(i*2 + 1) = v.asInstanceOf[Any]
      i += 1
    arr.asInstanceOf[IArray[Any]]

  // Build a sequence directly from a raw `Array[Any]` of items (no
  // copy if the length is already odd; pad once otherwise). The parser
  // uses this to avoid the `Array.map` step.
  private[ypsiloid] def seqFromAnyArray(items: Array[Any]): YamlAst =
    val n = items.length
    if (n & 1) == 1 then items.asInstanceOf[IArray[Any]]
    else
      val padded = new Array[Any](n + 1)
      System.arraycopy(items, 0, padded, 0, n)
      padded(n) = arrayPad
      padded.asInstanceOf[IArray[Any]]

  // Build a mapping directly from a flat `Array[Any]` of alternating
  // key/value entries. Length must be even.
  private[ypsiloid] def mapFromAnyArray(entries: Array[Any]): YamlAst =
    entries.asInstanceOf[IArray[Any]]

  // ── Inspection ──────────────────────────────────────────────────────────

  // The user-visible length of a sequence (excludes the pad sentinel).
  def sequenceLength(arr: IArray[Any]): Int =
    val n = arr.length
    if n > 0 && (arr(n - 1).asInstanceOf[AnyRef] eq arrayPad) then n - 1 else n

  // The number of (key, value) pairs in a mapping.
  def mappingSize(arr: IArray[Any]): Int = arr.length / 2

  // Apply `body(item)` to each user-visible item of a sequence, skipping
  // the pad sentinel. Hot-path iterator used by collection decoders.
  inline def foreachItem(arr: IArray[Any])(inline body: YamlAst => Unit): Unit =
    val n = sequenceLength(arr)
    var i = 0
    while i < n do
      body(arr(i).asInstanceOf[YamlAst])
      i += 1

  // Apply `body(key, value)` to each entry of a mapping. Used by Map/case-
  // class decoders.
  inline def foreachEntry(arr: IArray[Any])(inline body: (YamlAst, YamlAst) => Unit): Unit =
    val n = arr.length
    var i = 0
    while i < n do
      body(arr(i).asInstanceOf[YamlAst], arr(i + 1).asInstanceOf[YamlAst])
      i += 2

  // ── Pattern-match extractors ────────────────────────────────────────────
  // These let existing `case YamlAst.Bool(b) => ...` patterns keep working
  // after the case-class hierarchy is removed. Each unapply allocates an
  // `Option`, which is fine for non-hot-path code (tests, decoders).

  object Bool:
    def unapply(ast: YamlAst): Option[Boolean] = ast match
      case b: Boolean => Some(b)
      case _          => None

  object Integer:
    def unapply(ast: YamlAst): Option[Long] = ast match
      case n: Long => Some(n)
      case _       => None

  object Decimal:
    def unapply(ast: YamlAst): Option[Double] = ast match
      case d: Double => Some(d)
      case _         => None

  // Pattern extractor for high-precision BCD numbers. Matches a number
  // value that overflowed `Long`/`Double` precision during parsing.
  object BcdValue:
    def unapply(ast: YamlAst): Option[Bcd] = ast match
      case b: Array[Long] @unchecked => Some(b.asInstanceOf[Bcd])
      case _                         => None

  object Str:
    def unapply(ast: YamlAst): Option[Text] = ast match
      case s: String => Some(s.tt)
      case _         => None

  object Sequence:
    def unapply(ast: YamlAst): Option[IArray[YamlAst]] = ast match
      case xs: IArray[?] @unchecked
        if xs.isInstanceOf[Array[AnyRef]] && ((xs.length & 1) == 1 || xs.length == 1) =>
        // Strip the sentinel if present.
        val n = xs.length
        if n > 0 && (xs(n - 1).asInstanceOf[AnyRef] eq arrayPad) then
          val out = new Array[Any](n - 1)
          System.arraycopy(xs.asInstanceOf[Array[Any]], 0, out, 0, n - 1)
          Some(out.asInstanceOf[IArray[YamlAst]])
        else
          Some(xs.asInstanceOf[IArray[YamlAst]])
      case _ => None

  object Mapping:
    def unapply(ast: YamlAst): Option[IArray[(YamlAst, YamlAst)]] = ast match
      case xs: IArray[?] @unchecked
        if xs.isInstanceOf[Array[AnyRef]] && (xs.length & 1) == 0 =>
        val n = xs.length / 2
        Some(IArray.tabulate(n): i =>
          (xs(i*2).asInstanceOf[YamlAst], xs(i*2 + 1).asInstanceOf[YamlAst]))
      case _ => None

  // ── Deep equality ───────────────────────────────────────────────────────
  // Used by `Yaml.equals`/`hashCode` and by tests that compare two parsed
  // ASTs structurally. Walks `IArray[Any]` recursively so different array
  // instances with the same content compare equal.

  def deepEquals(left: YamlAst, right: YamlAst): Boolean =
    if left.asInstanceOf[AnyRef] eq right.asInstanceOf[AnyRef] then true
    else (left, right) match
      case (a: Long, b: Long)         => a == b
      case (a: Double, b: Double)     => a == b
      case (a: Boolean, b: Boolean)   => a == b
      case (a: String, b: String)     => a == b

      // Cross-shape numeric comparisons: a `Bcd` may be equal in value
      // to a `Long`, `Double`, or another `Bcd`, when their canonical
      // BigDecimal projections compare equal.
      case (a: Array[Long] @unchecked, b: Array[Long] @unchecked) =>
        a.asInstanceOf[Bcd].toBigDecimal == b.asInstanceOf[Bcd].toBigDecimal
      case (a: Array[Long] @unchecked, b: Long) =>
        a.asInstanceOf[Bcd].toBigDecimal == BigDecimal(b)
      case (a: Array[Long] @unchecked, b: Double) =>
        a.asInstanceOf[Bcd].toBigDecimal == BigDecimal(b)
      case (a: Long, b: Array[Long] @unchecked) =>
        BigDecimal(a) == b.asInstanceOf[Bcd].toBigDecimal
      case (a: Double, b: Array[Long] @unchecked) =>
        BigDecimal(a) == b.asInstanceOf[Bcd].toBigDecimal

      case (a: Array[AnyRef] @unchecked, b: Array[AnyRef] @unchecked) =>
        a.length == b.length && {
          var i = 0
          var equal = true
          while i < a.length && equal do
            val ax = a(i).asInstanceOf[AnyRef]
            val bx = b(i).asInstanceOf[AnyRef]
            if (ax eq bx) || (ax eq arrayPad) && (bx eq arrayPad) then ()
            else
              equal = deepEquals(a(i).asInstanceOf[YamlAst], b(i).asInstanceOf[YamlAst])
            i += 1
          equal
        }

      case _ => false

  def deepHash(ast: YamlAst): Int = ast match
    case null         => 0
    case b: Boolean   => b.hashCode
    case n: Long      => n.hashCode
    case d: Double    => d.hashCode
    case s: String    => s.hashCode

    case b: Array[Long] @unchecked =>
      // Hash via the BigDecimal projection so a `Bcd` whose value equals
      // a numeric `Long`/`Double` literal has a consistent hash.
      b.asInstanceOf[Bcd].toBigDecimal.hashCode

    case xs: Array[AnyRef] @unchecked =>
      var h = xs.length
      var i = 0
      while i < xs.length do
        val item = xs(i).asInstanceOf[AnyRef]
        if !(item eq arrayPad) then
          h = h*31 + deepHash(xs(i).asInstanceOf[YamlAst])
        i += 1
      h
    case _: Unset.type => 1
