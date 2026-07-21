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
package jacinta

import scala.compiletime.*
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
import vacuous.*
import wisteria.{Discriminable, VariantError}
import zephyrine.*

object internal:

  // Reuses `JsonPointer`'s own `Decodable` for validation: the literal is
  // decoded at macro-expansion time and, if it fails, the `JsonPointerError`'s
  // offset is mapped back to a source position so the error points exactly at
  // the offending character.
  def jsonPointer[parts <: Tuple: Type, origins <: Tuple: Type](insertions: Expr[Seq[Any]])
  :   Macro[JsonPointer] =

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
    if parts.length != 1 then halt(m"a JSON pointer literal cannot have substitutions")
    val raw: String = parts.head
    val start: Int = firstOrigin[origins]

    try unsafely(raw.tt.as[JsonPointer]) catch
      case error: JsonPointerError =>
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

    '{unsafely(${Expr(raw)}.tt.as[JsonPointer])}

  // Compile-time navigation for schema-typed `Json` values. A `Json of P from R`
  // carries a phantom *position* (`Topic = P`, a Scala model type) within a *root
  // schema* (`Origin = R`). The `Dynamic` methods on `Json` are `transparent
  // inline` and delegate here: when the receiver's position is bound and the field
  // name is a literal, the macro looks the field up in `P`'s structure and yields a
  // `Json of <field-type> from R`; otherwise it falls back to the plain
  // (`DynamicJsonEnabler`-gated) runtime access, exactly as before.

  // Every `type X = …` member of a (possibly nested) refinement, by name.
  private def refinements(using quotes: Quotes)(repr: quotes.reflect.TypeRepr)
  :   Map[Text, quotes.reflect.TypeRepr] =

    import quotes.reflect.*

    repr.dealias match
      case Refinement(parent, name, TypeBounds(_, hi)) => refinements(parent).updated(name.tt, hi)
      case Refinement(parent, name, info)              => refinements(parent).updated(name.tt, info)
      case AndType(left, right)                        => refinements(left) ++ refinements(right)
      case _                                           => Map()

  // Builds the refined type `Json of <position> from <root>`.
  private def jsonType(using quotes: Quotes)
    ( position: quotes.reflect.TypeRepr, root: quotes.reflect.TypeRepr )
  :   quotes.reflect.TypeRepr =

    import quotes.reflect.*

    Refinement
      ( Refinement(TypeRepr.of[Json], "Topic", TypeBounds(position, position)),
        "Origin",
        TypeBounds(root, root) )

  // The single ordered-collection element type of `repr`, if it is one (`List`,
  // `Vector`, `Seq`, `LazyList`, `Array`, `IArray`); `Set` is excluded as it has
  // no positional index.
  private def elementType(using quotes: Quotes)(repr: quotes.reflect.TypeRepr)
  :   Optional[quotes.reflect.TypeRepr] =

    import quotes.reflect.*

    repr.dealias match
      case AppliedType(constructor, List(element))
      if repr <:< TypeRepr.of[Seq[Any]] || constructor.typeSymbol == defn.ArrayClass =>
        element

      case _ =>
        Unset

  // Reads `Topic` (position) and `Origin` (root) from a receiver, if present.
  private def receiver(using quotes: Quotes)(self: Expr[Json])
  :   Optional[(quotes.reflect.TypeRepr, quotes.reflect.TypeRepr)] =

    import quotes.reflect.*
    val members = refinements(self.asTerm.tpe.widen)

    members.at(t"Topic").let: position =>
      (position, members.at(t"Origin").or(position))

  def select(self: Expr[Json], field: Expr[String]): Macro[Json] =

    // Plain (unverified) access: gate on the enabler (resolved at the call site),
    // then read the field totally.
    def plain: Expr[Json] =
      if Expr.summon[DynamicJsonEnabler].nil then halt:
        m"""
          dynamic field access on an unverified `Json` requires `import dynamicJsonAccess.enabled`
          (or verify the value against a schema first)
        """

      '{$self.selectField($field)}

    receiver(self) match
      case (position0, root0) =>
        val position = position0.asInstanceOf[quotes.reflect.TypeRepr]
        val root = root0.asInstanceOf[quotes.reflect.TypeRepr]

        field.value match
          case Some(name) => position.typeSymbol.caseFields.find(_.name == name) match
            case Some(member) =>
              jsonType(position.memberType(member), root).asType.absolve match
                case '[type result <: Json; result] =>
                  '{$self.selectField(${Expr(name)}).asInstanceOf[result]}

            case None =>
              halt(m"the schema position ${position.show} has no field $name")

          case None =>
            plain

      case _ =>
        plain

  def index(self: Expr[Json], idx: Expr[Int]): Macro[Json] =
    import quotes.reflect.*

    receiver(self) match
      case (position0, root0) =>
        val position = position0.asInstanceOf[TypeRepr]
        val root = root0.asInstanceOf[TypeRepr]
        val element = elementType(position)

        if element.absent
        then halt(m"the schema position ${position.show} is not an indexable array type")

        jsonType(element.vouch, root).asType.absolve match
          case '[type result <: Json; result] =>
            '{$self.selectIndex($idx).asInstanceOf[result]}

      case _ => Expr.summon[Tactic[JsonError]] match
        case Some(tactic) =>
          '{$self.indexValue($idx)(using $tactic)}

        case None =>
          halt:
            m"""
              indexing a `Json` array may raise `JsonError`; a `Tactic[JsonError]`
              must be in scope (e.g. via `raises JsonError`)
            """

  def applied(self: Expr[Json], field: Expr[String], idx: Expr[Int]): Macro[Json] =
    import quotes.reflect.*

    def plain: Expr[Json] =
      if Expr.summon[DynamicJsonEnabler].nil then halt:
        m"""
          dynamic field access on an unverified `Json` requires `import dynamicJsonAccess.enabled`
          (or verify the value against a schema first)
        """

      Expr.summon[Tactic[JsonError]] match
        case Some(tactic) => '{$self.selectField($field).indexValue($idx)(using $tactic)}

        case None =>
          halt:
            m"""
              indexing a `Json` array may raise `JsonError`; a `Tactic[JsonError]` must be in scope
              (e.g. via `raises JsonError`)
            """

    receiver(self) match
      case (position0, root0) =>
        val position = position0.asInstanceOf[TypeRepr]
        val root = root0.asInstanceOf[TypeRepr]

        field.value match
          case Some(name) => position.typeSymbol.caseFields.find(_.name == name) match
            case Some(member) =>
              val element = elementType(position.memberType(member))

              if element.absent
              then halt(m"the field $name of ${position.show} is not an indexable array")

              jsonType(element.vouch, root).asType.absolve match
                case '[type result <: Json; result] =>
                  '{$self.selectField(${Expr(name)}).selectIndex($idx).asInstanceOf[result]}

            case None =>
              halt(m"the schema position ${position.show} has no field $name")

          case None =>
            plain

      case _ =>
        plain

  opaque type Bcd = Array[Double]

  object Bcd:
    // Header masks for the raw-bits layout. Data words use all 64 bits.
    private inline val SignBit      = 0x8000_0000_0000_0000L  // bit 63
    private inline val MantissaMask = 0x000F_FFFF_FFFF_FFFFL  // bits 0–51
    inline val NibblesPerDouble = 16

    private inline def toRawBits(d: Double): Long =
      java.lang.Double.doubleToRawLongBits(d)

    private inline def fromRawBits(l: Long): Double =
      java.lang.Double.longBitsToDouble(l)

    // Encode 16 nibbles (already packed in `nibbles`) as a storage word.
    // BCD nibbles ≤ 0xC guarantee the bit pattern is never NaN, so the
    // raw 64-bit value is stored verbatim.
    private inline def packDataDouble(nibbles: Long): Double = fromRawBits(nibbles)

    // Encode the Bcd header. Sign goes to bit 63 (the double's own sign
    // bit); count lands in the mantissa (low 52 bits, plenty).
    private inline def packHeaderDouble(negative: Boolean, count: Int): Double =
      val sign = if negative then SignBit else 0L
      fromRawBits(sign | (count.toLong & MantissaMask))

    // Internal: wrap a freshly-built header+data array as a `Bcd`.
    private[jacinta] inline def wrap(arr: Array[Double]): Bcd = arr

    // Single-Long BCD encoding for arrays of numbers — see `Array[Long]` as
    // a `Json.Ast` array variant. One number per Long:
    //   bit 63        : sign (0 = non-negative, 1 = negative)
    //   bits 56–62    : nibble count (7 bits; max 14)
    //   bits 0–55     : up to 14 nibbles, oldest at the highest used position
    //                   (bit 52–55 when count = 14), newest at bits 0–3 —
    //                   matching the parser's `(content << 4) | n` order.
    // A value of `0L` is reserved for "uninitialised slot" since a valid
    // BCD-Long always has count >= 1 (the digit `0` is encoded as count = 1,
    // sign = 0, nibble 0x0 → header byte 0x01).
    inline val MaxBcdLongNibbles = 14
    private inline val BcdLongSignBit    = 0x8000_0000_0000_0000L
    private inline val BcdLongCountMask  = 0x7F00_0000_0000_0000L   // bits 56–62
    private inline val BcdLongNibbleMask = 0x00FF_FFFF_FFFF_FFFFL   // bits 0–55

    // Pack the parser's in-Long fast-path accumulator (`content` with
    // `nibbles` digits, plus sign) into the single-Long BCD format. The
    // caller is responsible for checking `nibbles <= MaxBcdLongNibbles`.
    inline def packBcdLong(content: Long, nibbles: Int, negative: Boolean): Long =
      val sign  = if negative then BcdLongSignBit else 0L
      val count = (nibbles.toLong & 0x7FL) << 56
      sign | count | (content & BcdLongNibbleMask)

    // Decode a single-Long BCD value's nibble count (1..14 for valid values).
    inline def bcdLongNibbleCount(value: Long): Int =
      ((value >>> 56) & 0x7FL).toInt

    // Decode a single-Long BCD value's sign.
    inline def bcdLongNegative(value: Long): Boolean = value < 0L

    // Decode a single-Long BCD value to its canonical JSON-number text. The
    // walk mirrors `Bcd.each` but operates on a single Long. The parser
    // omits the redundant leading "0" for "0.xxx" inputs, so this re-
    // inserts a "0" when the oldest nibble is "." (`0xA`) to keep the
    // output a valid JSON number.
    def bcdLongText(value: Long): String =
      val count = bcdLongNibbleCount(value)

      if count == 0 then "0"
      else
        val sb = new java.lang.StringBuilder(count + 2)
        if bcdLongNegative(value) then sb.append('-')

        val firstNibble = ((value >>> ((count - 1)*4)) & 0xFL).toInt
        if firstNibble == 0xA then sb.append('0')

        var j = count - 1

        while j >= 0 do
          val n = ((value >>> (j*4)) & 0xFL).toInt

          if      n <= 9     then sb.append(('0' + n).toChar)
          else if n == 0xA   then sb.append('.')
          else if n == 0xB   then sb.append('e')
          else if n == 0xC   then sb.append("e-")

          j -= 1

        sb.toString

    // Convert a single-Long BCD value to `Double` via its text form.
    inline def bcdLongToDouble(value: Long): Double =
      java.lang.Double.parseDouble(bcdLongText(value))

    // Single-Int BCD encoding for small numbers — a JSON number with up to
    // 7 nibbles fits in a single `Int`, with the same per-Long bit-layout
    // pattern but compressed:
    //   bit 31        : sign (0 = non-negative, 1 = negative)
    //   bits 28–30    : nibble count (3 bits; max 7)
    //   bits 0–27     : up to 7 nibbles, oldest at the highest used position
    //                   (bit 24–27 when count = 7), newest at bits 0–3.
    // Stored as the `Int` variant of `JsonNumber` for numbers that fit; the
    // boxed `Integer` runtime class is distinct from `Long` (`Long`) so the
    // pattern match in `Json.Ast` extensions can distinguish them.
    inline val MaxBcdIntNibbles = 7
    private inline val BcdIntSignBit    = 0x8000_0000
    private inline val BcdIntCountMask  = 0x7000_0000   // bits 28–30
    private inline val BcdIntNibbleMask = 0x0FFF_FFFF   // bits 0–27

    // Pack the parser's in-Long fast-path accumulator into the single-Int
    // BCD format. The caller is responsible for `nibbles <= MaxBcdIntNibbles`.
    inline def packBcdInt(content: Long, nibbles: Int, negative: Boolean): Int =
      val sign  = if negative then BcdIntSignBit else 0
      val count = (nibbles & 0x7) << 28
      sign | count | (content.toInt & BcdIntNibbleMask)

    inline def bcdIntNibbleCount(value: Int): Int = (value >>> 28) & 0x7
    inline def bcdIntNegative(value: Int): Boolean = value < 0

    def bcdIntText(value: Int): String =
      val count = bcdIntNibbleCount(value)

      if count == 0 then "0"
      else
        val sb = new java.lang.StringBuilder(count + 2)
        if bcdIntNegative(value) then sb.append('-')

        val firstNibble = (value >>> ((count - 1)*4)) & 0xF
        if firstNibble == 0xA then sb.append('0')

        var j = count - 1

        while j >= 0 do
          val n = (value >>> (j*4)) & 0xF

          if      n <= 9     then sb.append(('0' + n).toChar)
          else if n == 0xA   then sb.append('.')
          else if n == 0xB   then sb.append('e')
          else if n == 0xC   then sb.append("e-")

          j -= 1

        sb.toString

    inline def bcdIntToDouble(value: Int): Double =
      java.lang.Double.parseDouble(bcdIntText(value))

    // Re-encode a single-Int small-BCD value into the single-Long BCD form.
    // Used by the array parser's `bcdInt` → `bcdLong` migration step.
    inline def repackBcdIntAsLong(value: Int): Long =
      val sign     = if value < 0 then BcdLongSignBit else 0L
      val count    = (((value >>> 28) & 0x7).toLong) << 56
      val nibbles  = (value & 0x0FFF_FFFF).toLong
      sign | count | nibbles

    // Re-encode a single-Long BCD value as a single-Int small BCD. The
    // caller is responsible for ensuring the value has ≤ 7 nibbles (so the
    // nibble bits fit in the low 28 bits of the Int). Cheap bit-shuffle —
    // sign moves from bit 63 to bit 31; count moves from bits 56–62 to
    // bits 28–30; nibbles in bits 0–27 are unchanged.
    inline def packBcdIntFromLong(value: Long): Int =
      val sign     = if (value & BcdLongSignBit) != 0L then BcdIntSignBit else 0
      val count    = (((value >>> 56) & 0x7L).toInt) << 28
      val nibbles  = (value & 0x0FFF_FFFFL).toInt
      sign | count | nibbles

    // Build a `Bcd` directly from the parser's in-Long fast-path
    // accumulator when it filled to capacity (15 nibbles) and the caller
    // wants a `Bcd` instead of a single-Long packing. Skips the
    // `Bcd.Builder` allocation and the `seedFromLong` re-walk; the layout
    // is fixed (1 header + 1 data word).
    private[jacinta] def fromContent15(content: Long, negative: Boolean): Bcd =
      val arr = new Array[Double](2)
      arr(0) = packHeaderDouble(negative, 15)
      arr(1) = packDataDouble(content)
      arr

    // Build a `Bcd` from a `BigDecimal`. Goes via `toPlainString` so the result
    // matches the in-AST representation a parser would produce for the same
    // textual JSON number.
    def apply(value: BigDecimal): Bcd =
      val s: String = value.bigDecimal.toPlainString.nn
      val negative = s.startsWith("-")
      fromString(if negative then s.substring(1).nn else s, negative)

    // Public construction from a positive-magnitude JSON-number string. The
    // caller is responsible for stripping any leading `-`. The string may
    // contain digits, a single `.`, and an optional `e[+-]?\d+` suffix.
    // Skips a leading `0` if it's followed by `.` — matches the parser's
    // convention (the printer reinserts the `0` on emit).
    def fromString(text: String, negative: Boolean): Bcd =
      val builder = new Builder
      var i = if text.length >= 2 && text.charAt(0) == '0' && text.charAt(1) == '.' then 1 else 0
      val n = text.length
      var prevWasE = false

      while i < n do
        val char = text.charAt(i)

        (char: @scala.annotation.switch) match
          case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
            builder.add(char - '0')
            prevWasE = false

          case '.' =>
            builder.add(0xA)
            prevWasE = false

          case 'e' | 'E' =>
            builder.add(0xB)
            prevWasE = true

          case '+' =>
            prevWasE = false

          case '-' =>
            if prevWasE then builder.overwriteLast(0xC)
            prevWasE = false

          case _ => ()

        i += 1

      builder.finish(negative)

    // Incremental builder used by `Json.Parser` to assemble a `Bcd` one nibble
    // at a time as it overflows the in-Long fast path. Keeps a growing
    // `Array[Double]` of completed words and a current 52-bit nibble buffer.
    final class Builder extends caps.Mutable:
      private var data: Array[Double]^ = new Array[Double](2)
      private var wordIdx: Int = 0
      private var word: Long = 0L   // raw nibble buffer; packed into a Double on commit
      private var inWord: Int = 0
      private var nibbles: Int = 0

      // Append one nibble (0x0–0xC) to the in-progress word. When the word
      // fills (13 nibbles), it is committed to the `data` array.
      update def add(nibble: Int): Unit =
        word = (word << 4) | (nibble & 0xFL)
        inWord += 1
        nibbles += 1

        if inWord == NibblesPerDouble then
          ensureCapacity(wordIdx + 1)
          data(wordIdx) = packDataDouble(word)
          wordIdx += 1
          word = 0L
          inWord = 0

      // Replace the most recently-added nibble. Used by the parser to rewrite
      // an emitted `e` (0xB) as `e-` (0xC) when a `-` follows.
      update def overwriteLast(nibble: Int): Unit =
        val mask = 0xFL
        val v = nibble & mask

        if inWord > 0 then word = (word & ~mask) | v
        else if wordIdx > 0 then
          val prev = toRawBits(data(wordIdx - 1))
          data(wordIdx - 1) = packDataDouble((prev & ~mask) | v)

      // Snapshot the in-Long fast-path accumulator (15 nibbles) into the
      // builder, so the parser can hand off mid-number without losing state.
      update def seedFromLong(content: Long, count: Int): Unit =
        var i = count - 1

        while i >= 0 do
          add(((content >>> (i * 4)) & 0xFL).toInt)
          i -= 1

      def nibbleCount: Int = nibbles

      // Produce the final `Bcd`. The trailing partial word (if any) is
      // committed right-justified in its data slot.
      def finish(negative: Boolean): Bcd =
        val totalDataDoubles = if inWord > 0 then wordIdx + 1 else wordIdx
        val arr = new Array[Double](1 + totalDataDoubles)
        arr(0) = packHeaderDouble(negative, nibbles)
        System.arraycopy(data, 0, arr, 1, wordIdx)
        if inWord > 0 then arr(1 + wordIdx) = packDataDouble(word)
        arr

      private update def ensureCapacity(needed: Int): Unit =
        if needed > data.length then
          val newSize = (data.length * 2).max(needed)
          val newData = new Array[Double](newSize)
          System.arraycopy(data, 0, newData, 0, wordIdx)
          data = newData

    extension (bcd: Bcd)
      def negative:    Boolean = (toRawBits(bcd(0)) & SignBit) != 0L
      def nibbleCount: Int     = (toRawBits(bcd(0)) & MantissaMask).toInt

      // Iterate nibbles in left-to-right (oldest-first) order, invoking the
      // action for each. Used by the printer to emit a JSON-number string and
      // by conversion routines (`toBigDecimal`, `toDouble`, `toLong`).
      inline def each(inline action: Int => Unit): Unit =
        val total = bcd.nibbleCount
        val fullDoubles = total/NibblesPerDouble
        val partial = total - fullDoubles*NibblesPerDouble
        var i = 0

        while i < fullDoubles do
          val w = toRawBits(bcd(1 + i))
          var j = NibblesPerDouble - 1

          while j >= 0 do
            action(((w >>> (j*4)) & 0xFL).toInt)
            j -= 1

          i += 1

        if partial > 0 then
          val w = toRawBits(bcd(1 + fullDoubles))
          var j = partial - 1

          while j >= 0 do
            action(((w >>> (j*4)) & 0xFL).toInt)
            j -= 1

      // Render as a JSON number string (canonical form: digits, optional `.`
      // and fraction, optional `e[-]?\d+`). Re-inserts a leading "0" when
      // the oldest nibble is "." — the parser and `fromString` omit it for
      // "0.xxx" inputs, but JSON requires it.
      def text: String =
        if bcd.nibbleCount == 0 then "0"
        else
          val sb = new java.lang.StringBuilder(bcd.nibbleCount + 2)
          if bcd.negative then sb.append('-')
          var first = true

          bcd.each: nibble =>
            if first then
              first = false
              if nibble == 0xA then sb.append('0')

            if nibble <= 9 then sb.append(('0' + nibble).toChar)
            else if nibble == 0xA then sb.append('.')
            else if nibble == 0xB then sb.append('e')
            else if nibble == 0xC then sb.append("e-")

          sb.toString

      def toBigDecimal: BigDecimal = BigDecimal(bcd.text)
      def toDouble:     Double     = java.lang.Double.parseDouble(bcd.text)

      // Returns the value as a `Long` if it represents an exact integer that
      // fits in `Long`'s range; `Unset` otherwise. Lightweight: a non-fitting
      // value is detected by `Long.parseLong`'s `NumberFormatException`.
      def toLong: Optional[Long] =
        val s = bcd.text
        try java.lang.Long.parseLong(s) catch case _: NumberFormatException => Unset

  private final val Marker: Char = ' '
  private final val MarkerString: String = " "

  // Strip the trailing sentinel pad (if present) from a parity-padded
  // heterogeneous array literal read at compile time, returning just the
  // user-visible elements.
  private def arrayElements(arr: IArray[Any]): IArray[Any] =
    val n = arr.length

    if n > 0 && (arr(n - 1).asInstanceOf[AnyRef] eq Json.Ast.arrayPad)
    then arr.take(n - 1)
    else arr

  private def hasMarker(s: String): Boolean =
    var i = 0

    while i < s.length do
      if s.charAt(i) == Marker then return true
      i += 1

    false

  private def preprocess(parts: List[String]): (List[String], Set[Int]) =
    var spreads: Set[Int] = Set()

    val cleaned: List[String] = parts.zipWithIndex.map: (part, idx) =>
      if idx > 0 && part.startsWith("*") then
        spreads = spreads + (idx - 1)
        part.substring(1).nn
      else
        part

    (cleaned, spreads)


  import Bcd.*

  def interpolator[parts <: Tuple: Type, origins <: Tuple: Type]
    ( insertions0: Expr[Seq[Any]] )
  :   Macro[Json] =

    import quotes.reflect.*

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

    val insertions: Seq[Expr[Any]] = insertions0.absolve match
      case Varargs(insertions) => insertions

    val (parts2, spreads) = preprocess(parts)
    val source: String = parts2.mkString(MarkerString)
    val data: IArray[Byte] = IArray.from(source.getBytes("UTF-8").nn.iterator).asInstanceOf[IArray[Byte]]

    // Map a parser char-offset (within the joined input) back to a source-file
    // Position. The cleaned parts (parts2) are what the parser sees; the
    // original parts may carry a leading `*` for spreads which lives in the
    // source but not in the parser input — `srcSkip` accounts for that. The
    // mapping handles Scala backslash escapes and `$$` in each part. We don't
    // trust the compiler-supplied lit.pos.end (it's unreliable for parts
    // containing $$); the walker discovers the true source-end itself.
    val sourceFile = Position.ofMacroExpansion.sourceFile
    val macroPos = Position.ofMacroExpansion

    val sourceContent: Optional[String] = sourceFile.content match
      case Some(s: String) => s
      case _               => Unset

    val perPart: IndexedSeq[((String, Int, Int), Int -> Int)] =
      parts.zip(parts2).zip(partOrigins).map: pair =>
        val ((origPart, parserPart), (srcStart, _)) = pair
        val srcSkip = origPart.length - parserPart.length // 1 for `*`-prefixed spread
        val effectiveStart = srcStart + srcSkip

        val mapping: Int -> Int = sourceContent.lay[Int -> Int](identity): content =>
          if effectiveStart > 0 && effectiveStart < content.length then
            val upper = (effectiveStart + parserPart.length * 6 + 16).min(content.length)
            val sourceText = content.substring(effectiveStart, upper).nn
            Interpolation.buildMapping(sourceText, parserPart)
          else
            (i: Int) => i

        ((parserPart, effectiveStart, srcSkip), mapping)

      . toIndexedSeq

    def translateOffset(parserOff: Int, len: Int): Position =
      var acc = 0
      var i = 0

      while i < perPart.length do
        val ((parserPart, effectiveStart, _), mapping) = perPart(i)
        val partLen = parserPart.length

        if parserOff < acc + partLen && effectiveStart > 0 then
          val inPart = parserOff - acc
          val endIn = (inPart + len.max(1)).min(parserPart.length)
          val rawStart = (effectiveStart + mapping(inPart)).max(effectiveStart)
          val rawEnd = (effectiveStart + mapping(endIn)).max(rawStart + 1)
          return Position(sourceFile, rawStart, rawEnd)

        acc += partLen + 1
        i += 1

      macroPos

    // Custom HaltTactic: translate parser ParseError positions to source-file ranges.
    val ast: Json.Ast =
      given diagnostics: Diagnostics = Diagnostics.omit

      given parseTactic: HaltTactic[ParseError, Json.Ast] =
        new HaltTactic[ParseError, Json.Ast]:
          override def abort(error: Diagnostics ?=> ParseError): Nothing =
            val pe = error
            val off = pe.position.offset.or(0)
            val length = pe.position.length.or(0)
            halt(pe.labelled, translateOffset(off, length))

      Json.Ast.parse(data, true)

    abortive:

      var holeIndex: Int = 0

      def consumeHole(): Expr[Any] =
        val expr = insertions(holeIndex)
        holeIndex += 1
        expr

      def encodeValue(expr: Expr[Any]): Expr[Json.Ast] = expr.absolve match
        case '{$value: tpe} =>
          Expr.summon[(? >: tpe) is Encodable in Json] match
            case Some('{$enc: Encodable}) =>
              '{$enc.encode($value).root}

            case _ =>
              halt
                ( m"a value of ${TypeRepr.of[tpe].show} is not Encodable in Json",
                  expr.asTerm.underlyingArgument.pos )

      def encodeText(expr: Expr[Any]): Expr[String] = expr.absolve match
        case '{$value: tpe} =>
          Expr.summon[(? >: tpe) is Encodable in Text] match
            case Some('{$enc: Encodable}) =>
              '{$enc.encode($value).s}

            case _ =>
              halt
                ( m"a value of ${TypeRepr.of[tpe].show} is not Encodable in Text",
                  expr.asTerm.underlyingArgument.pos )

      def encodeArraySpread(expr: Expr[Any]): Expr[Iterable[Json.Ast]] = expr.absolve match
        case '{$value: tpe} => Type.of[tpe] match
          case '[Iterable[t]] =>
            Expr.summon[(? >: t) is Encodable in Json] match
              case Some('{$enc: Encodable}) =>
                ' {
                    $value.asInstanceOf[Iterable[t]].iterator
                    . map($enc.encode(_).root)
                    . to(Iterable)
                  }

              case _ =>
                halt
                  ( m"the elements of ${TypeRepr.of[tpe].show} are not Encodable in Json",
                    expr.asTerm.underlyingArgument.pos )

          case _ =>
            halt
              ( m"a `*`-spread requires an Iterable, but got ${TypeRepr.of[tpe].show}",
                expr.asTerm.underlyingArgument.pos )

      def encodeObjectRest(expr: Expr[Any]): Expr[Iterable[(String, Json.Ast)]] =
        expr.absolve match
          case '{$value: tpe} => Type.of[tpe] match
            case '[Map[Text, Json]] =>
              ' {
                  $value.asInstanceOf[Map[Text, Json]].iterator.map: (key, json) =>
                    (key.s, json.root)

                  . toList
                }

            case _ =>
              halt
                ( m"""
                    an object rest hole requires a Map[Text, Json], but got
                    ${TypeRepr.of[tpe].show}
                  """,
                  expr.asTerm.underlyingArgument.pos )

      def serializeString(s: String): Expr[Json.Ast] =
        if !hasMarker(s) then '{Json.Ast(${Expr(s)})}
        else
          val parts: Array[String | Null] = s.split(MarkerString, -1).nn
          var resultExpr: Expr[String] = Expr(parts(0).nn)
          var i = 1

          while i < parts.length do
            val fragment = encodeText(consumeHole())
            val partExpr = Expr(parts(i).nn)
            resultExpr = '{$resultExpr + $fragment + $partExpr}
            i += 1

          '{Json.Ast($resultExpr)}

      def serializeArray(elements: IArray[Any]): Expr[Json.Ast] =
        val n = elements.length

        val indexed = elements.zipWithIndex

        val pieces: List[Expr[Iterable[Json.Ast]]] = indexed.toList.map: (elem, idx) =>
            elem.asMatchable match
              case Unset =>
                if spreads.contains(holeIndex) then
                  if idx != n - 1 then halt:
                    m"a `*`-spread is only allowed as the last element of an array"

                  encodeArraySpread(consumeHole())
                else
                  val v = encodeValue(consumeHole())
                  '{Iterable($v)}

              case other =>
                val v = serialize(other)
                '{Iterable($v)}

        ' {
            val all = ${Expr.ofList(pieces)}.foldLeft(List.empty[Json.Ast])(_ ++ _)
            Json.Ast.arr(IArray.from(all).asInstanceOf[IArray[Any]])
          }

      def serializeObject(node: IArray[Any]): Expr[Json.Ast] =
        val n = node.length/2

        val pieces: List[Expr[Iterable[(String, Json.Ast)]]] =
          (0 until n).toList.map: i =>
            val k = node(i*2).asInstanceOf[String]
            val v = node(i*2 + 1)

            if k == MarkerString then
              v.asMatchable match
                case Unset => encodeObjectRest(consumeHole())

                case _ => halt:
                  m"unexpected non-rest hole in object key position"
            else
              v.asMatchable match
                case Unset =>
                  val expr = encodeValue(consumeHole())
                  '{Iterable((${Expr(k)}, $expr))}

                case other =>
                  val expr = serialize(other)
                  '{Iterable((${Expr(k)}, $expr))}

        ' {
            val all =
              ${Expr.ofList(pieces)}.foldLeft(List.empty[(String, Json.Ast)])(_ ++ _)

            val keysArr: IArray[String] = IArray.from(all.map(_(0)))
            val valuesArr: IArray[Json.Ast] = IArray.from(all.map(_(1)))
            Json.Ast.obj(keysArr, valuesArr.asInstanceOf[IArray[Any]])
          }

      def serialize(node: Any): Expr[Json.Ast] = node.asMatchable match
        case Unset =>
          if spreads.contains(holeIndex) then halt:
            m"a `*`-spread is only allowed as the last element of an array"

          encodeValue(consumeHole())

        case s: String =>
          serializeString(s)

        case b: Boolean =>
          '{Json.Ast(${Expr(b)})}

        case l: Long =>
          '{Json.Ast(${Expr(l)})}

        case i: Int =>
          // Small-BCD value parsed at compile time. Surface it as the
          // packed `Int` directly — the runtime `JsonNumber` variant.
          '{Json.Ast(${Expr(i)})}

        case d: Double =>
          '{Json.Ast(${Expr(d)})}

        case bcd: Array[Double] @unchecked =>
          // High-precision BCD value parsed at compile time. Reconstruct it
          // at runtime from its canonical text form so the literal stays
          // independent of the parser's internal nibble layout.
          val b = bcd.asInstanceOf[Bcd]
          val full = b.text
          val negative = b.negative

          val unsigned: String =
            if negative then full.substring(1).nn else full

          '{Json.Ast(Bcd.fromString(${Expr(unsigned)}, ${Expr(negative)}))}

        case null =>
          '{Json.Ast(Json.JsonNull)}

        case bcds: Array[Long] @unchecked =>
          // Number-only array literal (BCD-Long packed).
          val seq: Expr[Seq[Long]] = Expr(bcds.toSeq)
          '{Json.Ast.bcdArr($seq.toArray)}

        case smalls: Array[Int] @unchecked =>
          // Number-only array literal (small-BCD packed).
          val seq: Expr[Seq[Int]] = Expr(smalls.toSeq)
          '{Json.Ast.smallBcdArr($seq.toArray)}

        case arr: IArray[Any] @unchecked =>
          // Heterogeneous array or object, distinguished by parity.
          if (arr.length & 1) == 0 then serializeObject(arr)
          else serializeArray(arrayElements(arr))

        case other =>
          halt(m"unexpected JSON AST node ${other.toString.tt}")

      val ofAst: Expr[Json.Ast] = serialize(ast)

      '{Json.ast($ofAst)}


  def extractor[parts <: Tuple: Type, origins <: Tuple: Type]
    ( scrutinee: Expr[Json] )
  :   Macro[Extrapolation[Json]] =

    import quotes.reflect.*

    def recur[tuple: Type](strings: List[String]): List[String] = Type.of[tuple] match
      case '[head *: tail] => recur[tail](TypeRepr.of[head].literal[String].vouch :: strings)
      case _               => strings

    val parts = recur[parts](Nil)

    abortive:
      val (parts2, spreads) = preprocess(parts)
      val source: String = parts2.mkString(MarkerString)
      val data: IArray[Byte] = IArray.from(source.getBytes("UTF-8").nn.iterator).asInstanceOf[IArray[Byte]]
      val ast: Json.Ast = Json.Ast.parse(data, true)

      var nextHole: Int = 0
      var types: List[TypeRepr] = Nil

      def descend
        ( array: Expr[Array[Any]],
         pattern: Any,
         scrutinee: Expr[Json.Ast],
         accept: Expr[Boolean] )
      :   Expr[Boolean] =

        pattern.asMatchable match
          case Unset =>
            // Value-position hole: capture scrutinee as Json
            val idx = nextHole

            if spreads.contains(idx) then halt:
              m"a `*`-spread is only allowed as the last element of an array"

            nextHole += 1
            types ::= TypeRepr.of[Json]
            '{$accept && {$array(${Expr(idx)}) = Json.ast($scrutinee); true}}

          case s: String if hasMarker(s) =>
            // String-interior holes are not supported in extractors
            halt:
              m"""
                holes inside string literals are not supported in extractors; capture
                the entire string as a single hole instead
              """

          case s: String =>
            ' {
                $accept && $scrutinee.isString &&
                  $scrutinee.asInstanceOf[String] == ${Expr(s)}
              }

          case b: Boolean =>
            ' {
                $accept && $scrutinee.isBoolean &&
                  $scrutinee.asInstanceOf[Boolean] == ${Expr(b)}
              }

          case l: Long =>
            ' {
                $accept && $scrutinee.isLong &&
                  $scrutinee.asInstanceOf[Long] == ${Expr(l)}
              }

          case d: Double =>
            ' {
                $accept && $scrutinee.isDouble &&
                  $scrutinee.asInstanceOf[Double] == ${Expr(d)}
              }

          case bcd: Array[Long] @unchecked =>
            // High-precision BCD literal in the extractor pattern. We
            // compare the scrutinee's BCD against the same literal text
            // form, going via `BigDecimal` so a parsed `Bcd("1.0")` matches
            // a literal `1` in the pattern (and vice versa).
            val s = bcd.asInstanceOf[Bcd].text

            ' {
                $accept && $scrutinee.isBcd &&
                  $scrutinee.asInstanceOf[Bcd].toBigDecimal == BigDecimal(${Expr(s)})
              }

          case null =>
            '{$accept && $scrutinee.isNull}

          case nums: Array[Double] @unchecked =>
            // Number-only array literal in the pattern: descend by treating
            // each Double as an element. Synthesise an `IArray[Any]` of
            // unpacked element literals (Long for whole-valued, Double
            // otherwise) so the existing `descendArray` element comparison
            // reuses the numeric-equality cases above.
            val n = nums.length

            val elements0 = IArray.tabulate(n): i =>
              val d = nums(i)

              if d.isWhole && d >= Long.MinValue.toDouble && d <= Long.MaxValue.toDouble
              then d.toLong
              else d

            val elements: IArray[Any] = elements0

            descendArray(array, elements, scrutinee, accept)

          case arr: IArray[Any] @unchecked =>
            // Heterogeneous array or object, distinguished by parity.
            if (arr.length & 1) == 0 then descendObject(array, arr, scrutinee, accept)
            else descendArray(array, arrayElements(arr), scrutinee, accept)

          case other =>
            halt(m"unexpected JSON AST node ${other.toString.tt}")

      def descendArray
        ( array: Expr[Array[Any]],
         elements: IArray[Any],
         scrutinee: Expr[Json.Ast],
         accept: Expr[Boolean] )
      :   Expr[Boolean] =

        val n = elements.length
        // Determine whether the last element is a spread.

        val tailSpread: Boolean =
          n > 0 && (elements(n - 1).asMatchable match
            case Unset => spreads.contains(nextHole + countHolesInPrefix(elements, n - 1))
            case _     => false)

        val prefixLen = if tailSpread then n - 1 else n

        // Length check uses `arrayLength`, which strips sentinel padding.
        val lengthCheck: Expr[Boolean] =
          if tailSpread then
            '{$accept && $scrutinee.isArray && $scrutinee.arrayLength >= ${Expr(prefixLen)}}
          else
            '{$accept && $scrutinee.isArray && $scrutinee.arrayLength == ${Expr(prefixLen)}}

        // Recurse into prefix elements
        var combined: Expr[Boolean] = lengthCheck
        var i = 0

        while i < prefixLen do
          val el = elements(i)
          val itemExpr = '{$scrutinee.arrayElement(${Expr(i)})}
          combined = descend(array, el, itemExpr, combined)
          i += 1

        // Capture the spread tail, if present, as a Json array
        if tailSpread then
          val idx = nextHole
          nextHole += 1
          types ::= TypeRepr.of[Json]

          combined =
            ' {
                $combined && {
                  val total = $scrutinee.arrayLength
                  val tailLen = total - ${Expr(prefixLen)}
                  val tail = new Array[Any](tailLen)
                  var k = 0

                  while k < tailLen do
                    tail(k) = $scrutinee.arrayElement(${Expr(prefixLen)} + k)
                    k += 1

                  $array(${Expr(idx)}) =
                    Json.ast(Json.Ast.arr(tail.asInstanceOf[IArray[Any]]))

                  true
                }
              }

        combined

      def countHolesInPrefix(elements: IArray[Any], upTo: Int): Int =
        var count = 0
        var i = 0

        while i < upTo do
          count += countHolesIn(elements(i))
          i += 1

        count

      def countHolesIn(node: Any): Int = node.asMatchable match
        case Unset =>
          1

        case s: String =>
          var c = 0
          var k = 0

          while k < s.length do
            if s.charAt(k) == Marker then c += 1
            k += 1

          c

        case _: Array[Double] @unchecked =>
          // Number-only array literal — never contains holes.
          0

        case arr: IArray[Any] @unchecked =>
          // Heterogeneous array or object, distinguished by parity.
          if (arr.length & 1) == 0 then
            // Object: alternating key/value. A `MarkerString` key marks an
            // object-rest hole; otherwise recurse into the value.
            val pairs = arr.length/2
            var c = 0
            var k = 0

            while k < pairs do
              if arr(k*2) == MarkerString then c += 1
              else c += countHolesIn(arr(k*2 + 1))

              k += 1

            c
          else
            // Heterogeneous array (with possible sentinel pad on the end).
            val elems = arrayElements(arr)
            var c = 0
            var k = 0

            while k < elems.length do
              c += countHolesIn(elems(k))
              k += 1

            c

        case _ =>
          0

      def descendObject
        ( array: Expr[Array[Any]],
         node: IArray[Any],
         scrutinee: Expr[Json.Ast],
         accept: Expr[Boolean] )
      :   Expr[Boolean] =

        val pairs = node.length/2

        val literalKeys: List[String] =
          (0 until pairs).toList.collect:
            case i if node(i*2).asInstanceOf[String] != MarkerString =>
              node(i*2).asInstanceOf[String]

        val hasRest: Boolean =
          (0 until pairs).exists: i => node(i*2).asInstanceOf[String] == MarkerString

        // Initial: object-shape and key cardinality.
        val cardinality: Expr[Boolean] =
          if hasRest then
            ' {
                $accept && $scrutinee.isObject &&
                  {
                    val n = $scrutinee.objectSize
                    var keysSet = Set.empty[String]
                    var k = 0

                    while k < n do
                      keysSet += $scrutinee.objectKey(k)
                      k += 1

                    ${Expr(literalKeys)}.forall(keysSet.contains)
                  }
              }
          else
            ' {
                $accept && $scrutinee.isObject &&
                  {
                    val n = $scrutinee.objectSize

                    n == ${Expr(literalKeys.length)} && {
                      var keysSet = Set.empty[String]
                      var k = 0

                      while k < n do
                        keysSet += $scrutinee.objectKey(k)
                        k += 1

                      ${Expr(literalKeys)}.forall(keysSet.contains)
                    }
                  }
              }

        var combined: Expr[Boolean] = cardinality
        var i = 0

        while i < pairs do
          val k = node(i*2).asInstanceOf[String]
          val v = node(i*2 + 1)
          if k == MarkerString then
            // Rest hole — capture remaining keys/values as a Json object
            val idx = nextHole
            nextHole += 1
            types ::= TypeRepr.of[Json]
            val literalKeysExpr = Expr(literalKeys)

            combined =
              ' {
                  $combined && {
                    val n = $scrutinee.objectSize
                    val keep = $literalKeysExpr.toSet
                    val keysBuf = scala.collection.mutable.ArrayBuffer.empty[String]
                    val valsBuf = scala.collection.mutable.ArrayBuffer.empty[Any]
                    var j = 0

                    while j < n do
                      val key = $scrutinee.objectKey(j)

                      if !keep.contains(key) then
                        keysBuf += key
                        valsBuf += $scrutinee.objectValue(j)

                      j += 1

                    $array(${Expr(idx)}) =
                      Json.ast(Json.Ast.obj(IArray.from(keysBuf), IArray.from(valsBuf)))

                    true
                  }
                }
          else
            val keyLiteral = Expr(k)

            val valueExpr =
              ' {
                  val idx2 = $scrutinee.objectIndexOf($keyLiteral)
                  if idx2 < 0 then null.asInstanceOf[Json.Ast] else $scrutinee.objectValue(idx2)
                }

            combined = descend(array, v, valueExpr, combined)

          i += 1

        combined

      val numberOfHoles =
        var c = 0
        var k = 0

        while k < parts2.length - 1 do
          c += 1
          k += 1

        c

      val result: Expr[Extrapolation[Json]] =
        ' {
            val extracts = new Array[Any](${Expr(numberOfHoles)})

            val matches: Boolean =
              ${descend('extracts, ast, '{$scrutinee.root}, '{true})}

            $ {
                if numberOfHoles == 0 then '{matches}
                else if numberOfHoles == 1 then
                  '{if !matches then None else Some(extracts(0).asInstanceOf[Json])}
                else
                  '{if !matches then None else Some(Tuple.fromArray(extracts))}
              }
          }

      types.length match
        case 0 =>
          '{$result.asInstanceOf[Boolean]}

        case 1 =>
          types.head.asType.absolve match
            case '[type result <: Json; result] =>
              '{$result.asInstanceOf[Option[result]]}

        case _ =>
          AppliedType(defn.TupleClass(types.length).info.typeSymbol.typeRef, types.reverse)
          . asType
          . absolve match
            case '[type result <: Tuple; result] =>
              '{$result.asInstanceOf[Option[result]]}


  // ── Staged parser generation ──────────────────────────────────────────────
  // Generates a monomorphic `Json.Parsable` for a case class: field values
  // live in typed locals, keys dispatch through a precomputed `KeyTable`
  // into an integer switch, builtin primitives read inline off the token
  // stream, and the record is built by a direct constructor call — no
  // `Array[Any]` buffer, no `Mirror`, no per-field boxing. Field types
  // beyond the builtins resolve through `Json.Field` instances (summoned at
  // expansion, initialized lazily so recursive references stay deferred),
  // so semantics — defaults, `@name`, absent-vs-null, unknown keys,
  // duplicates, error foci — are identical to `ParsableDerivation`. The
  // body is assembled from reflection trees with only small, immediately-
  // scoped quotes: chained quotes carrying `Type` bindings through closures
  // are unpicklable.

  private enum StagedKind:
    case IntK, LongK, DoubleK, FloatK, BooleanK, TextK, StringK, InstanceK

  def stagedParsable[value: Type](renames: Expr[Map[Text, Text]])(using Quotes)
  :   Expr[value is Json.Parsable] =

    import quotes.reflect.*
    import StagedKind.*

    val tpe = TypeRepr.of[value].dealias

    val classSymbol = tpe.classSymbol.getOrElse:
      report.errorAndAbort("jacinta: staged parsing requires a case class")

    if !classSymbol.flags.is(Flags.Case) then
      report.errorAndAbort
        ("jacinta: staged parsing requires a case class; sums and other types use "+
          "`Json.Parsable.derived`")

    if classSymbol.owner.isTerm then
      report.errorAndAbort
        ("jacinta: staged parsing requires a top-level or object-nested case class; "+
          "method-local classes use `Json.Parsable.derived`")

    val ctor = classSymbol.primaryConstructor

    if ctor.paramSymss.filterNot(_.exists(_.isTypeParam)).length != 1 then
      report.errorAndAbort
        ("jacinta: staged parsing requires a single parameter list; use "+
          "`Json.Parsable.derived`")

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

    // Keys compile to literal packed-word comparisons when no `@name`
    // annotation can rename them (renames resolve at runtime, so annotated
    // classes keep the table-resolving step). A field whose name itself
    // cannot pack still parses: an unpackable wire key always takes the
    // general `keyIndex` step, which matches all fields by string.
    val literalKeys: Boolean =
      val annotated = ctor.paramSymss.flatten.filterNot(_.isTypeParam).flatMap(_.annotations)
        ++ fields.flatMap(_.annotations)

      !annotated.exists { annotation =>
        annotation.tpe <:< TypeRepr.of[adversaria.name[?]] }

    def packedName(index: Int): Option[(Long, Long)] =
      val name = fieldNames(index)
      val length = name.length

      val packs = length > 0 && length <= 16 &&
        name.forall { char => char >= ' ' && char < 127 }

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

    def summonField(index: Int): Expr[Json.Field | Null] =
      if kinds(index) != InstanceK then '{null}
      else fieldTypes(index).asType match
        case '[fieldType] =>
          Expr.summon[fieldType is Json.Field].getOrElse:
            report.errorAndAbort
              (s"jacinta: no Json.Field instance for field ${fieldNames(index)}: "+
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

    def body
      ( reader:    Expr[JsonReader],
        foci:      Expr[Foci[Json.Focus]],
        tactic:    Expr[Tactic[JsonError]],
        keys:      Expr[IArray[String]],
        table:     Expr[Json.KeyTable],
        instances: Expr[IArray[Json.Field | Null]],
        fallbacks: Expr[IArray[Any]] )
    :   Expr[value] =

      val owner = Symbol.spliceOwner

      val slots = List.range(0, arity).map: index =>
        Symbol.newVal(owner, "slot"+index, fieldTypes(index), Flags.Mutable, Symbol.noSymbol)

      val seens = List.range(0, arity).map: index =>
        Symbol.newVal(owner, "seen"+index, TypeRepr.of[Boolean], Flags.Mutable, Symbol.noSymbol)

      val cursor = Symbol.newVal(owner, "index", TypeRepr.of[Int], Flags.Mutable, Symbol.noSymbol)

      val slotDefs = List.range(0, arity).map: index =>
        ValDef(slots(index), Some(zero(fieldTypes(index))))

      val seenDefs = List.range(0, arity).map: index =>
        ValDef(seens(index), Some(Literal(BooleanConstant(false))))

      // One switch arm per field: read the value (with focus bookkeeping),
      // assign it and mark it seen.
      val arms = List.range(0, arity).map: index =>
        val read: Term = fieldTypes(index).asType match
          case '[fieldType] =>
            val raw: Expr[fieldType] = kinds(index) match
              case IntK     => '{ $reader.long().toInt }.asExprOf[fieldType]
              case LongK    => '{ $reader.long() }.asExprOf[fieldType]
              case DoubleK  => '{ $reader.double() }.asExprOf[fieldType]
              case FloatK   => '{ $reader.double().toFloat }.asExprOf[fieldType]
              case BooleanK => '{ $reader.boolean() }.asExprOf[fieldType]
              case TextK    => '{ $reader.string() }.asExprOf[fieldType]
              case StringK  => '{ $reader.string().s }.asExprOf[fieldType]

              case InstanceK =>
                '{
                  $instances(${Expr(index)}).asInstanceOf[fieldType is Json.Field]
                  . parse($reader)
                }

            '{ Json.Parsable.focusing($foci, $keys(${Expr(index)}).tt)($raw) }.asTerm

        val rhs =
          Block
            ( List(Assign(Ref(slots(index)), read),
                Assign(Ref(seens(index)), Literal(BooleanConstant(true)))),
              Literal(UnitConstant()) )

        CaseDef(Literal(IntConstant(index)), None, rhs)

      val fallthrough = CaseDef(Wildcard(), None, '{ $reader.skipValue() }.asTerm)

      // The key loop. With literal keys, each step scans the key in place
      // and compares its packed words against the field names as immediate
      // constants; otherwise (or whenever the in-place scan cannot run) the
      // general step resolves the key through the table.
      val loop: List[Statement] =
        if literalKeys then
          val owner2 = owner
          val run = Symbol.newVal(owner2, "run", TypeRepr.of[Boolean], Flags.Mutable,
            Symbol.noSymbol)

          val word = Symbol.newVal(owner2, "word", TypeRepr.of[Long], Flags.EmptyFlags,
            Symbol.noSymbol)

          val high = Symbol.newVal(owner2, "high", TypeRepr.of[Long], Flags.EmptyFlags,
            Symbol.noSymbol)

          val found = Symbol.newVal(owner2, "found", TypeRepr.of[Int], Flags.EmptyFlags,
            Symbol.noSymbol)

          val wordRef = Ref(word).asExprOf[Long]
          val highRef = Ref(high).asExprOf[Long]

          def chain(index: Int): Term =
            if index == arity then '{ Json.KeyTable.Unknown }.asTerm
            else packedNames(index) match
              case None => chain(index + 1)

              case Some((low, highWord)) =>
                If
                  ( '{ $wordRef == ${Expr(low)} && $highRef == ${Expr(highWord)} }.asTerm,
                    Literal(IntConstant(index)),
                    chain(index + 1) )

          val resolve: Term =
            If
              ( '{ $wordRef == JsonReader.KeyOpaque }.asTerm,
                '{ $reader.keyIndex($table) }.asTerm,
                Block(List(ValDef(high, Some('{ $reader.keyWordHigh }.asTerm))), chain(0)) )

          val step: Term =
            Block
              ( List(ValDef(word, Some('{ $reader.keyWord() }.asTerm))),
                If
                  ( '{ $wordRef == JsonReader.KeyEnd }.asTerm,
                    Assign(Ref(run), Literal(BooleanConstant(false))),
                    Block
                      ( List(ValDef(found, Some(resolve))),
                        If
                          ( '{ ${Ref(found).asExprOf[Int]} == Json.KeyTable.End }.asTerm,
                            Assign(Ref(run), Literal(BooleanConstant(false))),
                            Match(Ref(found), arms :+ fallthrough) ) ) ) )

          List
            ( ValDef(run, Some(Literal(BooleanConstant(true)))),
              While(Ref(run), step) )
        else
          val next: Term = '{ $reader.keyIndex($table) }.asTerm
          val dispatch = Match(Ref(cursor), arms :+ fallthrough)

          List
            ( ValDef(cursor, Some(next)),
              While
                ( '{ ${Ref(cursor).asExprOf[Int]} != Json.KeyTable.End }.asTerm,
                  Block(List(dispatch), Assign(Ref(cursor), next)) ) )

      // Fields whose keys never arrived: the declared default, else the
      // field's absent value (`Unset`/`None` for optional shapes, an
      // `Absent` error otherwise).
      val absents: List[Term] = List.range(0, arity).map: index =>
        fieldTypes(index).asType match
          case '[fieldType] =>
            val onAbsent: Expr[fieldType] = kinds(index) match
              case InstanceK =>
                '{
                  $instances(${Expr(index)}).asInstanceOf[fieldType is Json.Field]
                  . absent()(using $tactic)
                }

              case _ => '{ Json.Parsable.missing[fieldType]()(using $tactic) }

            val resolve: Term =
              '{
                val declared = $fallbacks(${Expr(index)}).asInstanceOf[Optional[fieldType]]

                if !declared.absent then declared.asInstanceOf[fieldType]
                else Json.Parsable.focusing($foci, $keys(${Expr(index)}).tt)($onAbsent)
              }.asTerm

            If
              ( '{ !${Ref(seens(index)).asExprOf[Boolean]} }.asTerm,
                Assign(Ref(slots(index)), resolve),
                Literal(UnitConstant()) )

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
        ( '{ $reader.openObject() }.asTerm
            :: slotDefs ::: seenDefs
            ::: loop
            ::: absents,
          construct )
      . asExprOf[value]

    def summonOrAbort[required: Type](role: String): Expr[required] =
      Expr.summon[required].getOrElse:
        report.errorAndAbort(s"jacinta: staged parsing needs a contextual $role")

    val fociExpr = summonOrAbort[Foci[Json.Focus]]("Foci[Json.Focus]")
    val tacticExpr = summonOrAbort[Tactic[JsonError]]("Tactic[JsonError]")
    val nameExprs = fieldNames.map { name => Expr(name) }
    val instanceExprs = List.range(0, arity).map(summonField)
    val fallbackExprs = List.range(0, arity).map(declaredDefault)

    '{
      // Sealed per the codec-thunk pattern, like the derived instances: the
      // generated parser captures the resolution-scoped tactic and foci.
      // The instance and default arrays are single lazy vals, so recursive
      // self-references stay deferred until the first parse.
      caps.unsafe.unsafeAssumePure:
        val foci: Foci[Json.Focus] = $fociExpr
        val tactic: Tactic[JsonError] = $tacticExpr

        val keys: IArray[String] =
          Json.Parsable.wireKeys(IArray[String](${Varargs(nameExprs)}*), $renames)

        val table: Json.KeyTable = Json.KeyTable(keys)
        lazy val instances: IArray[Json.Field | Null] = IArray(${Varargs(instanceExprs)}*)
        lazy val fallbacks: IArray[Any] = IArray[Any](${Varargs(fallbackExprs)}*)

        new Json.Parsable:
          type Self = value
          def shape(): Morphology = Morphology.Any

          def parse(reader: JsonReader^): value =
            ${
              body
                ( '{reader}, '{foci}, '{tactic}, '{keys}, '{table}, '{instances},
                  '{fallbacks} )
            }
    }

  // Generates a monomorphic `Json.Parsable` for a sealed sum with a
  // field-discriminated wire shape: the tag is located with `discriminant`'s
  // bounded scan-ahead (which rewinds), dispatched through a chain of
  // monomorphic string comparisons against the wire variant names (renames
  // applied once, at instance construction), and the chosen variant parses
  // the whole object directly through its `Json.Field` instance (summoned at
  // expansion, so a sibling staged given composes) — no per-occurrence map
  // building, no generic-equality dispatch, no `delegate` fold. A missing
  // tag and an unknown tag raise exactly as `ParsableDerivation.disjunction`
  // does: `JsonError(Absent)` and wisteria's `VariantError`, each through
  // the same deferred `provide` summons the derived engine uses.
  def stagedSum[value: Type](renames: Expr[Map[Text, Text]])(using Quotes)
  :   Expr[value is Json.Parsable] =

    import quotes.reflect.*

    val tpe = TypeRepr.of[value].dealias

    val classSymbol = tpe.classSymbol.getOrElse:
      report.errorAndAbort("jacinta: staged sum parsing requires a sealed trait or enum")

    tpe match
      case AppliedType(_, _) =>
        report.errorAndAbort
          ("jacinta: staged parsing does not support generic sums; use `Json.Parsable.derived`")

      case _ =>
        ()

    val children = classSymbol.children

    if children.isEmpty then
      report.errorAndAbort("jacinta: staged sum parsing requires at least one variant")

    if !children.forall { child => child.isClassDef && child.flags.is(Flags.Case) } then
      report.errorAndAbort
        ("jacinta: staged sum parsing requires every variant to be a case class; singleton "+
          "variants use `Json.Parsable.derived`")

    val variantTypes: List[TypeRepr] = children.map(_.typeRef)
    val variantNames: List[String] = children.map(_.name)
    val arity = children.length

    def summonVariant(index: Int): Expr[Json.Field] =
      variantTypes(index).asType match
        case '[variantType] =>
          Expr.summon[variantType is Json.Field].getOrElse:
            report.errorAndAbort
              (s"jacinta: no Json.Field instance for variant ${variantNames(index)}: "+
                variantTypes(index).show)

    val discriminableExpr: Expr[value is Discriminable in Json] =
      Expr.summon[value is Discriminable in Json].getOrElse:
        report.errorAndAbort
          ("jacinta: staged sum parsing needs a contextual `Discriminable in Json`, like "+
            "`jacinta.discriminables.jsonByKindDiscriminable`")

    val nameExprs = variantNames.map { name => Expr(name) }
    val variantExprs = List.range(0, arity).map(summonVariant)

    // The dispatch chain: one monomorphic comparison per variant, ending in
    // the derived engine's unknown-variant raise.
    def dispatch
      ( index:        Int,
        reader:       Expr[JsonReader],
        wire:         Expr[Text],
        wireString:   Expr[String],
        variants:     Expr[IArray[Json.Field]],
        wireVariants: Expr[IArray[String]] )
    :   Expr[value] =

      if index == arity then
        '{
          provide[Tactic[VariantError]]:
            abort(VariantError[value]($wire))
        }
      else variantTypes(index).asType match
        case '[type variantType <: value; variantType] =>
          '{
            if $wireVariants(${Expr(index)}) == $wireString then
              $variants(${Expr(index)}).asInstanceOf[variantType is Json.Field].parse($reader)
            else ${ dispatch(index + 1, reader, wire, wireString, variants, wireVariants) }
          }

    '{
      // Sealed per the codec-thunk pattern, like the derived instances: the
      // variant instances may capture resolution-scoped tactics. The variant
      // array is a single lazy val, so recursive references stay deferred.
      caps.unsafe.unsafeAssumePure:
        val discriminable: value is Discriminable in Json = $discriminableExpr
        val tagField: Text = Json.Parsable.discriminantField(discriminable)

        val wireVariants: IArray[String] =
          Json.Parsable.wireKeys(IArray[String](${Varargs(nameExprs)}*), $renames)

        lazy val variants: IArray[Json.Field] = IArray(${Varargs(variantExprs)}*)

        new Json.Parsable:
          type Self = value
          def shape(): Morphology = Morphology.Any

          def parse(reader: JsonReader^): value =
            provide[Tactic[JsonError]]:
              val wire: Text = reader.discriminant(tagField).or:
                abort(JsonError(JsonError.Reason.Absent))

              val wireString: String = wire.s

              ${
                dispatch
                  ( 0, '{reader}, '{wire}, '{wireString}, '{variants}, '{wireVariants} )
              }
    }
