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
package spectacular

import scala.reflect

import scala.{caps, compiletime}

import scala.collection.mutable as scm

import anticipation.*
import denominative.*
import fulminate.*
import hypotenuse.*
import prepositional.*
import rudiments.*
import vacuous.*
import wisteria.*

// Style guide for `Inspectable` instances
// ═══════════════════════════════════════
//
// `Inspectable` renders a value for a programmer to read — in a debugger, or in test output.
// It is not `Showable` (text for human consumption, locale- and configuration-sensitive) and
// not `Encodable in Text` (a wire form intended to be decoded again). The principles:
//
//  1. Show every piece of state which is relevant at the value's *static* type. A rendering
//     which omits state is worse than a verbose one: the reader is debugging precisely
//     because their model of the value is wrong.
//  2. Be unambiguous against every other type's rendering. Two different types must never
//     render identically; this is what the suffixes and brackets below buy.
//  3. Stay compact and on one line. Inspection output appears inline, in tables, and nested
//     inside other renderings.
//  4. Resemble valid source where that is cheap (`'x'`, `3L`, `t"…"`, `BigInt(42)`), and use
//     Unicode decoration where it is not (`⟨ 1 2 3 ⟩`, `1ˢᵗ`, `42ᵘ⁸`).
//
// Two markers indicate that no native instance was found and something else was rendered in
// its place. Both are signs of incomplete coverage rather than a working rendering:
//
//     “…”   no instance at all; the value's `toString`. Always a design failure.
//     ⸢…⸣   borrowed from the type's `Showable` — a human-facing form, not a debug form.
//     ⸤…⸥   borrowed from the type's `Encodable in Text` — a wire form, possibly encoded.
//
// A type whose `Showable` or `Encodable` output is escaped or encoded (URL-encoding, say)
// *must* define its own `Inspectable`; see `legerdemain.Query` for the canonical case.
//
// Notation in use, by category:
//
//     containers     [a, b] list       {a, b} set        {k → v} map
//                    ⟨ a b ⟩ sequence  ⟦k → v⟧ ledger    ⦋…⦌ array   ⁅…⁆ frozen array
//                    a ⋰ b ⋰ ..? lazy  ∿∿∿ unforced      ⯁ end
//     products       Name(field:value ╱ field2:value)    (a ╱ b) tuple
//     optionality    ｢value｣ present   ○ absent
//     text           t"…" with escapes                   'x' char
//     numbers        3 int   3L long   3.1F float   3.toByte byte   BigInt(42)
//     sized numbers  42ᵘ⁸ unsigned     -7ˢ³² signed      2Fᵇ⁸ bits   3.14ᶠ⁶⁴ float
//     positions      1ˢᵗ ordinal       1ˢᵗ‥5ᵗʰ interval  ⟪4:8+5⟫ span
//
object Inspectable extends Inspectable2:
  object Derivation extends Derivable[Inspectable]:
    inline def conjunction[derivation <: Product: ProductReflection]: derivation is Inspectable =
      value =>
        val rendered = fields(value): [field] => field =>
          val text = contextual.text(field)
          if tuple then text else s"$label:$text"

        if rendered.readable.isEmpty && !tuple then typeName
        else rendered.readable.mkString(if tuple then "(" else s"$typeName(", " ╱ ", ")").tt

    inline def disjunction[derivation: SumReflection]: derivation is Inspectable = value =>
      variant(value):
        [variant <: derivation] => variant => contextual.give(variant.inspect)

  // Every instance below is parameterised over the subtypes of the type it renders, rather
  // than written against that type alone. `Self` is invariant, so `Char is Inspectable` does
  // not cover a singleton literal type like `'x'`, `Text is Inspectable` does not cover
  // `nomenclature.Name`, and an instance for a refined type covers neither the bare type nor a
  // further refinement of it. Each such near-miss falls silently through `derived` to the
  // `“…”` toString case rather than failing to compile, so the bound is what keeps a rendering
  // from disappearing when a type is refined.
  given char: [char <: Char] => char is Inspectable = char => (("'": String)+escape(char).s+"'").tt
  given int: [int <: Int] => int is Inspectable = int => int.toString.tt
  given long: [long <: Long] => long is Inspectable = long => (long.toString+"L").tt
  given byte: [byte <: Byte] => byte is Inspectable = byte => (byte.toString+".toByte").tt
  given short: [short <: Short] => short is Inspectable = short => (short.toString+".toShort").tt

  given string: [string <: String] => string is Inspectable = string =>
    text.text(string.tt).s.substring(1).nn.tt

  given text: [text <: Text] => text is Inspectable = text =>
    val builder: StringBuilder = new StringBuilder()
    text.each { char => builder.append(escape(char, true).s) }

    (("t\"": String)+builder.toString+"\"").tt

  given float: [float <: Float] => float is Inspectable =
    case Float.PositiveInfinity => "Float.PositiveInfinity".tt
    case Float.NegativeInfinity => "Float.NegativeInfinity".tt
    case float if float.isNaN   => "Float.NaN".tt
    case float                  => (float.toString+"F").tt

  given double: [double <: Double] => double is Inspectable =
    case Double.PositiveInfinity => "Double.PositiveInfinity".tt
    case Double.NegativeInfinity => "Double.NegativeInfinity".tt
    case double if double.isNaN  => "Double.NaN".tt
    case double                  => double.toString.tt

  given boolean: [boolean <: Boolean] => boolean is Inspectable = boolean =>
    if boolean then "true".tt else "false".tt

  given unit: [unit <: Unit] => unit is Inspectable = unit => "()".tt
  given bigInt: [bigInt <: BigInt] => bigInt is Inspectable = bigInt => (("BigInt(": String)+bigInt+")").tt

  given bigDecimal: [bigDecimal <: BigDecimal] => bigDecimal is Inspectable = bigDecimal =>
    (("BigDecimal(": String)+bigDecimal+")").tt

  given unset: [unset <: Unset] => unset is Inspectable = unset => "○".tt

  // Only for a value statically typed as `reflect.Enum` itself, for which no reflection is
  // available; a value of a known enum type is rendered structurally by `enumeration`, below.
  given reflectEnum: reflect.Enum is Inspectable = _.toString.show

  // Enums reach `Showable`'s `enumeration` instance, which renders them with `toString` and so
  // loses the field labels of a parameterised case — `Circle(5)` rather than `Circle(radius:5)`.
  // Deriving them here, in the companion, takes priority over that borrowed rendering, while
  // still yielding to an enum which defines its own instance (a more specific given wins).
  inline given enumeration: [enumeration <: reflect.Enum: Reflection]
  =>  enumeration is Inspectable =
    Derivation.derived[enumeration]

  // The sized numeric types all erase to a primitive, so a rendering which showed only the
  // number would be indistinguishable from an `Int` or a `Long` — and, for the unsigned types,
  // would show the wrong number entirely. Each carries a superscript suffix naming its
  // interpretation (`ᵘ` unsigned, `ˢ` signed, `ᵇ` bits, `ᶠ` floating-point) and its width.
  // Each value is widened to the exact type before its accessor is called: these are `inline`
  // extensions on an opaque type, whose expansion unseals the underlying primitive, and that
  // expansion is not available through a subtype of the opaque type.
  given u8: [u8 <: U8] => u8 is Inspectable = u8 => ((u8: U8).text.s+"ᵘ⁸").tt
  given u16: [u16 <: U16] => u16 is Inspectable = u16 => ((u16: U16).text.s+"ᵘ¹⁶").tt
  given u32: [u32 <: U32] => u32 is Inspectable = u32 => ((u32: U32).text.s+"ᵘ³²").tt
  given u64: [u64 <: U64] => u64 is Inspectable = u64 => ((u64: U64).text.s+"ᵘ⁶⁴").tt
  given s8: [s8 <: S8] => s8 is Inspectable = s8 => ((s8: S8).int.toString+"ˢ⁸").tt
  given s16: [s16 <: S16] => s16 is Inspectable = s16 => ((s16: S16).int.toString+"ˢ¹⁶").tt
  given s32: [s32 <: S32] => s32 is Inspectable = s32 => ((s32: S32).int.toString+"ˢ³²").tt
  given s64: [s64 <: S64] => s64 is Inspectable = s64 => ((s64: S64).long.toString+"ˢ⁶⁴").tt

  // The bit types are rendered as full-width hexadecimal — zero-padded, so that the width is
  // legible from the rendering itself, and uppercase, so that the digits are distinct from the
  // superscript suffix which follows them. The hexadecimal is formatted here rather than
  // through hypotenuse's own `hex`, whose inline expansion of `String.format` does not survive
  // separation checking at this use site.
  given b8: [b8 <: B8] => b8 is Inspectable = b8 =>
    (hexadecimal((b8: B8).s8.int.toLong, 2)+"ᵇ⁸").tt

  given b16: [b16 <: B16] => b16 is Inspectable = b16 =>
    (hexadecimal((b16: B16).s16.int.toLong, 4)+"ᵇ¹⁶").tt

  given b32: [b32 <: B32] => b32 is Inspectable = b32 =>
    (hexadecimal((b32: B32).s32.int.toLong, 8)+"ᵇ³²").tt

  given b64: [b64 <: B64] => b64 is Inspectable = b64 =>
    (hexadecimal((b64: B64).s64.long, 16)+"ᵇ⁶⁴").tt

  private def hexadecimal(value: Long, digits: Int): String =
    val masked = if digits >= 16 then value else value & ((1L << (digits*4)) - 1)
    val string = java.lang.Long.toHexString(masked).nn.toUpperCase.nn
    val builder: StringBuilder = new StringBuilder()
    while builder.length + string.length < digits do builder.append('0')

    builder.append(string).toString

  given f32: [f32 <: F32] => f32 is Inspectable = f32 =>
    val float: Float = (f32: F32).float
    (floatingPoint(float.toDouble, float.isNaN)+"ᶠ³²").tt

  given f64: [f64 <: F64] => f64 is Inspectable = f64 =>
    val double: Double = (f64: F64).double
    (floatingPoint(double, double.isNaN)+"ᶠ⁶⁴").tt

  private def floatingPoint(double: Double, nan: Boolean): String =
    if nan then "NaN"
    else if double == Double.PositiveInfinity then "∞"
    else if double == Double.NegativeInfinity then "-∞"
    else double.toString

  // The remaining hypotenuse numerics: two rationals, which render as a fraction, and two
  // arbitrary-precision decimals. Each keeps hypotenuse's own textual form and adds the suffix
  // which says which type produced it — without one, `3/4` and `3.14` would be as anonymous as
  // the primitives the sized types erase to.
  given q32: [q32 <: Q32] => q32 is Inspectable = q32 => ((q32: Q32).text.s+"ʳ³²").tt
  given q64: [q64 <: Q64] => q64 is Inspectable = q64 => ((q64: Q64).text.s+"ʳ⁶⁴").tt
  given bcd: [bcd <: Bcd] => bcd is Inspectable = bcd => ((bcd: Bcd).text+"ᵇᶜᵈ").tt

  given decimal: [decimal <: Decimal] => decimal is Inspectable = decimal =>
    ((decimal: Decimal).text.s+"ᵈ").tt

  // An `Ordinal` is rendered in its one-based form, with the English ordinal suffix, since that
  // is the number the programmer counts with; the zero-based `n0` is an implementation detail
  // of the type, and showing it would make every rendering off by one.
  given ordinal: [ordinal <: Ordinal] => ordinal is Inspectable = ordinal(_)

  // Shared by `ordinal` and `interval`; the givens themselves are parameterised, so neither
  // can be referred to by name without a type application.
  private def ordinal(value: Ordinal): Text =
    (value.n1.toString+ordinalSuffix(value.n1)).tt

  private def ordinalSuffix(n1: Int): String =
    if n1 % 100 >= 11 && n1 % 100 <= 13 then "ᵗʰ" else n1 % 10 match
      case 1 => "ˢᵗ"
      case 2 => "ⁿᵈ"
      case 3 => "ʳᵈ"
      case _ => "ᵗʰ"

  given interval: [interval <: Interval] => interval is Inspectable = interval =>
    val value: Interval = interval
    if value.nil then "∅".tt else (ordinal(value.start).s+("‥": String)+ordinal(value.end).s).tt

  // A `Span` packs one of five differently-shaped ranges into a `Long`, so its rendering shows
  // the shape as well as the numbers: `⟪∅⟫` empty, `⟪@4+5⟫` an offset and length, `⟪4:8+5⟫` a
  // line, column and length, `⟪4‥8⟫` a range of whole lines, and `⟪4:8‥6:2⟫` an area. The
  // numbers are one-based ordinals, without suffixes, which the `:` and `‥` keep unambiguous.
  given span: [span <: Span] => span is Inspectable = span =>
    def n(ordinal: Optional[Ordinal]): String = ordinal.let(_.n1.toString).or("?")

    val body = span.mode match
      case Span.Mode.Empty  => "∅"
      case Span.Mode.Offset => ("@(": String)+n(span.offset)+(": String)+": String)+span.length.let(_.toString).or("?")
      case Span.Mode.Lines  => n(span.startLine)+("‥": String)+n(span.endLine)

      case Span.Mode.Line =>
        n(span.startLine)+(":(": String)+n(span.startColumn)+(": String)+": String)+span.length.let(_.toString).or("?")

      case Span.Mode.Area =>
        n(span.startLine)+(":": String)+n(span.startColumn)+("‥": String)+n(span.endLine)+(":": String)+n(span.endColumn)

    (("⟪": String)+body+"⟫").tt

  // `Bytes` is a count, not a quantity to be rounded for display: an inspection which showed
  // `4MB` would hide the difference between two nearby sizes, which is usually the reason for
  // looking.
  given bytes: [bytes <: Bytes] => bytes is Inspectable = bytes => (bytes.long.toString+"B").tt
  given digit: [digit <: Digit] => digit is Inspectable = digit => (digit.int.toString+"ᵈᵍ").tt

  // A `Message` renders as its own text, in the style of a `t"…"` literal but marked `m"…"`,
  // since the interpolated parts are no longer distinguishable once the message is built.
  given message: [message <: Message] => message is Inspectable = message =>
    (("m\"": String)+message.text.s+"\"").tt

  // A missing instance is never a compile error: `derived` always succeeds, and quietly
  // substitutes a `toString`, a `Showable` or an `Encodable` rendering, each marked as such.
  // Nothing therefore stops coverage from regressing — a type gains a refinement, or a new
  // type is added, and its rendering silently degrades. `fallbacks` is what a library's tests
  // assert on to prevent that: it returns those of the given renderings which carry a marker,
  // so a failure names the renderings which are not native rather than merely counting them.
  //
  //     test(m"aviation's core types inspect natively"):
  //       Inspectable.fallbacks(instant.inspect, date.inspect, month.inspect)
  //     . assert(_ == Nil)
  //
  def fallbacks(renderings: Text*): List[Text] =
    renderings.filter { rendering => rendering.s.exists(marker(_)) }.to(List)

  def marker(char: Char): Boolean = char == '“' || char == '⸢' || char == '⸤'

  def escape(char: Char, eEscape: Boolean = false): Text = char match
    case '\n'                => "\\n".tt
    case '\t'                => "\\t".tt
    case '\r'                => "\\r".tt
    case '\\'                => "\\\\".tt
    case '\"'                => "\\\"".tt
    case '\''                => "\\\'".tt
    case '\b'                => "\\b".tt
    case '\f'                => "\\f".tt
    case '\u001b' if eEscape => "\\e".tt

    case char =>
      if char < 128 && char >= 32
      then char.toString.tt
      else String.format("\\u%04x", Integer.valueOf(char.toInt)).nn.tt

  // The collection instances below retain their by-name element instance, which shares each
  // instance's given-resolution lifetime. The by-name is bound as a *pure thunk* before the
  // SAM body: under `-scalajs` the SAM expands to an anonymous class before capture checking,
  // and the pure self-type of an `Inspectable` (a `Typeclass.Pure`) forbids it from capturing
  // the by-name parameter directly — a seal on the lambda cannot reach that self-type check.
  // The thunk keeps resolution lazy, which recursive derivations depend on (the codec-thunk
  // seal pattern; see rep/DECISIONS.md).
  // `Set`, `Map` and `List` keep an exact `Self`, unlike `sequence` below. They are opaque
  // types, so their upper bound is abstract outside proscenium, and a *stdlib* collection
  // reaching one of these instances would fail the bound check outright rather than fall
  // through to another candidate — a subtype bound here turns a silent miss into a hard error.
  given set: [element] => (inspectable: => element is Inspectable) => Set[element] is Inspectable =
    val insp: () -> (element is Inspectable) = caps.unsafe.unsafeAssumePure(() => inspectable)
    _.map(insp().text(_)).stdlib.mkString("{", ", ", "}").tt

  given map: [key, value]
  =>  ( inspectableKey: => key is Inspectable, inspectableValue: => value is Inspectable )
  =>  Map[key, value] is Inspectable =

    val inspKey: () -> (key is Inspectable) = caps.unsafe.unsafeAssumePure(() => inspectableKey)

    val inspValue: () -> (value is Inspectable) =
      caps.unsafe.unsafeAssumePure(() => inspectableValue)

    entries =>
      entries.remap: (key, value) =>
        inspKey().text(key).s+(" → ": String)+inspValue().text(value).s

      . stdlib.mkString("{", ", ", "}").tt

  // A `Ledger` keeps its insertion order, so it is bracketed differently from the unordered
  // `Map` above: the rendering has to say which of the two a value is, since the entries of a
  // `Map` may be printed in any order and those of a `Ledger` may not.
  given ledger: [key, value]
  =>  ( inspectableKey: => key is Inspectable, inspectableValue: => value is Inspectable )
  =>  Ledger[key, value] is Inspectable =

    val inspKey: () -> (key is Inspectable) = caps.unsafe.unsafeAssumePure(() => inspectableKey)

    val inspValue: () -> (value is Inspectable) =
      caps.unsafe.unsafeAssumePure(() => inspectableValue)

    ledger =>
      ledger.stdlib.map: (key, value) =>
        inspKey().text(key).s+(" → ": String)+inspValue().text(value).s

      . mkString("⟦", ", ", "⟧").tt

  // `Self` is subtype-parametric so branded literals (`Sequence(1, 2, 3)`, typed
  // `Sequence[Int] & Populated`) match; rendering produces no collection, so no proof leaks.
  given sequence: [element, sequence <: Sequence[element]]
  =>  (inspectable: => element is Inspectable)
  =>  sequence is Inspectable =

    val insp: () -> (element is Inspectable) = caps.unsafe.unsafeAssumePure(() => inspectable)
    _.map(insp().text(_)).stdlib.mkString("⟨ ", " ", " ⟩").tt

  given list: [element] => (inspectable: => element is Inspectable)
  =>  List[element] is Inspectable =

    val insp: () -> (element is Inspectable) = caps.unsafe.unsafeAssumePure(() => inspectable)
    _.map(insp().text(_)).stdlib.mkString("[", ", ", "]").tt

  given array: [element] => (inspectable: => element is Inspectable)
  =>  scala.Array[element] is Inspectable =

    val insp: () -> (element is Inspectable) = caps.unsafe.unsafeAssumePure(() => inspectable)

    array =>
      array.iterator.zipWithIndex.map: (value, index) =>
        val subscript = index.toString.map { digit => (digit + 8272).toChar }.mkString
        (subscript+insp().text(value).s).tt

      . mkString(("⦋": String)+arrayPrefix(array.toString), "∣", "⦌").tt

  given arraySeq: [element, arraySeq <: scm.ArraySeq[element]]
  =>  (inspectable: => element is Inspectable)
  =>  arraySeq is Inspectable =
    val insp: () -> (element is Inspectable) = caps.unsafe.unsafeAssumePure(() => inspectable)

    array =>
      array.zipWithIndex.map: (value, index) =>
        val subscript = index.toString.map { digit => (digit + 8272).toChar }.mkString
        (subscript+insp().text(value).s).tt

      . mkString(("⦋": String)+arrayPrefix(array.toString), "∣", "⦌ₛ").tt

  // Exact `Self`, for the reason given at `set` above: `Chain` is opaque too.
  given stream: [element] => (inspectable: => element is Inspectable)
  =>  Chain[element] is Inspectable =

    val insp: () -> (element is Inspectable) = caps.unsafe.unsafeAssumePure(() => inspectable)

    stream =>
      def recur(stream: Chain[element], todo: Int): Text =
        if todo <= 0 then "..?".tt
        // The opaque `Chain`'s runtime `toString` still comes from the underlying
        // `sci.LazyList`, so the un-forced marker is spelt `LazyList(<not computed>)`.
        else if stream.toString == "LazyList(<not computed>)" then "∿∿∿".tt
        else stream match
          case first #:: rest => (insp().text(first).s+(" ⋰ ": String)+recur(rest, todo - 1)).tt
          case _              => "⯁ ".tt

      recur(stream, 3)

  given iarray: [element] => (inspectable: => element is Inspectable)
  =>  (Array[element]^{}) is Inspectable =

    val insp: () -> (element is Inspectable) = caps.unsafe.unsafeAssumePure(() => inspectable)

    iarray =>
      iarray.readable.zipWithIndex.map: (value, index) =>
        val subscript = index.toString.map { digit => (digit + 8272).toChar }.mkString
        subscript+insp().text(value).s.tt

      . mkString(arrayPrefix(iarray.toString)+"⁅", "╱", "⁆").tt

  private def arrayPrefix(string: String): String =
    val brackets = string.count(_ == '[')

    val arrayType: String = string.charAt(brackets) match
      case 'B' => "🅱" // Byte
      case 'C' => "🅲" // Char
      case 'D' => "🅳" // Double
      case 'F' => "🅵" // Float
      case 'I' => "🅸" // Int
      case 'J' => "🅹" // Long
      case 'L' => "🅻" // Object
      case 'S' => "🆂" // Short
      case 'Z' => "🆉" // Boolean
      case _   => "🯄" // Unknown

    val dimension = if brackets < 2 then "".tt else brackets.toString.map { digit => ("⁰¹²³⁴⁵⁶⁷⁸⁹": String).charAt(digit - '0') }.tt

    arrayType+dimension//+renderBraille(string.split("@").nn(1).nn)

  // A pure thunk like the collection instances above; see that comment.
  given option: [value] => (inspectable: => value is Inspectable) => Option[value] is Inspectable =
    val insp: () -> (value is Inspectable) = caps.unsafe.unsafeAssumePure(() => inspectable)

    {
      case None        => "None".tt
      case Some(value) => s"Some(${insp().text(value).s})".tt
    }

  given none: None.type is Inspectable = none => "None".tt

trait Inspectable2:
  // The `Encodable` and `Showable` branches borrow a rendering which was designed for another
  // purpose — a wire form and a human-facing form respectively — and neither is guaranteed to
  // show the value's state as a programmer needs to see it. They are kept for coverage, but
  // their output is bracketed so that a borrowed rendering is visible as such at a glance, and
  // so that the types still relying on them can be found by inspecting output. A type whose
  // encoded form is escaped (`legerdemain.Query`, URL-encoded) must define its own instance.
  inline given derived: [value] => value is Inspectable = compiletime.summonFrom:
    case given (`value` is Encodable in Text) => value => (("⸤": String)+value.encode.s+"⸥").tt
    case given (`value` is Showable)          => value => (("⸢": String)+value.show.s+"⸣").tt

    case mandatable: (`value` is Mandatable) =>
      val inspectable = compiletime.summonInline[mandatable.Result is Inspectable]

      optional =>
        optional.let: present =>
          s"｢${inspectable.text(present.asInstanceOf[mandatable.Result])}｣".tt

        . or("○".tt)

    case given Reflection[`value`] => Inspectable.Derivation.derived[value]
    case _                         => value => (("“": String)+value+"”").tt

trait Inspectable extends Typeclass.Pure:
  def text(value: Self): Text
  def contramap[self2](lambda: self2 -> Self): self2 is Inspectable = value => text(lambda(value))
