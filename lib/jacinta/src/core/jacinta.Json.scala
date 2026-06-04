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
package jacinta

import language.dynamics
import language.experimental.pureFunctions

import scala.collection.Factory
import scala.collection.mutable as scm
import scala.compiletime.*

import adversaria.*
import anticipation.*
import contextual.*
import contingency.*
import denominative.*
import distillate.*
import fulminate.*
import gossamer.*
import hieroglyph.*
import panopticon.*
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*
import symbolism.*
import turbulence.*
import vacuous.*
import wisteria.*
import zephyrine.*

import JsonError.Reason

// Base mixin for Jacinta's `Decodable in Json` instances. Fixes the focus
// type to `Json.Focus` and provides the position-enrichment hook that
// `Json#as[T]` runs over the accumulated `Foci[Json.Focus]` after decoding.
private[jacinta] trait JsonDecodable[T] extends Decodable:
  type Self = T
  type Form = Json
  type Locus = Json.Focus
  override def position(value: Json, focus: Json.Focus): Json.Focus =
    focus.withPosition(value)

trait Json2:
  // Lower-priority adapter that lifts any `Decodable in Json` into one
  // carrying `type Locus = Json.Focus`. Used by `as[T]` so it can call
  // `.position` (delegating to `Json.Focus.withPosition`) over the
  // accumulated `Foci[Json.Focus]` after decoding. Specific `Decodable in
  // Json` givens stay unchanged; the wrapping happens only at the
  // `at Json.Focus` boundary.
  inline given decodableAtFocus: [value]
  =>  (inner: value is Decodable in Json)
  =>  value is Decodable in Json at Json.Focus =

    new JsonDecodable[value]:
      def decoded(json: Json): value = inner.decoded(json)

  given optionalEncodable: [inner <: value, value >: Unset.type: Mandatable to inner]
  =>  ( encodable: inner is Encodable in Json )
  =>  value is Encodable in Json =

    new Encodable:
      type Self = Optional[value]
      type Form = Json

      def encoded(value: Optional[value]): Json =
        value.let(_.asInstanceOf[inner]).let(encodable.encode(_)).or(Json.ast(Json.Ast(Unset)))


  given optional: [inner <: value, value >: Unset.type: Mandatable to inner] => Tactic[JsonError]
  =>  ( decodable: => inner is Decodable in Json )
  =>  value is Decodable in Json = json =>

    if json.root.isAbsent then Unset else decodable.decoded(json)


  given bytes: Tactic[JsonError] => Bytes is Decodable in Json = json => json.root.long.b

  inline given decodable: [value] => value is Decodable in Json = summonFrom:
    case given (`value` is Decodable in Text) =>
      provide[Tactic[JsonError]](_.root.string.decode[value])

    case given Reflection[`value`] =>
      DecodableDerivation.derived

  inline given encodable: [value] => value is Encodable in Json = summonFrom:
    case given (`value` is Encodable in Text) => value => Json.ast(Json.Ast(value.encode.s))
    case given Reflection[`value`]            => EncodableDerivation.derived

  object DecodableDerivation extends Derivable[Decodable in Json]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Decodable in Json =

      json =>
        provide[Foci[Json.Focus]]:
          provide[Tactic[JsonError]]:
            val root = json.root
            val n = root.objectSize
            val values = scm.HashMap.empty[String, Json.Ast]
            var i = 0

            while i < n do
              values.update(root.objectKey(i), root.objectValue(i))
              i += 1

            // `@name[Json]` / bare `@name` renames: field name -> JSON key, read
            // back the same way they are written.
            val renames: Map[Text, Text] = relabelling[derivation, Json]

            build: [field] =>
              context =>
                val key: Text = renames.at(label).or(label)
                focus({
                  val base = prior.let(_.pointer).or(JsonPointer())

                  val newPointer =
                    JsonPointer
                      ( base.url,
                        Path[JsonPointer, JsonPointer.type, Tuple]
                          ( base.path.root, base.path.descent :+ key ) )

                  Json.Focus(newPointer)
                }):
                  values.get(key.s) match
                    case Some(value) => context.decoded(new Json(value))
                    case None        => default.or(context.decoded(new Json(Json.Ast(Unset))))

    inline def disjunction[derivation: SumReflection]: derivation is Decodable in Json =
      json =>
        provide[Tactic[JsonError]]:
          provide[Tactic[VariantError]]:
            val discriminable = infer[derivation is Discriminable in Json]

            // `@name[Json]` / bare `@name` variant renames: map the serialized
            // discriminator back to the variant name before delegating.
            val variantNames: Map[Text, Text] =
              variantRelabelling[derivation, Json].map((variant, wire) => wire -> variant)

            val wire: Text = discriminable.discriminate(json).or:
              focus(prior.or(Json.Focus(JsonPointer())))(abort(JsonError(Reason.Absent)))

            val discriminant: Text = variantNames.getOrElse(wire, wire)

            delegate(discriminant): [variant <: derivation] =>
              context => context.decoded(json)

  object EncodableDerivation extends Derivable[Encodable in Json]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Encodable in Json =

      value =>
        provide[Foci[Json.Focus]]:
          val labels: scm.ArrayBuffer[String] = scm.ArrayBuffer()
          val values: scm.ArrayBuffer[Json.Ast] = scm.ArrayBuffer()

          // `@name[Json]` / bare `@name` renames: field name -> JSON key.
          val renames: Map[Text, Text] = relabelling[derivation, Json]

          fields(value): [field] =>
            field =>
              val key: Text = renames.at(label).or(label)
              focus({
                val base = prior.let(_.pointer).or(JsonPointer())

                val newPointer =
                  JsonPointer
                    ( base.url,
                      Path[JsonPointer, JsonPointer.type, Tuple]
                        ( base.path.root, base.path.descent :+ key ) )

                Json.Focus(newPointer)
              }):
                contextual.encode(field).root.tap: encoded =>
                  if !encoded.isAbsent then
                    labels += key.s
                    values += encoded

          Json.ast
            ( Json.Ast.obj
                ( unsafely(labels.toArray.immutable), unsafely(values.toArray.immutable) ) )

    inline def disjunction[derivation: SumReflection]: derivation is Encodable in Json = value =>
      val discriminable = infer[derivation is Discriminable in Json]

      // `@name[Json]` / bare `@name` variant renames: variant name -> wire
      // discriminator, read back the same way by the decoder.
      val variantNames: Map[Text, Text] = variantRelabelling[derivation, Json]

      variant(value): [variant <: derivation] =>
        value => discriminable.rewrite(variantNames.getOrElse(label, label), contextual.encode(value))

object Json extends Json2, Dynamic:
  type JsonString  = String
  type JsonNumber  = Long | Double | Bcd | Int
  type JsonBoolean = Boolean
  type JsonNull    = Null
  type JsonObject  = IArray[Any]
  type JsonArray   = IArray[Any] | Array[Long] | Array[Int]

  // All internal references in a `PositionIndex` are stored as offsets
  // relative to the start of the containing node descriptor, so any slice
  // extracted at a descriptor boundary is itself a valid `PositionIndex`.
  opaque type PositionIndex = IArray[Int]

  object PositionIndex:
    private[jacinta] def apply(data: IArray[Int]): PositionIndex = data

  extension (positionIndex: PositionIndex)
    private[jacinta] def ints: IArray[Int] = positionIndex

  // Focus value tracked by Jacinta's decoders / encoders. `pointer` is the
  // JSON-pointer path to the current node. `position` is initially `Unset`
  // and filled in by `withPosition(json)` against the root `Json` that
  // produced the focus — typically at error-render time so the success
  // path pays nothing for `locate` lookups. Constructed lazily inside
  // `Foci.supplement`, only for actually-registered errors.
  case class Focus
                ( pointer:  JsonPointer,
                  position: Optional[Json.Ast.Position] = Unset )
  derives CanEqual:

    def withPosition(json: Json): Focus = copy(position = json.locate(pointer))

  opaque type Ast =
    JsonString | JsonNumber | JsonBoolean | JsonNull | JsonObject | JsonArray | Unset.type

  object Ast extends Format:
    def name: Text = "JSON"

    case class Position
      ( line:                Int,
        column:              Int,
        override val offset: Optional[Int] = Unset,
        override val length: Optional[Int] = Unset )
    extends Format.Position:
      def describe: Text = ("line "+line+", column "+column).tt

    enum Issue extends Format.Issue:
      case EmptyInput
      case UnexpectedChar(found: Char)
      case ExpectedTrue
      case ExpectedFalse
      case ExpectedNull
      case ExpectedSomeValue(char: Char)
      case ExpectedColon(found: Char)
      case InvalidWhitespace
      case ExpectedString(found: Char)
      case ExpectedHexDigit(found: Char)
      case PrematureEnd
      case NumberHasLeadingZero
      case SpuriousContent(found: Char)
      case LeadingDecimalPoint
      case NotEscaped(char: Char)
      case IncorrectEscape(char: Char)
      case MultipleDecimalPoints
      case ExpectedDigit(found: Char)

      def describe: Message = this match
        case EmptyInput              => m"the input was empty"
        case UnexpectedChar(found)   => m"the character $found was not expected here"
        case ExpectedTrue            => m"true was expected"
        case ExpectedFalse           => m"false was expected"
        case ExpectedNull            => m"null was expected"
        case ExpectedSomeValue(char) => m"a value was expected but $char was found instead"
        case ExpectedColon(found)    => m"a colon was expected but $found was found instead"
        case InvalidWhitespace       => m"invalid whitespace was found"
        case ExpectedString(found)   => m"a string was expected but $found was found instead"
        case ExpectedHexDigit(found) => m"a hexadecimal digit was expected"
        case PrematureEnd            => m"the content ended prematurely"
        case SpuriousContent(found)  => m"$found was found after the full JSON value was read"
        case LeadingDecimalPoint     => m"a number cannot start with a decimal point"
        case NotEscaped(char)        => m"the character $char must be escaped with a backslash"
        case ExpectedDigit(found)    => m"a digit was expected but $found was found instead"
        case MultipleDecimalPoints   => m"a number cannot contain more than one decimal point"

        case NumberHasLeadingZero =>
          m"a number cannot start with a zero except when followed by a decimal point"

        case IncorrectEscape(char) =>
          m"the character $char was escaped with a backslash unnecessarily"

    object AsciiByte:
      inline final val Tab:          9   = 9   // '\t'
      inline final val Newline:      10  = 10  // '\n'
      inline final val Return:       13  = 13  // '\r'
      inline final val Space:        32  = 32  // ' '
      inline final val Comma:        44  = 44  // ','
      inline final val Quote:        34  = 34  // '"'
      inline final val Minus:        45  = 45  // '-'
      inline final val Plus:         43  = 43  // '+'
      inline final val Slash:        47  = 47  // '/'
      inline final val Period:       46  = 46  // '.'
      inline final val Num0:         48  = 48  //'0'
      inline final val Num1:         49  = 49  //'1'
      inline final val Num2:         50  = 50  //'2'
      inline final val Num3:         51  = 51  //'3'
      inline final val Num4:         52  = 52  //'4'
      inline final val Num5:         53  = 53  //'5'
      inline final val Num6:         54  = 54  //'6'
      inline final val Num7:         55  = 55  //'7'
      inline final val Num8:         56  = 56  //'8'
      inline final val Num9:         57  = 57  //'9'
      inline final val Colon:        58  = 58  // ':'
      inline final val UpperA:       65  = 65  // 'A'
      inline final val UpperB:       66  = 66  // 'B'
      inline final val UpperC:       67  = 67  // 'C'
      inline final val UpperD:       68  = 68  // 'D'
      inline final val UpperE:       69  = 69  // 'E'
      inline final val UpperF:       70  = 70  // 'F'
      inline final val OpenBracket:  91  = 91  // '['
      inline final val CloseBracket: 93  = 93  // ']'
      inline final val Backslash:    92  = 92  // '\\'
      inline final val LowerA:       97  = 97  // 'a'
      inline final val LowerB:       98  = 98  // 'b'
      inline final val LowerC:       99  = 99  // 'c'
      inline final val LowerD:       100 = 100 // 'd'
      inline final val LowerE:       101 = 101 // 'e'
      inline final val LowerF:       102 = 102 // 'f'
      inline final val LowerL:       108 = 108 // 'l'
      inline final val LowerN:       110 = 110 // 'n'
      inline final val LowerR:       114 = 114 // 'r'
      inline final val LowerS:       115 = 115 // 's'
      inline final val LowerT:       116 = 116 // 't'
      inline final val LowerU:       117 = 117 // 'u'
      inline final val OpenBrace:    123 = 123 // '{'
      inline final val CloseBrace:   125 = 125 // '}'

    // Sentinel used to pad a heterogeneous array whose original length is
    // even, so that all such arrays have odd `IArray[Any]` length and can be
    // distinguished from objects (encoded as alternating `key, value, …` and
    // therefore always even length). The sentinel only ever appears as the
    // last element of a padded array and is never part of the user-visible
    // contents. (Pure number arrays use `Array[Double]` and need no
    // padding.)
    val arrayPad: AnyRef = new Object

    def apply
      ( value
      : JsonString | JsonNumber | JsonBoolean | JsonNull | JsonObject | JsonArray | Unset.type )
    :   Ast =

      value

    // Build an object node from parallel `keys` and `values` arrays. The result
    // is stored as a single `IArray[Any]` of length `2 * keys.length`, with keys
    // at even indices and values at odd indices.
    def obj(keys: IArray[String], values: IArray[Any]): Ast =
      val n = keys.length
      val arr = new Array[Any](n*2)
      var i = 0

      while i < n do
        arr(i*2) = keys(i)
        arr(i*2 + 1) = values(i)
        i += 1

      arr.asInstanceOf[IArray[Any]]

    // Build a heterogeneous array node. If the element count is even, a
    // single sentinel `arrayPad` is appended so the stored `IArray[Any]` has
    // odd length, distinguishing it from objects (always even length).
    def arr(elements: IArray[Any]): Ast =
      val n = elements.length

      if (n & 1) == 1 then elements
      else
        val padded = new Array[Any](n + 1)
        System.arraycopy(elements.asInstanceOf[Array[Any]], 0, padded, 0, n)
        padded(n) = arrayPad
        padded.asInstanceOf[IArray[Any]]

    // Build a number-only array node using the single-Long BCD encoding
    // (see `Bcd.packBcdLong`). Each `Long` element carries one number's
    // sign + count + nibbles inline — no per-element `Double` materialisation
    // and no per-element heap allocation.
    def bcdArr(values: Array[Long]): Ast = values

    // Build a number-only array node using the single-Int small-BCD
    // encoding (see `Bcd.packBcdInt`). For arrays where every number
    // fits in 7 nibbles — the half-memory variant of `bcdArr`.
    def smallBcdArr(values: Array[Int]): Ast = values

    // The number of user-visible elements in an array node (excludes the
    // sentinel pad of a parity-padded heterogeneous array, if present).
    def arrayLength(json: Ast): Int = (json: @unchecked) match
      case bcds: Array[Long] @unchecked   => bcds.length
      case smalls: Array[Int] @unchecked  => smalls.length

      case _ =>
        val arr = json.asInstanceOf[Array[?]]
        val n = arr.length
        if n > 0 && (arr(n - 1).asInstanceOf[AnyRef] eq arrayPad) then n - 1 else n

    // The number of key/value pairs in an object node.
    def objectSize(json: Ast): Int = json.asInstanceOf[IArray[Any]].length/2

  def ast(value: Json.Ast): Json = new Json(value)

  // `object Json` extends `Dynamic`, which suppresses the universal-apply
  // synthesis for `Json(...)`; these forward to the constructor manually.
  def apply(value: Any): Json = new Json(value)
  def apply(value: Any, positions: Optional[Json.PositionIndex]): Json = new Json(value, positions)

  // Defined on the companion directly (not as a `Json.type` extension) because
  // the companion's `Dynamic` parentage intercepts `Json.parseTracked(...)`
  // before extension-method resolution gets a chance.
  def parseTracked(source: Data)(using NumberMode): Json raises ParseError =
    val (ast, index) = Json.Ast.parseTracked(source)
    new Json(ast, index)

  def parseTracked(input: Iterator[Data])(using NumberMode): Json raises ParseError =
    val (ast, index) = Json.Ast.parseTracked(input)
    new Json(ast, index)

  def parseTracked(source: Text)(using NumberMode, CharEncoder): Json raises ParseError =
    parseTracked(source.data)

  // Canonical external accessor for the underlying AST. The `root`
  // method on `class Json` is package-private so that breaking through
  // the `Json` abstraction is a deliberate, named action.
  def unseal(json: Json): Json.Ast = json.root


  inline given interpolator: Json is Interpolable:
    type Result = Json

    transparent inline def interpolate[parts <: Tuple, origins <: Tuple]
      ( inline insertions: Any* )
    :   Json =

      ${jacinta.internal.interpolator[parts, origins]('insertions)}


  inline given extrapolator: Json is Extrapolable:
    transparent inline def extrapolate[parts <: Tuple, origins <: Tuple](scrutinee: Json)
    :   Boolean | Option[Tuple | Json] =

      ${jacinta.internal.extractor[parts, origins]('scrutinee)}


  given lens: [name <: Label: ValueOf] => (erased DynamicJsonEnabler) => Tactic[JsonError]
  =>  name is Lens from Json onto Json =

    Lens(_.selectDynamic(valueOf[name]), _.modify(valueOf[name], _))


  given ordinalOptical: [element] => Ordinal is Optical from Json onto Json =
    ordinal =>
      Optic: (origin, lambda) =>
        if origin.root.isArray then
          val n = origin.root.arrayLength

          if n <= ordinal.n0 then origin else Json.ast:
            val updated = new Array[Any](n)
            var i = 0

            while i < n do
              updated(i) =
                if i == ordinal.n0
                then lambda(Json.ast(origin.root.arrayElement(i))).root
                else origin.root.arrayElement(i)

              i += 1

            Json.Ast.arr(updated.asInstanceOf[IArray[Any]])
        else
          origin

  given boolean: Json is Decodable in Json = identity(_)
  given boolean: Tactic[JsonError] => Boolean is Decodable in Json = _.root.boolean
  given double: Tactic[JsonError] => Double is Decodable in Json = _.root.double
  given float: Tactic[JsonError] => Float is Decodable in Json = _.root.double.toFloat
  given long: Tactic[JsonError] => Long is Decodable in Json = _.root.long
  given int: Tactic[JsonError] => Int is Decodable in Json = _.root.long.toInt
  given ordinalDecodable: Tactic[JsonError] => Ordinal is Decodable in Json = _.root.long.toInt.z
  given text: Tactic[JsonError] => Text is Decodable in Json = _.root.string
  given string: Tactic[JsonError] => String is Decodable in Json = _.root.string.s

  given unit: Tactic[JsonError] => Unit is Decodable in Json =
    value =>
      if value.root.isNull then ()
      else
        val reason =
          if value.root.isAbsent then Reason.Absent
          else Reason.NotType(value.root.primitive, JsonPrimitive.Null)

        raise(JsonError(reason))


  given option: [value: Decodable in Json] => Tactic[JsonError]
  =>  Option[value] is Decodable in Json =

    json => if json.root.isAbsent then None else Some(value.decoded(json))


  given optionEncodable: [value] => (encodable: value is Encodable in Json)
  =>  Option[value] is Encodable in Json =

    new Encodable:
      type Self = Option[value]
      type Form = Json

      def encoded(value: Option[value]): Json = value match
        case None        => Json.ast(Json.Ast(Unset))
        case Some(value) => encodable.encode(value)


  given integralEncodable: [integral: Integral] => integral is Encodable in Json =
    int => Json.ast(Json.Ast(integral.toLong(int)))

  given textEncodableInJson: Text is Encodable in Json = text => Json.ast(Json.Ast(text.s))
  given stringEncodable: String is Encodable in Json = string => Json.ast(Json.Ast(string))
  given doubleEncodable: Double is Encodable in Json = double => Json.ast(Json.Ast(double))
  given intEncodable: Int is Encodable in Json = int => Json.ast(Json.Ast(int.toLong))
  given unitEncodable: Unit is Encodable in Json = unit => Json.ast(Json.Ast(null))

  given ordinalEncodable: Ordinal is Encodable in Json =
    ordinal => Json.ast(Json.Ast(ordinal.n0.toLong))

  given longEncodable: Long is Encodable in Json = long => Json.ast(Json.Ast(long))
  given booleanEncodable: Boolean is Encodable in Json = boolean => Json.ast(Json.Ast(boolean))
  given jsonEncodable: Json is Encodable in Json = identity(_)


  given listEncodable: [list <: List, element] => (encodable: => element is Encodable in Json)
  =>  list[element] is Encodable in Json =

    values => Json.ast(Json.Ast.arr(IArray.from(values.map(encodable.encoded(_).root))))


  given setEncodable: [set <: Set, element] => (encodable: => element is Encodable in Json)
  =>  set[element] is Encodable in Json =

    values => Json.ast(Json.Ast.arr(IArray.from(values.map(encodable.encoded(_).root))))


  given trieEncodable: [trie <: Trie, element] => (encodable: => element is Encodable in Json)
  =>  trie[element] is Encodable in Json =

    values => Json.ast(Json.Ast.arr(IArray.from(values.map(encodable.encoded(_).root))))


  given array: [collection <: Iterable, element]
  =>  ( factory: Factory[element, collection[element]],
        tactic:  Tactic[JsonError],
        foci:    Foci[Json.Focus] )
  =>  ( decodable: => element is Decodable in Json )
  =>  collection[element] is Decodable in Json =

    value =>
      val builder = factory.newBuilder

      value.root.array.each: json =>
        focus({
          val base = prior.let(_.pointer).or(JsonPointer())

          val newPointer =
            JsonPointer
              ( base.url,
                Path[JsonPointer, JsonPointer.type, Tuple]
                  ( base.path.root, base.path.descent :+ ordinal.n0.toString.tt ) )

          Json.Focus(newPointer)
        }):
          builder += decodable.decoded(Json.ast(json))

      builder.result()


  given map: [key: Decodable in Text, element] => (decodable: => element is Decodable in Json)
  =>  Tactic[JsonError]
  =>  Map[key, element] is Decodable in Json =

    value =>
      val root = value.root
      val n = root.objectSize
      var i = 0
      var acc = Map.empty[key, element]

      while i < n do
        acc =
          acc.updated
            ( root.objectKey(i).tt.decode,
              decodable.decoded(Json.ast(root.objectValue(i))) )

        i += 1

      acc


  given mapEncodable: [key: Encodable in Text, element]
  =>  ( encodable: element is Encodable in Json )
  =>  Map[key, element] is Encodable in Json =

    map =>
      val keys: List[key] = map.keys.to(List)
      val values = IArray.from(keys.map(map(_).encode.root))
      Json.ast(Json.Ast.obj(IArray.from(keys.map(_.encode.s)), values))


  given jsonEncodableInText: Json is Encodable in Text = json => JsonPrinter.print(json.root, false)

  given aggregable: Tactic[ParseError] => Json is Aggregable by Data =
    bytes => Json(bytes.read[Json.Ast])


  given aggregableDirect: [value: Decodable in Json] => Tactic[ParseError] => Tactic[JsonError]
  =>  (value over Json) is Aggregable by Data =

    bytes => Json(bytes.read[Json.Ast]).as[value].asInstanceOf[value over Json]


  given showable: JsonPrinter => Json is Showable = _.root.show


  given abstractable: (encoder: CharEncoder, printer: JsonPrinter)
  =>  Json is Abstractable across HttpStreams to HttpStreams.Content =

    new Abstractable:
      type Self = Json
      type Domain = HttpStreams
      type Result = HttpStreams.Content

      def genericize(json: Json): HttpStreams.Content =
        (t"application/json; charset=${encoder.encoding.name}", Stream(json.show.data))


  given decodable: Tactic[ParseError] => Json is Decodable in Text =
    text => Stream(text.data(using charEncoders.utf8)).read[Json]

  given instantiable: Tactic[ParseError] => Json is Instantiable across HttpRequests from Text =
    text => Stream(text.data(using charEncoders.utf8)).read[Json]

  def applyDynamicNamed(methodName: "make")(elements: (String, Json)*): Json =
    val keys: IArray[String] = IArray.from(elements.map(_(0)))
    val values: IArray[Json.Ast] = IArray.from(elements.map(_(1).root))
    Json(Json.Ast.obj(keys, values.asInstanceOf[IArray[Any]]))

  def discriminatedUnion[value](label: Text): value is Discriminable in Json = new Discriminable:
    type Form = Json
    type Self = value

    protected def key: String = label.s

    import dynamicJsonAccess.enabled

    def rewrite(kind: Text, json: Json): Json = unsafely(json.updateDynamic(key)(kind))
    def discriminate(json: Json): Optional[Text] = safely(json.selectDynamic(key).as[Text])
    def variant(json: Json): Json = unsafely(json.updateDynamic(key)(Unset))

class Json(rootValue: Any, positions: Optional[Json.PositionIndex] = Unset)
extends Dynamic derives CanEqual:
  private[jacinta] def root: Json.Ast = rootValue.asInstanceOf[Json.Ast]
  def positionIndex: Optional[Json.PositionIndex] = positions
  def apply(index: Int): Json raises JsonError = Json(root.array(index))

  def selectDynamic(field: String)(using erased DynamicJsonEnabler): Json raises JsonError =
    apply(field.tt)


  def applyDynamic(field: String)(index: Int)(using erased DynamicJsonEnabler)
  :   Json raises JsonError =

    apply(field.tt)(index)


  def update[value: Encodable in Json](index: Int, value: value)(using erased DynamicJsonEnabler)
  :   Json raises JsonError =

    if !root.isArray then raise(JsonError(Reason.NotType(root.primitive, JsonPrimitive.Array)))
    val n = root.arrayLength
    val updated = new Array[Any](n)
    var i = 0

    while i < n do
      updated(i) =
        if i == index then value.encode.root
        else root.arrayElement(i)

      i += 1

    Json.ast(Json.Ast.arr(updated.asInstanceOf[IArray[Any]]))


  def updateDynamic(field: String)[value: Encodable in Json](value: value)
    ( using erased DynamicJsonEnabler )
  :   Json raises JsonError =

    modify(field, value.encode)


  def updateDynamic(field: String)[value](unset: Unset.type)(using erased DynamicJsonEnabler)
  :   Json raises JsonError =

    delete(field)


  private[jacinta] def modify(field: String, value: Json): Json raises JsonError =
    val arr = root.asInstanceOf[IArray[Any]]
    val len = arr.length
    val n = len/2

    root.objectIndexOf(field) match
      case -1 =>
        val out = new Array[Any](len + 2)
        System.arraycopy(arr.asInstanceOf[Array[Any]], 0, out, 0, len)
        out(len) = field
        out(len + 1) = value.root
        Json.ast(Json.Ast(out.asInstanceOf[IArray[Any]]))

      case index =>
        val out = new Array[Any](len)
        System.arraycopy(arr.asInstanceOf[Array[Any]], 0, out, 0, len)
        out(index*2 + 1) = value.root
        Json.ast(Json.Ast(out.asInstanceOf[IArray[Any]]))

  private[jacinta] def delete(field: String): Json raises JsonError =
    val arr = root.asInstanceOf[IArray[Any]]
    val len = arr.length

    root.objectIndexOf(field) match
      case -1 =>
        Json.ast(root)

      case index =>
        val out = new Array[Any](len - 2)
        System.arraycopy(arr.asInstanceOf[Array[Any]], 0, out, 0, index*2)

        System.arraycopy
          ( arr.asInstanceOf[Array[Any]],
            index*2 + 2,
            out,
            index*2,
            len - index*2 - 2 )

        Json.ast(Json.Ast(out.asInstanceOf[IArray[Any]]))

  def apply(field: Text): Json raises JsonError =
    if root.isAbsent then Json.ast(Json.Ast(Unset))
    else root.objectIndexOf(field.s) match
      case -1    => Json.ast(Json.Ast(Unset))
      case index => Json(root.objectValue(index))

  override def hashCode: Int =
    def recur(value: Json.Ast): Int = value.asMatchable match
      case value: Long       => value.hashCode
      case value: Double     => value.hashCode
      case value: String     => value.hashCode
      case value: Boolean    => value.hashCode

      case value: Int        =>
        // Small-BCD number — hash through the BigDecimal projection so
        // it stays consistent with the `Bcd` / `Long` / `Double` paths.
        BigDecimal(Bcd.bcdIntText(value)).hashCode

      case value: Array[Long] @unchecked =>
        // BCD-packed number array — same recursion as `Array[Double]` so
        // arrays of equal values hash identically regardless of which
        // backing representation the parser picked.
        val ast = value.asInstanceOf[Json.Ast]
        val n = value.length
        var acc = n.hashCode
        var i = 0

        while i < n do
          acc = acc*31 ^ recur(ast.arrayElement(i))
          i += 1

        acc

      case value: Array[Double] @unchecked =>
        // High-precision number (`Bcd`) — hash via the BigDecimal
        // projection so a Bcd whose value equals a BigDecimal literal has
        // a consistent hash.
        value.asInstanceOf[Bcd].toBigDecimal.hashCode

      case value: Array[Int] @unchecked =>
        // Number array in single-Int small-BCD form — recurse per element
        // for cross-form equality with the boxed/Double/Long array shapes.
        val ast = value.asInstanceOf[Json.Ast]
        val n = value.length
        var acc = n.hashCode
        var i = 0

        while i < n do
          acc = acc*31 ^ recur(ast.arrayElement(i))
          i += 1

        acc

      case value: IArray[Any] @unchecked =>
        // Heterogeneous array or object, distinguished by parity.
        val ast = value.asInstanceOf[Json.Ast]

        if ast.isObject then
          val n = ast.objectSize
          var acc = Map.empty[String, Int]
          var i = 0

          while i < n do
            acc = acc.updated(ast.objectKey(i), recur(ast.objectValue(i)))
            i += 1

          acc.hashCode
        else
          val n = ast.arrayLength
          var acc = n.hashCode
          var i = 0

          while i < n do
            acc = acc*31 ^ recur(ast.arrayElement(i))
            i += 1

          acc

      case _ =>
        0

    recur(root)

  override def equals(right: Any): Boolean = right.asMatchable match
    case right: Json =>
      def arrayEq(leftAst: Json.Ast, rightAst: Json.Ast): Boolean =
        val rn = rightAst.arrayLength
        val ln = leftAst.arrayLength

        rn == ln &&
          { var i = 0
            var eq = true

            while i < rn && eq do
              if !recur(leftAst.arrayElement(i), rightAst.arrayElement(i)) then eq = false
              i += 1

            eq }

      def objectEq(leftAst: Json.Ast, rightAst: Json.Ast): Boolean =
        val rn = rightAst.objectSize
        val ln = leftAst.objectSize

        if rn != ln then false
        else
          var leftMap = Map.empty[String, Json.Ast]
          var i = 0

          while i < ln do
            leftMap = leftMap.updated(leftAst.objectKey(i), leftAst.objectValue(i))
            i += 1

          var rightMap = Map.empty[String, Json.Ast]
          i = 0

          while i < rn do
            rightMap = rightMap.updated(rightAst.objectKey(i), rightAst.objectValue(i))
            i += 1

          leftMap.keySet == rightMap.keySet && leftMap.keySet.forall: key =>
            recur(leftMap(key), rightMap(key))

      def recur(left: Json.Ast, right: Json.Ast): Boolean = right.asMatchable match
        case right: Long => left.asMatchable match
          case left: Long                    => left == right
          case left: Int                     => left == right.toInt && left.toLong == right
          case left: Double                  => left == right

          case left: Array[Double] @unchecked =>
            left.asInstanceOf[Bcd].toBigDecimal == BigDecimal(right)

          case _                             => false

        case right: Int => left.asMatchable match
          case left: Int                     => left == right
          case left: Long                    => BigDecimal(Bcd.bcdIntText(right)) == BigDecimal(left)
          case left: Double                  => BigDecimal(Bcd.bcdIntText(right)) == BigDecimal(left)

          case left: Array[Double] @unchecked =>
            left.asInstanceOf[Bcd].toBigDecimal == BigDecimal(Bcd.bcdIntText(right))

          case _                             => false

        case right: Double => left.asMatchable match
          case left: Long                    => left == right
          case left: Int                     => BigDecimal(Bcd.bcdIntText(left)) == BigDecimal(right)
          case left: Double                  => left == right

          case left: Array[Double] @unchecked =>
            left.asInstanceOf[Bcd].toBigDecimal == BigDecimal(right)

          case _                             => false

        case right: String => left.asMatchable match
          case left: String => left == right
          case _            => false

        case right: Boolean => left.asMatchable match
          case left: Boolean => left == right
          case _             => false

        case right: Array[Long] @unchecked =>
          // BCD-Long-packed number array.
          val rightAst = right.asInstanceOf[Json.Ast]

          left.asMatchable match
            case _: Array[Long] @unchecked => arrayEq(left, rightAst)
            case _: Array[Int] @unchecked  => arrayEq(left, rightAst)

            case _: Array[AnyRef] @unchecked if left.asInstanceOf[Json.Ast].isArray =>
              arrayEq(left, rightAst)

            case _ => false

        case right: Array[Double] @unchecked =>
          // High-precision number (`Bcd`).
          val rb = right.asInstanceOf[Bcd]

          left.asMatchable match
            case left: Long                    => BigDecimal(left) == rb.toBigDecimal
            case left: Int                     => BigDecimal(Bcd.bcdIntText(left)) == rb.toBigDecimal
            case left: Double                  => BigDecimal(left) == rb.toBigDecimal

            case left: Array[Double] @unchecked =>
              left.asInstanceOf[Bcd].toBigDecimal == rb.toBigDecimal

            case _                             => false

        case right: Array[Int] @unchecked =>
          // Number array in single-Int small-BCD form.
          val rightAst = right.asInstanceOf[Json.Ast]

          left.asMatchable match
            case _: Array[Int] @unchecked  => arrayEq(left, rightAst)
            case _: Array[Long] @unchecked => arrayEq(left, rightAst)

            case _: Array[AnyRef] @unchecked if left.asInstanceOf[Json.Ast].isArray =>
              arrayEq(left, rightAst)

            case _ => false

        case right: IArray[Any] @unchecked =>
          // Heterogeneous array or object, distinguished by parity.
          val rightAst = right.asInstanceOf[Json.Ast]
          val rightIsObject = rightAst.isObject

          left.asMatchable match
            case _: Array[AnyRef] @unchecked =>
              val leftAst = left.asInstanceOf[Json.Ast]

              if rightIsObject then
                if leftAst.isObject then objectEq(leftAst, rightAst) else false
              else if leftAst.isArray then
                arrayEq(leftAst, rightAst)
              else
                false

            case _: Array[Long] @unchecked if !rightIsObject =>
              arrayEq(left, rightAst)

            case _: Array[Int] @unchecked if !rightIsObject =>
              arrayEq(left, rightAst)

            case _ => false

        case _ =>
          false

      recur(root, right.root)

    case _ =>
      false

  def as[value: Decodable in Json at Json.Focus]
  :   value raises JsonError tracks Json.Focus =

    val decodable = summon[value is Decodable in Json at Json.Focus]
    val result = decodable.decoded(this)
    val foci = summon[Foci[Json.Focus]]
    foci.supplement(foci.length, _.let(decodable.position(this, _)).vouch)
    result
