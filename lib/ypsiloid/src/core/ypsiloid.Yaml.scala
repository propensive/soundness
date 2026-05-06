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

import language.dynamics

import scala.collection.Factory
import scala.collection.mutable as scm
import scala.compiletime.*

import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import gossamer.*
import panopticon.*
import prepositional.*
import proscenium.*
import rudiments.*
import symbolism.*
import turbulence.*
import vacuous.*
import wisteria.*
import zephyrine.*

import YamlError.Reason

trait Yaml2:
  given optionalEncodable: [inner <: value, value >: Unset.type: Mandatable to inner]
  =>  ( encodable: inner is Encodable in Yaml )
  =>  value is Encodable in Yaml =

    new Encodable:
      type Self = Optional[value]
      type Form = Yaml

      def encoded(value: Optional[value]): Yaml =
        value.let(_.asInstanceOf[inner]).let(encodable.encode(_))
        . or(Yaml.ast(YamlAst(Unset)))


  given optional: [inner <: value, value >: Unset.type: Mandatable to inner] => Tactic[YamlError]
  =>  ( decodable: => inner is Decodable in Yaml )
  =>  value is Decodable in Yaml = yaml =>
      if yaml.root.asInstanceOf[AnyRef] eq Unset then Unset else decodable.decoded(yaml)


  inline given decodable: [value] => value is Decodable in Yaml = summonFrom:
    case given (`value` is Decodable in Text) =>
      yaml =>
        provide[Tactic[YamlError]]:
          yaml.root.asMatchable match
            case s: String => s.tt.decode[value]
            case _         =>
              raise(YamlError(Reason.NotType(Yaml.primitive(yaml.root), YamlPrimitive.Str)))
              .yet(t"".decode[value])

    case given Reflection[`value`] =>
      DecodableDerivation.derived

  inline given encodable: [value] => value is Encodable in Yaml = summonFrom:
    case given (`value` is Encodable in Text) => value => Yaml.ast(YamlAst(value.encode.s))
    case given Reflection[`value`]            => EncodableDerivation.derived

  object DecodableDerivation extends Derivable[Decodable in Yaml]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Decodable in Yaml = yaml =>
      provide[Foci[YamlPath]]:
        provide[Tactic[YamlError]]:
          val arr: IArray[Any] | Null = yaml.root.asMatchable match
            case xs: IArray[?] @unchecked if (xs.length & 1) == 0 =>
              xs.asInstanceOf[IArray[Any]]
            case _ => null

          build: [field] =>
            context =>
              val target = label.s
              var found: YamlAst | Null = null
              if arr != null then
                val n = arr.length
                var i = 0
                while i < n && found == null do
                  val key = arr(i).asInstanceOf[YamlAst]
                  key.asMatchable match
                    case s: String if s == target => found = arr(i + 1).asInstanceOf[YamlAst]
                    case _                        => ()
                  i += 2
              focus(prior.or(YamlPath()) / label):
                if found != null then context.decoded(new Yaml(found))
                else
                  default.or(context.decoded(new Yaml(YamlAst.Null)))

    inline def disjunction[derivation: SumReflection]: derivation is Decodable in Yaml = yaml =>
      provide[Foci[YamlPath]]:
        provide[Tactic[YamlError]]:
          provide[Tactic[VariantError]]:
            val discriminable = infer[derivation is Discriminable in Yaml]

            val discriminant: Text = discriminable.discriminate(yaml).or:
              focus(prior.or(YamlPath()))(abort(YamlError(Reason.Absent)))

            delegate(discriminant): [variant <: derivation] =>
              context => context.decoded(discriminable.variant(yaml))

  object EncodableDerivation extends Derivable[Encodable in Yaml]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Encodable in Yaml = value =>
      provide[Foci[YamlPath]]:
        val entries = scm.ArrayBuffer.empty[Any]
        fields(value): [field] =>
          field => focus(prior.or(YamlPath()) / label):
            val encoded = contextual.encode(field).root
            if !(encoded.asInstanceOf[AnyRef] eq Unset) then
              entries += YamlAst.Str(label).asInstanceOf[Any]
              entries += encoded.asInstanceOf[Any]
        Yaml.ast(YamlAst.mapFromAnyArray(entries.toArray))

    inline def disjunction[derivation: SumReflection]: derivation is Encodable in Yaml = value =>
      val discriminable = infer[derivation is Discriminable in Yaml]
      variant(value): [variant <: derivation] =>
        value => discriminable.rewrite(label, contextual.encode(value))

object Yaml extends Yaml2, Dynamic:
  def ast(value: YamlAst): Yaml = new Yaml(value)

  // Canonical external accessor for the underlying AST. The `root`
  // field on `class Yaml` is package-private so that breaking through
  // the `Yaml` abstraction is a deliberate, named action.
  def unseal(yaml: Yaml): YamlAst = yaml.root

  // Named-parameter construction: `Yaml.make(name = …, age = …)`
  // desugars to `applyDynamicNamed("make")(("name", …), ("age", …))`.
  // Mirrors `Json.applyDynamicNamed`.
  def applyDynamicNamed(methodName: "make")(elements: (String, Yaml)*): Yaml =
    val arr = new Array[Any](elements.length*2)
    var i = 0
    while i < elements.length do
      arr(i*2)     = YamlAst.Str(elements(i)(0).tt).asInstanceOf[Any]
      arr(i*2 + 1) = elements(i)(1).root.asInstanceOf[Any]
      i += 1
    Yaml.ast(YamlAst.mapFromAnyArray(arr))

  given yaml: Yaml is Decodable in Yaml = identity(_)
  given yamlEncodable: Yaml is Encodable in Yaml = identity(_)

  given bytes: Tactic[YamlError] => Bytes is Decodable in Yaml = _.root.long.b

  given lens: [name <: Label: ValueOf] => (erased DynamicYamlEnabler) => Tactic[YamlError]
  =>  name is Lens from Yaml onto Yaml =
    Lens(_.selectDynamic(valueOf[name]), _.modify(valueOf[name], _))

  given ordinalOptical: [element] => Ordinal is Optical from Yaml onto Yaml = ordinal =>
    Optic: (origin, lambda) =>
      if origin.root.isArray then
        val n = origin.root.arrayLength
        if n <= ordinal.n0 then origin else Yaml.ast:
          val updated = new Array[Any](n)
          var i = 0
          while i < n do
            updated(i) =
              if i == ordinal.n0
              then lambda(Yaml.ast(origin.root.arrayElement(i))).root
              else origin.root.arrayElement(i)
            i += 1
          YamlAst.seqFromAnyArray(updated)
      else origin

  private inline def typeMismatch[T]
      (yaml: Yaml, expected: YamlPrimitive, default: T)
      (using Tactic[YamlError])
  :   T =
    raise(YamlError(Reason.NotType(primitive(yaml.root), expected))) yet default

  given int: Tactic[YamlError] => Int is Decodable in Yaml = yaml =>
    yaml.root.asMatchable match
      case n: Long   => n.toInt
      case d: Double => d.toInt
      case _         => typeMismatch(yaml, YamlPrimitive.Integer, 0)

  given long: Tactic[YamlError] => Long is Decodable in Yaml = yaml =>
    yaml.root.asMatchable match
      case n: Long   => n
      case d: Double => d.toLong
      case _         => typeMismatch(yaml, YamlPrimitive.Integer, 0L)

  given double: Tactic[YamlError] => Double is Decodable in Yaml = yaml =>
    yaml.root.asMatchable match
      case d: Double => d
      case n: Long   => n.toDouble
      case _         => typeMismatch(yaml, YamlPrimitive.Decimal, 0.0)

  given float: Tactic[YamlError] => Float is Decodable in Yaml = yaml =>
    yaml.root.asMatchable match
      case d: Double => d.toFloat
      case n: Long   => n.toFloat
      case _         => typeMismatch(yaml, YamlPrimitive.Decimal, 0.0f)

  given boolean: Tactic[YamlError] => Boolean is Decodable in Yaml = yaml =>
    yaml.root.asMatchable match
      case b: Boolean => b
      case _          => typeMismatch(yaml, YamlPrimitive.Bool, false)

  given text: Tactic[YamlError] => Text is Decodable in Yaml = yaml =>
    yaml.root.asMatchable match
      case s: String => s.tt
      case _         => typeMismatch(yaml, YamlPrimitive.Str, t"")

  given string: Tactic[YamlError] => String is Decodable in Yaml = yaml =>
    yaml.root.asMatchable match
      case s: String => s
      case _         => typeMismatch(yaml, YamlPrimitive.Str, "")

  given unit: Tactic[YamlError] => Unit is Decodable in Yaml = yaml =>
    if yaml.root.asInstanceOf[AnyRef] == null then ()
    else typeMismatch(yaml, YamlPrimitive.Null, ())

  given iterable: [collection <: Iterable, element]
  =>  ( factory:   Factory[element, collection[element]],
        tactic:    Tactic[YamlError],
        foci:      Foci[YamlPath] )
  =>  ( decodable: => element is Decodable in Yaml )
  =>  collection[element] is Decodable in Yaml = yaml =>
    yaml.root.asMatchable match
      case xs: IArray[?] @unchecked if (xs.length & 1) == 1 =>
        // Sequence (odd length, possibly with a trailing pad sentinel).
        val n = xs.length
        val effective =
          if n > 0 && (xs(n - 1).asInstanceOf[AnyRef] eq YamlAst.arrayPad) then n - 1
          else n
        val builder = factory.newBuilder
        var i = 0
        while i < effective do
          val ordinal = denominative.Ordinal.zerary(i)
          focus(prior.or(YamlPath()) / ordinal):
            builder += decodable.decoded(new Yaml(xs(i).asInstanceOf[YamlAst]))
          i += 1
        builder.result()

      case other =>
        raise(YamlError(Reason.NotType(primitive(other.asInstanceOf[YamlAst]),
                                       YamlPrimitive.Sequence)))
        factory.newBuilder.result()

  given map: [value: Decodable in Yaml] => Tactic[YamlError]
  =>  Map[Text, value] is Decodable in Yaml = yaml =>
    yaml.root.asMatchable match
      case xs: IArray[?] @unchecked if (xs.length & 1) == 0 =>
        // Mapping (even length, alternating keys and values flat).
        val n = xs.length / 2
        var result = Map.empty[Text, value]
        var i = 0
        while i < n do
          val rawKey = xs(i*2).asInstanceOf[YamlAst]
          val rawValue = xs(i*2 + 1).asInstanceOf[YamlAst]
          val keyText: Text =
            if rawKey.asInstanceOf[AnyRef] == null then t"null"
            else rawKey.asMatchable match
              case s: String  => s.tt
              case k: Long    => k.toString.tt
              case k: Double  => k.toString.tt
              case k: Boolean => k.toString.tt

              case other =>
                raise(YamlError(Reason.NotType(primitive(other.asInstanceOf[YamlAst]),
                                               YamlPrimitive.Str))) yet t""

          result = result.updated(keyText, value.decoded(new Yaml(rawValue)))
          i += 1
        result

      case other =>
        raise(YamlError(Reason.NotType(primitive(other.asInstanceOf[YamlAst]),
                                       YamlPrimitive.Mapping)))
        Map.empty

  given option: [value: Decodable in Yaml] => Option[value] is Decodable in Yaml = yaml =>
    if yaml.root.isAbsent || yaml.root.asInstanceOf[AnyRef] == null then None
    else Some(value.decoded(yaml))

  // ── Encodable givens ────────────────────────────────────────────────────

  given optionEncodable: [value] => (encodable: value is Encodable in Yaml)
  =>  Option[value] is Encodable in Yaml =

    new Encodable:
      type Self = Option[value]
      type Form = Yaml

      def encoded(value: Option[value]): Yaml = value match
        case None        => Yaml.ast(YamlAst(Unset))
        case Some(value) => encodable.encode(value)


  given integralEncodable: [integral: Integral] => integral is Encodable in Yaml =
    int => Yaml.ast(YamlAst(integral.toLong(int)))

  given textEncodable: Text is Encodable in Yaml = text => Yaml.ast(YamlAst(text.s))
  given stringEncodable: String is Encodable in Yaml = string => Yaml.ast(YamlAst(string))
  given doubleEncodable: Double is Encodable in Yaml = double => Yaml.ast(YamlAst(double))
  given floatEncodable: Float is Encodable in Yaml = float => Yaml.ast(YamlAst(float.toDouble))
  given intEncodable: Int is Encodable in Yaml = int => Yaml.ast(YamlAst(int.toLong))
  given longEncodable: Long is Encodable in Yaml = long => Yaml.ast(YamlAst(long))
  given booleanEncodable: Boolean is Encodable in Yaml = boolean => Yaml.ast(YamlAst(boolean))
  given unitEncodable: Unit is Encodable in Yaml = _ => Yaml.ast(YamlAst.Null)


  given iterableEncodable: [collection <: Iterable, element]
  =>  ( encodable: => element is Encodable in Yaml )
  =>  collection[element] is Encodable in Yaml = values =>
    val items = IArray.from(values.map(encodable.encode(_).root))
    Yaml.ast(YamlAst.Sequence(items))


  given mapEncodable: [key: Encodable in Text, element]
  =>  ( encodable: element is Encodable in Yaml )
  =>  Map[key, element] is Encodable in Yaml = map =>
    val keys: List[key] = map.keys.to(List)
    val arr = new Array[Any](keys.size*2)
    var i = 0
    keys.foreach: k =>
      arr(i*2) = YamlAst.Str(k.encode).asInstanceOf[Any]
      arr(i*2 + 1) = encodable.encode(map(k)).root.asInstanceOf[Any]
      i += 1
    Yaml.ast(YamlAst.mapFromAnyArray(arr))


  // ── Discriminator support for sum-type derivation ───────────────────────

  // Build a `Discriminable` for `Yaml` keyed on a single mapping field
  // named `label`. `discriminate` looks the field up in the YAML mapping;
  // `rewrite` adds (or replaces) the field with the variant's name; and
  // `variant` strips the field for variant decoding.
  def discriminatedUnion[value](label: Text): value is Discriminable in Yaml =
    new Discriminable:
      type Form = Yaml
      type Self = value

      def discriminate(yaml: Yaml): Optional[Text] =
        yaml.root.asMatchable match
          case xs: IArray[?] @unchecked if (xs.length & 1) == 0 =>
            var i = 0
            var result: Optional[Text] = Unset
            while i < xs.length && result.absent do
              xs(i).asMatchable match
                case s: String if s == label.s =>
                  xs(i + 1).asMatchable match
                    case v: String => result = v.tt
                    case _         => ()
                case _ => ()
              i += 2
            result

          case _ => Unset

      def rewrite(kind: Text, yaml: Yaml): Yaml =
        yaml.root.asMatchable match
          case xs: IArray[?] @unchecked if (xs.length & 1) == 0 =>
            // Replace existing entry if present, else append.
            var existing = -1
            var i = 0
            while i < xs.length && existing < 0 do
              xs(i).asMatchable match
                case s: String if s == label.s => existing = i
                case _                         => ()
              i += 2
            val out =
              if existing >= 0 then
                val arr = new Array[Any](xs.length)
                System.arraycopy(xs.asInstanceOf[Array[Any]], 0, arr, 0, xs.length)
                arr(existing + 1) = YamlAst.Str(kind).asInstanceOf[Any]
                arr
              else
                val arr = new Array[Any](xs.length + 2)
                System.arraycopy(xs.asInstanceOf[Array[Any]], 0, arr, 0, xs.length)
                arr(xs.length)     = YamlAst.Str(label).asInstanceOf[Any]
                arr(xs.length + 1) = YamlAst.Str(kind).asInstanceOf[Any]
                arr
            Yaml.ast(YamlAst.mapFromAnyArray(out))

          case _ =>
            // Not a mapping — wrap in a one-entry mapping.
            val arr = Array[Any]
                       ( YamlAst.Str(label).asInstanceOf[Any],
                         YamlAst.Str(kind).asInstanceOf[Any] )
            Yaml.ast(YamlAst.mapFromAnyArray(arr))

      def variant(yaml: Yaml): Yaml =
        yaml.root.asMatchable match
          case xs: IArray[?] @unchecked if (xs.length & 1) == 0 =>
            var existing = -1
            var i = 0
            while i < xs.length && existing < 0 do
              xs(i).asMatchable match
                case s: String if s == label.s => existing = i
                case _                         => ()
              i += 2
            if existing < 0 then yaml
            else
              val arr = new Array[Any](xs.length - 2)
              System.arraycopy(xs.asInstanceOf[Array[Any]], 0, arr, 0, existing)
              System.arraycopy
                ( xs.asInstanceOf[Array[Any]],
                  existing + 2,
                  arr,
                  existing,
                  xs.length - existing - 2 )
              Yaml.ast(YamlAst.mapFromAnyArray(arr))

          case _ => yaml


  // ── Parser entry-points ─────────────────────────────────────────────────

  given decodable: Tactic[ParseError] => Yaml is Decodable in Text =
    text => Yaml(YamlParser.parse(text))

  def parseAll(input: Text)(using Tactic[ParseError]): List[Yaml] =
    YamlParser.parseAll(input).map(Yaml(_))

  given aggregable: Tactic[ParseError] => Yaml is Aggregable by Text =
    summon[Text is Aggregable by Text].map(text => Yaml(YamlParser.parse(text)))

  def primitive(ast: YamlAst): YamlPrimitive =
    if ast.asInstanceOf[AnyRef] == null then YamlPrimitive.Null
    else ast.asMatchable match
      case _: Boolean    => YamlPrimitive.Bool
      case _: Long       => YamlPrimitive.Integer
      case _: Double     => YamlPrimitive.Decimal
      case _: String     => YamlPrimitive.Str

      case xs: IArray[?] @unchecked =>
        if (xs.length & 1) == 0 then YamlPrimitive.Mapping else YamlPrimitive.Sequence

      case _ => YamlPrimitive.Null

class Yaml(private[ypsiloid] val root: YamlAst) extends Dynamic derives CanEqual:
  def as[value: Decodable in Yaml]: value raises YamlError tracks YamlPath =
    value.decoded(this)

  // Sequence indexing: `yaml(0)` returns the first element of a sequence,
  // raising `YamlError` (with `Reason.NotType`) if the root is not a
  // sequence.
  def apply(index: Int): Yaml raises YamlError =
    if root.isArray then new Yaml(root.arrayElement(index))
    else
      raise(YamlError(Reason.NotType(Yaml.primitive(root), YamlPrimitive.Sequence)))
      new Yaml(YamlAst.Null)

  // Mapping field access by name: `yaml(t"foo")` returns the value
  // associated with key `foo`, or `Unset` (encoded as `YamlAst(Unset)`)
  // when the field is absent. Mirrors Jacinta's `Json.apply(field)`.
  def apply(field: Text): Yaml =
    if root.isAbsent then new Yaml(YamlAst(Unset))
    else root.objectIndexOf(field.s) match
      case -1    => new Yaml(YamlAst(Unset))
      case index => new Yaml(root.objectValue(index))

  // Dynamic field access — `yaml.foo` desugars to `selectDynamic("foo")`.
  // Gated on an erased `DynamicYamlEnabler` so the feature is opt-in via
  // `import dynamicYamlAccess.enabled`.
  def selectDynamic(field: String)(using erased DynamicYamlEnabler): Yaml = apply(field.tt)

  def applyDynamic(field: String)(index: Int)(using erased DynamicYamlEnabler)
  :   Yaml raises YamlError =
    apply(field.tt)(index)

  // Immutable update: `yaml(0) = newValue` desugars to `update(0, newValue)`.
  def update[value: Encodable in Yaml](index: Int, value: value)
                ( using erased DynamicYamlEnabler )
  :   Yaml raises YamlError =
    if !root.isArray then
      raise(YamlError(Reason.NotType(Yaml.primitive(root), YamlPrimitive.Sequence)))
    val n = root.arrayLength
    val updated = new Array[Any](n)
    var i = 0
    while i < n do
      updated(i) =
        if i == index then value.encode.root.asInstanceOf[Any]
        else root.arrayElement(i).asInstanceOf[Any]
      i += 1
    Yaml.ast(YamlAst.seqFromAnyArray(updated))

  // `yaml.foo = newValue` — replaces `foo` if present, or appends a new
  // entry. `yaml.foo = Unset` deletes the entry.
  def updateDynamic(field: String)[value: Encodable in Yaml](value: value)
                ( using erased DynamicYamlEnabler )
  :   Yaml raises YamlError =
    modify(field, value.encode)

  def updateDynamic(field: String)[value](unset: Unset.type)(using erased DynamicYamlEnabler)
  :   Yaml raises YamlError =
    delete(field)

  // ── Internal mapping update helpers ─────────────────────────────────────

  private[ypsiloid] def modify(field: String, value: Yaml): Yaml raises YamlError =
    if !root.isObject then
      raise(YamlError(Reason.NotType(Yaml.primitive(root), YamlPrimitive.Mapping)))
      this
    else
      val arr = root.asInstanceOf[IArray[Any]]
      val len = arr.length
      root.objectIndexOf(field) match
        case -1 =>
          val out = new Array[Any](len + 2)
          System.arraycopy(arr.asInstanceOf[Array[Any]], 0, out, 0, len)
          out(len)     = field
          out(len + 1) = value.root.asInstanceOf[Any]
          Yaml.ast(YamlAst.mapFromAnyArray(out))
        case index =>
          val out = new Array[Any](len)
          System.arraycopy(arr.asInstanceOf[Array[Any]], 0, out, 0, len)
          out(index*2 + 1) = value.root.asInstanceOf[Any]
          Yaml.ast(YamlAst.mapFromAnyArray(out))

  private[ypsiloid] def delete(field: String): Yaml raises YamlError =
    if !root.isObject then
      raise(YamlError(Reason.NotType(Yaml.primitive(root), YamlPrimitive.Mapping)))
      this
    else
      val arr = root.asInstanceOf[IArray[Any]]
      val len = arr.length
      root.objectIndexOf(field) match
        case -1 => this
        case index =>
          val out = new Array[Any](len - 2)
          System.arraycopy(arr.asInstanceOf[Array[Any]], 0, out, 0, index*2)
          System.arraycopy
                  ( arr.asInstanceOf[Array[Any]],
                    index*2 + 2,
                    out,
                    index*2,
                    len - index*2 - 2 )
          Yaml.ast(YamlAst.mapFromAnyArray(out))

  override def hashCode: Int = YamlAst.deepHash(root)

  override def equals(right: Any): Boolean = right match
    case right: Yaml => YamlAst.deepEquals(root, right.root)
    case _           => false

