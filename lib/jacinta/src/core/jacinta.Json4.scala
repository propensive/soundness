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

import scala.language.experimental.into

import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import fulminate.*
import gossamer.*
import kaleidoscope.*
import polyvinyl.*
import prepositional.*
import rudiments.*
import turbulence.read
import spectacular.*
import vacuous.*

import strategies.throwUnsafely

// Holds `Json.Provider`, the type provider over a JSON Schema document. Its own layer in `Json`'s
// companion chain so that `Json.scala` need not carry it; it contributes no givens for `Json`
// itself.
trait Json4:
  // A polyvinyl `Specification` over a JSON Schema document: each `Json.Provider` binds a
  // schema to a structurally-typed `Record` with one field per property of the root object,
  // read at the type the property's schema declares.
  //
  // Usage:
  //   object Catalogue extends Json.Provider(cp"/schemas/catalogue.json"):
  //     transparent inline def record(json: Json): Record = ${build('json)}
  //
  // A property named in `required` reads as its type; any other, or one whose type admits
  // `null`, as `Optional`. An `array` property reads its `items` schema as a `List`; an `object`
  // property with `properties` as a nested record, and one without (a dictionary constrained by
  // `additionalProperties` or `patternProperties`) as the raw `Json`. A local `$ref` is followed
  // (`#/definitions/…`, `#/$defs/…`, any JSON pointer into the document); a reference to
  // another document, a recursive reference, and a `type` the provider cannot make one type of
  // (`oneOf`/`anyOf` over different types, `not`, `if`) read as the raw `Json`. An `allOf` of
  // objects merges their properties. `enum`/`const` over strings, `pattern`, `minLength`/
  // `maxLength`, and `minimum`/`maximum`/`exclusiveMinimum`/`exclusiveMaximum` are checked as
  // the field is read, raising `Json.Provider.Error`.
  //
  // The formats which name a network or identity type — `email`, `hostname`, `ipv4`, `ipv6`,
  // `uri`, `uuid` and their relatives — read as whichever type represents that domain in scope,
  // selected by importing its interface (`emailAddressInterfaces.soundnessEmailAddress`,
  // `urlInterfaces.soundnessUrl`, `uuidInterfaces.soundnessUuid`, …).
  object Provider:
    given boolean: ("boolean" is Intensional in Json.Provider from Json to Boolean) =
      Intensional(_.as[Boolean])

    given string: ("string" is Intensional in Json.Provider from Json to Text) =
      Intensional(_.as[Text])

    given integer: ("integer" is Intensional in Json.Provider from Json to Int) =
      Intensional(_.as[Int])

    // An `integer` whose `format` is `int64`
    given long: ("long" is Intensional in Json.Provider from Json to Long) =
      Intensional(_.as[Long])

    given number: ("number" is Intensional in Json.Provider from Json to Double) =
      Intensional(_.as[Double])

    // A value the schema does not constrain to one Scala type, read as it is
    given json: ("json" is Intensional in Json.Provider from Json to Json) = Intensional(identity)

    given dateTime: [instant: Instantiable across Instants from Text]
    =>  ( "date-time" is Intensional in Json.Provider from Json to instant ) =
      Intensional(_.as[Text].instantiate)

    given date: [date: Instantiable across Dates from Text]
    =>  ( "date" is Intensional in Json.Provider from Json to date ) =
      Intensional(_.as[Text].instantiate)

    given time: [time: Instantiable across Times from Text]
    =>  ( "time" is Intensional in Json.Provider from Json to time ) =
      Intensional(_.as[Text].instantiate)

    given duration: [duration: Instantiable across Durations from Text]
    =>  ( "duration" is Intensional in Json.Provider from Json to duration ) =
      Intensional(_.as[Text].instantiate)

    given uriReference: ("uri-reference" is Intensional in Json.Provider from Json to Text) =
      Intensional(_.as[Text])

    given email: [email: Instantiable across EmailAddresses from Text]
    =>  ( "email" is Intensional in Json.Provider from Json to email ) =
      Intensional(_.as[Text].instantiate)

    given idnEmail: [email: Instantiable across EmailAddresses from Text]
    =>  ( "idn-email" is Intensional in Json.Provider from Json to email ) =
      Intensional(_.as[Text].instantiate)

    given hostname: [hostname: Instantiable across Hostnames from Text]
    =>  ( "hostname" is Intensional in Json.Provider from Json to hostname ) =
      Intensional(_.as[Text].instantiate)

    given ipv4: [ipv4: Instantiable across IpAddresses from Text]
    =>  ( "ipv4" is Intensional in Json.Provider from Json to ipv4 ) =
      Intensional(_.as[Text].instantiate)

    given ipv6: [ipv6: Instantiable across IpAddresses from Text]
    =>  ( "ipv6" is Intensional in Json.Provider from Json to ipv6 ) =
      Intensional(_.as[Text].instantiate)

    given uri: [url: Instantiable across Urls from Text]
    =>  ( "uri" is Intensional in Json.Provider from Json to url ) =
      Intensional(_.as[Text].instantiate)

    given iri: [url: Instantiable across Urls from Text]
    =>  ( "iri" is Intensional in Json.Provider from Json to url ) =
      Intensional(_.as[Text].instantiate)

    given iriReference: ("iri-reference" is Intensional in Json.Provider from Json to Text) =
      Intensional(_.as[Text])

    given uuid: [uuid: Instantiable across Uuids from Text]
    =>  ( "uuid" is Intensional in Json.Provider from Json to uuid ) =
      Intensional(_.as[Text].instantiate)

    given uriTemplate: ("uri-template" is Intensional in Json.Provider from Json to Text) =
      Intensional(_.as[Text])

    given jsonPointer: ("json-pointer" is Intensional in Json.Provider from Json to JsonPointer) =
      Intensional(_.as[Text].as[JsonPointer])

    given regex: ("regex" is Intensional in Json.Provider from Json to (Regex in JavaBaseRegex)) =
      Intensional: json => Regex(json.as[Text])

    // The formats above, by name; any other format reads as a plain string
    private val formats: scala.collection.immutable.Set[Text] =
      scala.collection.immutable.Set
        ( t"date-time", t"date", t"time", t"duration", t"uri-reference", t"email", t"idn-email",
          t"hostname", t"ipv4", t"ipv6", t"uri", t"iri", t"iri-reference", t"uuid",
          t"uri-template", t"json-pointer", t"regex" )

    private def fail(reason: Json.Provider.Error.Reason): Nothing =
      abort(Json.Provider.Error(reason))

    private def bound(param: Text): Optional[Double] = safely(param.as[Double])

    // The fallible readers are named classes, with their givens declared at the classes' own
    // types: an instance declared at the refined `Intensional.Fallible` type would be a field
    // whose type captures `any`, making this object a capability, and an anonymous subclass
    // freshens that capability in its inferred `Result`.

    // An integer within the schema's bounds. The parameters are `minimum`, `maximum`,
    // `exclusiveMinimum` and `exclusiveMaximum`, each possibly empty.
    class BoundedInteger extends Intensional.Fallible:
      type Self = "integer!"
      type Origin = Json
      type Form = Json.Provider
      type Result = Int
      type Error = Json.Provider.Error

      def transform(json: Json, params: List[Text])(using Tactic[Json.Provider.Error]): Int =
        val int = json.as[Int]

        params.absolve match
          case min :: max :: xmin :: xmax :: Nil =>
            val minimum: Optional[Int] =
              bound(min).let(_.toInt).or(bound(xmin).let(_.toInt + 1))

            val maximum: Optional[Int] =
              bound(max).let(_.toInt).or(bound(xmax).let(_.toInt - 1))

            if minimum.let(int < _).or(false) || maximum.let(int > _).or(false)
            then fail(Json.Provider.Error.Reason.IntOutOfRange(int, minimum, maximum))
            else int

    given boundedInteger: BoundedInteger = BoundedInteger()

    // A number within the schema's bounds, with the same parameters as `BoundedInteger`
    class BoundedNumber extends Intensional.Fallible:
      type Self = "number!"
      type Origin = Json
      type Form = Json.Provider
      type Result = Double
      type Error = Json.Provider.Error

      def transform(json: Json, params: List[Text])(using Tactic[Json.Provider.Error]): Double =
        val double = json.as[Double]

        params.absolve match
          case min :: max :: xmin :: xmax :: Nil =>
            val low = bound(min).let(double < _).or(false) || bound(xmin).let(double <= _).or(false)

            val high =
              bound(max).let(double > _).or(false) || bound(xmax).let(double >= _).or(false)

            if low || high
            then
              fail:
                Json.Provider.Error.Reason.NumberOutOfRange
                  ( double, bound(min).or(bound(xmin)), bound(max).or(bound(xmax)) )
            else
              double

    given boundedNumber: BoundedNumber = BoundedNumber()

    // A string whose length is within the schema's `minLength` and `maxLength`
    class BoundedString extends Intensional.Fallible:
      type Self = "string!"
      type Origin = Json
      type Form = Json.Provider
      type Result = Text
      type Error = Json.Provider.Error

      def transform(json: Json, params: List[Text])(using Tactic[Json.Provider.Error]): Text =
        val text = json.as[Text]
        val length = text.length

        params.absolve match
          case min :: max :: Nil =>
            val minimum = bound(min).let(_.toInt)
            val maximum = bound(max).let(_.toInt)

            if minimum.let(length < _).or(false) || maximum.let(length > _).or(false)
            then fail(Json.Provider.Error.Reason.LengthOutOfRange(text, minimum, maximum))
            else text

    given boundedString: BoundedString = BoundedString()

    // A string matching the schema's `pattern`, which is the member's parameter
    class Pattern extends Intensional.Fallible:
      type Self = "pattern"
      type Origin = Json
      type Form = Json.Provider
      type Result = Text
      type Error = Json.Provider.Error

      def transform(json: Json, params: List[Text])(using Tactic[Json.Provider.Error]): Text =
        params.absolve match
          case pattern :: Nil =>
            val regex = Regex(pattern)
            val text = json.as[Text]

            if regex.matches(text) then text
            else fail(Json.Provider.Error.Reason.PatternMismatch(text, regex))

    given pattern: Pattern = Pattern()

    // A string among the schema's `enum` values (or equal to its `const`), which are the
    // member's parameters
    class Enumeration extends Intensional.Fallible:
      type Self = "enum"
      type Origin = Json
      type Form = Json.Provider
      type Result = Text
      type Error = Json.Provider.Error

      def transform(json: Json, params: List[Text])(using Tactic[Json.Provider.Error]): Text =
        val text = json.as[Text]

        if params.has(text) then text
        else fail(Json.Provider.Error.Reason.NotPermitted(text, params))

    given enumeration: Enumeration = Enumeration()

    // The format's reading primitives, without a schema: what a `Json.Provider` adds its `fields`
    // to, and what a macro with fields of its own (apoplexy's `record()`) builds records over
    trait Primitives extends Specification:
      type Origin = Json
      type Form = Json.Provider

      // `Json.apply` yields an absent JSON value, rather than failing, for a key the object lacks;
      // a `null` value is read as absent too, since the schema's `null` type marks a field optional.
      def access(name: Text, json: Json): Json = json(name)
      def absent(json: Json): Boolean = json.root.isAbsent || json.root.isNull

      // A required field which is absent fails as it is read, whether it is a value or an object
      override def required(name: Text, json: Json): Json =
        if absent(json) then abort(Json.Error(Json.Error.Reason.Absent)) else json

      // The kind of a JSON value, by which a union chooses its alternative
      override def kind(json: Json): Text =
        if json.root.isString then t"string"
        else if json.root.isBoolean then t"boolean"
        else if json.root.isNumber then t"number"
        else if json.root.isObject then t"object"
        else if json.root.isArray then t"array"
        else t"null"

      override def elements(json: Json): List[Json] = repeated(t"", json)

      override def pairs(json: Json): List[(Text, Json)] =
        if !json.root.isObject then List()
        else
          val ast = json.root

          val entries = scala.collection.immutable.List.tabulate(ast.objectSize): index =>
            ast.objectKey(index).tt -> Json.ast(ast.objectValue(index))

          List.from(entries)

      override def entries(name: Text, json: Json): List[(Text, Json)] = pairs(json(name))

      def repeated(name: Text, json: Json): List[Json] =
        val value = if name.nil then json else json(name)
        if absent(value) then List() else value.as[List[Json]]

    // The fields of a schema's root object, as the provider's specification. The root must
    // describe an object with properties, possibly through a `$ref` or an `allOf`.
    // The root must describe an object with properties, possibly through a `$ref` or an `allOf`;
    // where the root offers alternatives (`anyOf`/`oneOf`) of which one is such an object — a
    // configuration file which may also be written as a bare string, say — the provider reads
    // that object form.
    def fieldsOf(schema: Json): List[(Text, Member)] =
      val walk = Walk(schema)

      walk.rootRecord(schema) match
        case Member.Record(fields, Multiplicity.One) => fields

        case _ =>
          val reason =
            if walk.external
            then m", and refers to another document, which cannot be followed"
            else m""

          panic(m"the schema's root does not describe an object with properties$reason")

    // The member reading the values a node within a larger document describes: its `$ref`s
    // resolve against the document, so a schema embedded in one — a response schema within an
    // OpenAPI document, referring to `#/components/schemas/…` — reads as it would alone. An
    // object reads as a `Member.Record`, an array of objects as one of `Multiplicity.Many`.
    def memberOf(document: Json, node: Json, limit: Int = Int.MaxValue): Member =
      Walk(document, limit).member(node, scala.collection.immutable.Set())

    // The walk from a schema node to the `Member` reading a value it describes. `seen` holds
    // the `$ref` targets on the path to the node, so a recursive schema reads as raw `Json` at
    // the point of recursion rather than expanding without end. The walk keeps to the standard
    // library's lists internally and converts at its boundary.
    // `limit` bounds how many `$ref`s deep the walk follows before reading a reference as raw
    // `Json`: an API's error schema may refer into a graph of thousands of properties, of which
    // a caller wants the first level or two as a record. References already followed are
    // memoised, so a graph is walked once, not once per path into it.
    private class Walk(root: Json, limit: Int = Int.MaxValue):
      private val followed = scala.collection.mutable.HashMap[Text, Member]()
      private type Sl[element] = scala.collection.immutable.List[element]
      private type SSet[element] = scala.collection.immutable.Set[element]
      private val Sl = scala.collection.immutable.List
      private val SNil = scala.collection.immutable.Nil
      private val any: Member = Member.Value(t"json")
      private val absentJson: Json = Json.ast(Json.Ast(Unset))
      private val ref: Text = "$ref".tt

      private def isObject(node: Json): Boolean = node.root.isObject
      private def present(node: Json): Boolean = !(node.root.isAbsent || node.root.isNull)

      private def key(node: Json, name: Text): Json =
        if isObject(node) then node(name) else absentJson

      private def text(node: Json, name: Text): Optional[Text] =
        val value = key(node, name)
        if present(value) && value.root.isString then value.as[Text] else Unset

      private def number(node: Json, name: Text): Optional[Double] =
        val value = key(node, name)
        if present(value) && value.root.isNumber then value.as[Double] else Unset

      private def flag(node: Json, name: Text): Boolean =
        val value = key(node, name)
        present(value) && value.root.isBoolean && value.as[Boolean]

      private def elements(node: Json): Sl[Json] =
        if present(node) && node.root.isArray
        then Sl.tabulate(node.root.arrayLength): index => Json.ast(node.root.arrayElement(index))
        else SNil

      private def list(node: Json, name: Text): Sl[Json] = elements(key(node, name))

      private def pairs(node: Json): Sl[(Text, Json)] =
        if !isObject(node) then SNil
        else
          val ast = node.root

          Sl.tabulate(ast.objectSize): index =>
            ast.objectKey(index).tt -> Json.ast(ast.objectValue(index))

      private def texts(node: Json, name: Text): Sl[Text] =
        list(node, name).filter(_.root.isString).map(_.as[Text])

      // The value a JSON pointer (RFC 6901) into this document names, or `Unset`
      private def deref(pointer: Text): Optional[Json] =
        val path = pointer.s.stripPrefix("#").stripPrefix("/")

        val segments: Sl[String] =
          if path.isEmpty then SNil
          else
            val parts = scala.collection.immutable.ArraySeq.unsafeWrapArray(path.split("/", -1).nn)
            Sl.from(parts).map(_.nn.replace("~1", "/").nn.replace("~0", "~").nn)

        def descend(current: Optional[Json], segment: String): Optional[Json] = current.let: node =>
          if node.root.isObject then
            val value = key(node, segment.tt)
            if present(value) then value else Unset
          else if node.root.isArray && segment.nonEmpty && segment.forall(_.isDigit) then
            val items = elements(node)
            val index = segment.toInt
            if index < items.length then items(index) else Unset
          else
            Unset

        segments.foldLeft[Optional[Json]](root)(descend)

      private def kind(sample: Json): Text =
        if sample.root.isString then t"string"
        else if sample.root.isBoolean then t"boolean"
        else if sample.root.isLong then t"integer"
        else if sample.root.isNumber then t"number"
        else if sample.root.isNull then t"null"
        else t"json"

      // The `enum` values and the `const`, if any
      private def samples(node: Json): Sl[Json] =
        val constant = key(node, t"const")
        if present(constant) then Sl(constant) else list(node, t"enum")

      // The types a node declares, from `type` (a name or a list of names) or inferred from
      // its other keywords
      private def types(node: Json): Sl[Text] =
        val declared = key(node, t"type")

        if present(declared) && declared.root.isString then Sl(declared.as[Text])
        else if present(declared) && declared.root.isArray then texts(node, t"type")
        else if isObject(key(node, t"properties")) then Sl(t"object")
        else if present(key(node, t"items")) then Sl(t"array")
        else samples(node).map(kind).distinct

      // The permitted string values, from `enum` or `const`, if every non-null value is a string
      private def permitted(node: Json): Optional[Sl[Text]] =
        val nonNull = samples(node).filterNot(_.root.isNull)

        if nonNull.isEmpty || !nonNull.forall(_.root.isString) then Unset
        else nonNull.map(_.as[Text])

      private def nullable(node: Json): Boolean =
        types(node).contains(t"null") ||
          samples(node).exists(_.root.isNull) ||
          (list(node, t"anyOf") ++ list(node, t"oneOf")).exists(types(_).contains(t"null"))

      // The fields of an object node: its `properties`, each required or optional as its
      // `required` list says
      private def fields(node: Json, seen: SSet[Text]): Sl[(Text, Member)] =
        val required = texts(node, t"required").toSet
        val properties = key(node, t"properties")

        if !isObject(properties) then SNil
        else
          val ast = properties.root

          Sl.tabulate(ast.objectSize): index =>
            val name = ast.objectKey(index).tt
            val property = Json.ast(ast.objectValue(index))
            val member = this.member(property, seen)

            // A list or dictionary keeps its multiplicity, reading as empty when absent
            val multiplicity = member.multiplicity match
              case Multiplicity.Many | Multiplicity.Keyed => member.multiplicity

              case _ =>
                if required.contains(name) && !nullable(property) then Multiplicity.One
                else Multiplicity.Optional

            name -> member.of(multiplicity)

      // `minimum`, `maximum`, `exclusiveMinimum` and `exclusiveMaximum`, each empty when
      // unset. Draft 4 wrote `exclusiveMinimum: true` to qualify `minimum`; later drafts give
      // the bound itself.
      private def bounds(node: Json): Sl[Text] =
        def exclusive(name: Text, inclusive: Text): Optional[Double] =
          if flag(node, name) then number(node, inclusive) else number(node, name)

        val minimum = if flag(node, t"exclusiveMinimum") then Unset else number(node, t"minimum")
        val maximum = if flag(node, t"exclusiveMaximum") then Unset else number(node, t"maximum")

        Sl
          ( minimum, maximum, exclusive(t"exclusiveMinimum", t"minimum"),
            exclusive(t"exclusiveMaximum", t"maximum") )

        . map(_.let(_.toString.tt).or(t""))

      private def value(label: Text, params: Sl[Text] = SNil): Member =
        Member.Value(label, List.from(params))

      // The JSON kind of the values a member reads, by which a union tells its alternatives
      // apart, or `Unset` for a member which reads values of several kinds
      private def kindOf(member: Member): Optional[Text] = member.multiplicity match
        case Multiplicity.Many  => t"array"
        case Multiplicity.Keyed => t"object"

        case _ => member match
          case Member.Record(_, _) => t"object"
          case Member.Union(_, _)  => Unset

          case Member.Value(label, _, _) => label.s match
            case "boolean"                                     => t"boolean"
            case "integer" | "integer!" | "number" | "number!" => t"number"
            case "json"                                        => Unset
            case _                                             => t"string"

      // A member as the element of a list or the value of a dictionary: one already carrying a
      // multiplicity is wrapped as the one alternative of a union, since a member carries a
      // single multiplicity and the union's is then the outer one
      private def nested(member: Member): Member =
        if member.multiplicity == Multiplicity.One then member
        else kindOf(member).lay(any): kind => Member.Union(List(kind -> member))

      // Alternatives of distinct kinds read as a union of their types, chosen by the value's
      // kind. Identical alternatives collapse; alternatives sharing a kind, or of no one kind,
      // cannot be told apart, and the whole reads as raw JSON.
      private def union(members: Sl[Member]): Member =
        val distinct = members.distinct
        val kinds = distinct.map(kindOf)

        if distinct.length == 1 then distinct.head
        else if kinds.exists(_.absent) || kinds.distinct.length != kinds.length then any
        else Member.Union(List.from(kinds.map(_.or(t"")).zip(distinct)))

      // The member reading a value of one declared type
      private def typed(name: Text, node: Json, seen: SSet[Text]): Member = name.s match
        // An object with properties is a record; one whose values a schema describes instead —
        // `additionalProperties`, or `patternProperties` all of one type — is a dictionary
        case "object" =>
          val additional = key(node, t"additionalProperties")
          val patterns = key(node, t"patternProperties")

          if isObject(key(node, t"properties")) then Member.Record(List.from(fields(node, seen)))
          else if isObject(additional) then nested(member(additional, seen)).keyed
          else if isObject(patterns) then
            val values = pairs(patterns).map { (_, schema) => member(schema, seen) }.distinct
            if values.length == 1 then nested(values.head).keyed else any
          else
            any

        case "array" =>
          val items = key(node, t"items")
          (if isObject(items) then nested(member(items, seen)) else any).many

        case "string" =>
          val minimum = number(node, t"minLength")
          val maximum = number(node, t"maxLength")
          val format = text(node, t"format")
          val values = permitted(node)
          val pattern = text(node, t"pattern")

          if values.present then value(t"enum", values.or(SNil))
          else if pattern.present then value(t"pattern", Sl(pattern.or(t"")))
          else if minimum.present || maximum.present then
            value(t"string!", Sl(minimum, maximum).map(_.let(_.toInt.show).or(t"")))
          else if format.let(formats.contains(_)).or(false) then
            value(format.or(t"string"))
          else
            value(t"string")

        case "integer" =>
          val limits = bounds(node)
          val wide = text(node, t"format") == t"int64"

          if !limits.forall(_.nil) then value(t"integer!", limits)
          else if wide then value(t"long")
          else value(t"integer")

        case "number" =>
          val limits = bounds(node)
          if limits.forall(_.nil) then value(t"number") else value(t"number!", limits)

        case "boolean" => value(t"boolean")
        case _         => any

      // Whether two members read at the same type, so that alternatives may be unified
      private def same(left: Member, right: Member): Boolean = (left, right) match
        case (Member.Value(l, lp, lm), Member.Value(r, rp, rm)) => l == r && lp == rp && lm == rm
        case _                                                  => false

      private def isRecord(member: Member): Boolean = member match
        case Member.Record(_, _) => true
        case _                   => false

      private def recordFields(member: Member): Sl[(Text, Member)] = member match
        case Member.Record(fields, _) => fields.stdlib
        case _                        => SNil

      // Whether the walk met a reference to another document, which it cannot follow
      private val externalRefs = java.util.concurrent.atomic.AtomicBoolean(false)
      def external: Boolean = externalRefs.get

      // The root's member, or the one object among its alternatives
      def rootRecord(node: Json): Member =
        val seen = scala.collection.immutable.Set[Text]()

        def single(member: Member): Boolean = member.multiplicity != Multiplicity.Many

        member(node, seen) match
          case record: Member.Record if single(record) => record

          case other =>
            val alternatives = list(node, t"anyOf") ++ list(node, t"oneOf")

            val records = alternatives.map(member(_, seen)).filter: m => isRecord(m) && single(m)
            records.headOption.getOrElse(other)

      def member(node: Json, seen: SSet[Text]): Member =
        if !isObject(node) then any
        else
          val reference = text(node, ref)

          if reference.present then referenced(reference.or(t""), seen)
          else
            val allOf = list(node, t"allOf")
            val alternatives = list(node, t"anyOf") ++ list(node, t"oneOf")

            if allOf.nonEmpty then merged(node, allOf, seen)
            else if alternatives.nonEmpty then unified(alternatives, seen)
            else
              val named = types(node).filterNot(_ == t"null")

              if named.length == 1 then typed(named.head, node, seen)
              else if named.isEmpty then any
              else union(named.map(typed(_, node, seen)))

      // An `allOf` of objects merges their properties, the first declaration of a name winning;
      // the node's own properties join them
      private def merged(node: Json, allOf: Sl[Json], seen: SSet[Text]): Member =
        val own =
          if !isObject(key(node, t"properties")) then SNil
          else Sl(Member.Record(List.from(fields(node, seen))))

        val parts = allOf.map(member(_, seen)) ++ own

        if parts.forall(isRecord) then
          val names = scala.collection.mutable.LinkedHashMap.empty[Text, Member]

          parts.flatMap(recordFields).foreach: (name, member) =>
            if !names.contains(name) then names(name) = member

          Member.Record(List.from(names.toList))
        else
          any

      // Alternatives all reading at one type read at that type; a `null` alternative alone marks
      // the value optional, which `nullable` reports to the enclosing object
      private def unified(alternatives: Sl[Json], seen: SSet[Text]): Member =
        val members = alternatives.filterNot(types(_) == Sl(t"null")).map(member(_, seen))

        if members.isEmpty then any
        else if members.tail.forall(same(members.head, _)) then members.head
        else unifiedStrings(members).or(union(members))

      // Alternatives which are all strings read as a string: every one an `enum` (a common way
      // to document each value) as one `enum` of all their values; otherwise, if any is a plain
      // string, as a plain string, since the constrained forms cannot be combined
      private def unifiedStrings(members: Sl[Member]): Optional[Member] =
        val strings =
          scala.collection.immutable.Set(t"string", t"string!", t"pattern", t"enum") ++ formats

        def label(member: Member): Optional[Text] = member match
          case Member.Value(label, _, Multiplicity.One) if strings.contains(label) => label
          case _                                                                   => Unset

        val labels = members.map(label)

        if !labels.forall(_.present) then Unset
        else if labels.forall(_ == t"enum") then
          val values = members.flatMap:
            case Member.Value(_, params, _) => params.stdlib
            case _                          => SNil

          Member.Value(t"enum", List.from(values.distinct))
        else
          Member.Value(t"string")

      // The member a `$ref` reads: its target's, for a local reference not already on the path
      private def referenced(target: Text, seen: SSet[Text]): Member =
        if !target.starts(t"#") then
          externalRefs.set(true)
          any
        else if seen.contains(target) || seen.size >= limit then
          any
        else
          followed.getOrElseUpdate(target, deref(target).let(member(_, seen.incl(target))).or(any))

    // The schema a provider is built from: JSON already parsed, or — through the conversions in
    // the companion, applied at the `into` parameter — anything readable as JSON, such as a
    // classpath resource (`cp"/x.json"`), a file, or JSON text. A conversion rather than a second
    // constructor, so that no source type need be named here.
    class Schema(val json: Json)

    trait Schema2:
      given readable: [source] => (readable: (source is turbulence.Readable to Json)^)
      =>  (Conversion[source, Schema]^{readable}) =
        source => Schema(source.read[Json](using readable))

    object Schema extends Schema2:
      given json: Conversion[Json, Schema] = Schema(_)

    object Error:
      object Reason:
        given Reason is Communicable =
          case JsonType(expected, found) => m"expected JSON type $expected, but found $found"
          case MissingValue              => m"the value was missing"

          case IntOutOfRange(value, minimum, maximum) =>
            if minimum.absent then m"the value was greater than the maximum, ${maximum.or(0)}"
            else if maximum.absent then m"the value was less than the minimum, ${minimum.or(0)}"
            else m"the value was not between ${minimum.or(0)} and ${maximum.or(0)}"

          case PatternMismatch(value, pattern) =>
            m"the value did not conform to the regular expression ${pattern.pattern}"

          case NumberOutOfRange(value, minimum, maximum) =>
            val low = minimum.or(0.0).toString.tt
            val high = maximum.or(0.0).toString.tt

            if minimum.absent then m"the number was above the maximum, $high"
            else if maximum.absent then m"the number was below the minimum, $low"
            else m"the number was not between $low and $high"

          case LengthOutOfRange(value, minimum, maximum) =>
            if minimum.absent then m"the string was longer than ${maximum.or(0)} characters"
            else if maximum.absent then m"the string was shorter than ${minimum.or(0)} characters"
            else m"the string's length was not between ${minimum.or(0)} and ${maximum.or(0)}"

          case NotPermitted(value, permitted) =>
            m"the value $value was not one of ${permitted.join(t", ")}"

      enum Reason(val number: Int) extends Clarification:
        case JsonType(expected: Json.Primitive, found: Json.Primitive) extends Reason(1)
        case MissingValue                                              extends Reason(2)
        case PatternMismatch(value: Text, pattern: Regex)              extends Reason(4)
        case NotPermitted(value: Text, permitted: List[Text])          extends Reason(7)

        case IntOutOfRange(value: Int, minimum: Optional[Int], maximum: Optional[Int])
        extends Reason(3)

        case NumberOutOfRange(value: Double, minimum: Optional[Double], maximum: Optional[Double])
        extends Reason(5)

        case LengthOutOfRange(value: Text, minimum: Optional[Int], maximum: Optional[Int])
        extends Reason(6)

    case class Error(reason: Json.Provider.Error.Reason)(using Diagnostics)
    extends fulminate.Error(624, reason.number)
      ( m"the JSON was not valid according to the schema because $reason" )

  abstract class Provider(schema0: into[Json.Provider.Schema]) extends Provider.Primitives:
    val schema: Json = schema0.json

    def fields: List[(Text, Member)] = Json.Provider.fieldsOf(schema)

