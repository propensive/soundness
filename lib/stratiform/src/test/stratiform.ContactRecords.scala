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
import gossamer.*
import polyvinyl.*
import vacuous.*

import Tels.{Field, Polarity, Reference, Scalar, Struct}

// Hand-built TEL schema for the Polyvinyl `Tel.Provider` tests:
// a `Contact` record with a required `name` (String scalar), an
// optional `email` (String scalar), and a required `age` (identifier).
object ContactSchemaFixture:
  val tels: Tels = Tels(
    name     = t"contact",
    document = Struct(
      members = Array(
        Field(Polarity.Implicit, Polarity.Implicit, t"name",  Scalar(Array(t"string")),     Unset),
        Field(Polarity.Loose,    Polarity.Implicit, t"email", Scalar(Array(t"string")),     Unset),
        Field(Polarity.Implicit, Polarity.Implicit, t"age",   Scalar(Array(t"identifier")), Unset)),
      validators = Array.empty),
    layers   = Array.empty,
    sigil    = Unset,
    records  = Array.empty,
    scalars  = Array.empty,
    selects  = Array.empty)

// User-defined Tel.Provider with the polyvinyl `record` inline-macro
// entry point. Lives in its own file so its macro can be expanded
// without a cyclic dependency from the call-site test file.
object ContactRecords extends Tel.Provider(ContactSchemaFixture.tels):
  transparent inline def record(tel: Tel): Record = ${build('tel)}
  transparent inline def tuple(tel: Tel): NamedTuple.AnyNamedTuple = ${tuple('tel)}

// A second schema with a Flag-typed field for the boolean records test.
object FeatureSchemaFixture:
  val tels: Tels = Tels(
    name     = t"feature",
    document = Struct(
      members    = Array
                    (Field(Polarity.Loose, Polarity.Implicit, t"enabled", Tels.Flag, Unset)),
      validators = Array.empty),
    layers   = Array.empty,
    sigil    = Unset,
    records  = Array.empty,
    scalars  = Array.empty,
    selects  = Array.empty)

object FeatureRecords extends Tel.Provider(FeatureSchemaFixture.tels):
  transparent inline def record(tel: Tel): Record = ${build('tel)}

// A layered schema for the layer-provenance records test: `name` in the base, `email` in the
// layer `with-email`, so `email` reads as optional although the layer declares it required.
object LayeredSchemaFixture:
  val tels: Tels = Tels(
    name     = t"layered",
    document = Struct(
      members    = Array(Field(Polarity.Implicit, Polarity.Implicit, t"name", Scalar(Array(t"string")), Unset)),
      validators = Array.empty),
    layers   = Array(
      Tels.Layer(
        t"with-email",
        Struct(
          members    = Array(Field(Polarity.Implicit, Polarity.Implicit, t"email", Scalar(Array(t"string")), Unset)),
          validators = Array.empty),
        Array.empty, Array.empty, Array.empty)),
    sigil    = Unset,
    records  = Array.empty,
    scalars  = Array.empty,
    selects  = Array.empty)

object LayeredRecords extends Tel.Provider(LayeredSchemaFixture.tels):
  transparent inline def record(tel: Tel): Record = ${build('tel)}


// Nested and repeatable members: a required `name`, a repeatable `tag` scalar, a required and an
// optional reference to the `Person` record, and a repeatable one.
object TeamSchemaFixture:
  val person: Tels.RecordDefinition = Tels.RecordDefinition(
    t"Person",
    Array(
      Field(Polarity.Implicit, Polarity.Implicit, t"name", Scalar(Array(t"string")),     Unset),
      Field(Polarity.Loose,    Polarity.Implicit, t"role", Scalar(Array(t"identifier")), Unset)),
    Array.empty)

  val tels: Tels = Tels(
    name     = t"team",
    document = Struct(
      members = Array(
        Field(Polarity.Implicit, Polarity.Implicit, t"name",   Scalar(Array(t"string")), Unset),
        Field(Polarity.Implicit, Polarity.Loose,    t"tag",    Scalar(Array(t"string")), Unset),
        Field(Polarity.Implicit, Polarity.Implicit, t"lead",   Reference(t"Person"),     Unset),
        Field(Polarity.Loose,    Polarity.Implicit, t"deputy", Reference(t"Person"),     Unset),
        Field(Polarity.Implicit, Polarity.Loose,    t"member", Reference(t"Person"),     Unset)),
      validators = Array.empty),
    layers   = Array.empty,
    sigil    = Unset,
    records  = Array(person),
    scalars  = Array.empty,
    selects  = Array.empty)

object TeamRecords extends Tel.Provider(TeamSchemaFixture.tels):
  transparent inline def record(tel: Tel): Record = ${build('tel)}
  transparent inline def tuple(tel: Tel): NamedTuple.AnyNamedTuple = ${tuple('tel)}

// Kebab-case names, a custom validator, an inline struct, and a select member
object ProfileSchemaFixture:
  val bio: Struct =
    Struct
      ( Array(Field(Polarity.Implicit, Polarity.Loose, t"line", Scalar(Array(t"string")), Unset)),
        Array.empty )

  val tels: Tels = Tels(
    name     = t"profile",
    document = Struct(
      members = Array(
        Field(Polarity.Implicit, Polarity.Implicit, t"first-name", Scalar(Array(t"string")), Unset),
        Field(Polarity.Implicit, Polarity.Implicit, t"handle",     Scalar(Array(t"handle")), Unset),
        Field(Polarity.Loose,    Polarity.Implicit, t"bio",        bio,                      Unset),
        Tels.SelectRef(Polarity.Loose, Polarity.Implicit, t"Status")),
      validators = Array.empty),
    layers   = Array.empty,
    sigil    = Unset,
    records  = Array.empty,
    scalars  = Array.empty,
    selects  = Array(Tels.SelectDefinition(
      name       = t"Status",
      variants   = Array(
        Tels.Variant(t"active",   Tels.Flag),
        Tels.Variant(t"archived", Tels.Flag),
        Tels.Variant(t"note",     Scalar(Array(t"string")))),
      validators = Array.empty)))

object ProfileRecords extends Tel.Provider(ProfileSchemaFixture.tels):
  transparent inline def record(tel: Tel): Record = ${build('tel)}

// A user type for the custom `handle` validator, read through a given at the call site
case class Handle(name: Text)
