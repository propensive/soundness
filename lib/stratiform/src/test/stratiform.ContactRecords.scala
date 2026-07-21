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

import Tels.{Field, Polarity, Scalar, Struct}

// Hand-built TEL schema for the Polyvinyl `TelBlueprint` tests:
// a `Contact` record with a required `name` (String scalar), an
// optional `email` (String scalar), and a required `age` (identifier).
object ContactSchemaFixture:
  val tels: Tels = Tels(
    name     = t"contact",
    document = Struct(
      members = IArray(
        Field(Polarity.Implicit, Polarity.Implicit, t"name",  Scalar(IArray(t"string")),     Unset),
        Field(Polarity.Loose,    Polarity.Implicit, t"email", Scalar(IArray(t"string")),     Unset),
        Field(Polarity.Implicit, Polarity.Implicit, t"age",   Scalar(IArray(t"identifier")), Unset)),
      validators = IArray.empty),
    layers   = IArray.empty,
    sigil    = Unset,
    records  = IArray.empty,
    scalars  = IArray.empty,
    selects  = IArray.empty)

// User-defined TelBlueprint with the polyvinyl `record` inline-macro
// entry point. Lives in its own file so its macro can be expanded
// without a cyclic dependency from the call-site test file.
object ContactRecords extends TelBlueprint(ContactSchemaFixture.tels):
  transparent inline def record(tel: Tel): Record = ${build('tel)}

// A second schema with a Flag-typed field for the boolean records test.
object FeatureSchemaFixture:
  val tels: Tels = Tels(
    name     = t"feature",
    document = Struct(
      members    = IArray
                    (Field(Polarity.Loose, Polarity.Implicit, t"enabled", Tels.Flag, Unset)),
      validators = IArray.empty),
    layers   = IArray.empty,
    sigil    = Unset,
    records  = IArray.empty,
    scalars  = IArray.empty,
    selects  = IArray.empty)

object FeatureRecords extends TelBlueprint(FeatureSchemaFixture.tels):
  transparent inline def record(tel: Tel): Record = ${build('tel)}
