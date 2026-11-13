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

import soundness.*

import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics
import codepages.utf8Codepage

// The schema and the document of the RaTEL tests, apart from the suite so that a provider may
// be made from the schema: an object a macro consults is compiled before whatever uses it.
object RatelFixture:
  private def schemaOf(text: Text): Tels = Tels.Validation.validate(text.read[Tel].as[Tels])

  val folioSchema: Tels = schemaOf:
    Text("""|tel 1.0 specification.tel/tels:2.1.0
        |
        |name folio
        |
        |scalar Count
        |  encoding u32
        |
        |scalar Genre
        |  intern
        |
        |scalar Id
        |  encoding uuid
        |
        |record Author
        |  field name String
        |  field born Count optional
        |
        |record Stage
        |  field theatre String
        |  field city String optional
        |
        |select Ending
        |  variant death Flag
        |  variant marriage String
        |  variant staged Stage
        |
        |select Mark
        |  variant starred Flag
        |  variant note String
        |
        |record Play
        |  field title String key
        |  field id Id
        |  field genre Genre
        |  field acts Count
        |  field lines Count optional
        |  field author Author
        |  field stage Stage optional
        |  field lost Flag optional
        |  field revised Flag optional repeatable
        |  field canon Flag
        |  field role String optional repeatable
        |    index /
        |  field year Count optional repeatable
        |    index /
        |  select Ending optional
        |  select Mark optional repeatable
        |
        |document
        |  field name String
        |  field play Play optional repeatable
        |    index /genre /acts
        |    index .stage.city
        |    index /author/born
        |""".stripMargin)

  val folio: Tel =
    Text("""|tel 1.0
        |
        |name  First Folio
        |
        |play
        |  title Hamlet
        |  id 0192b1c2-3d4e-7f5a-8b6c-9d0e1f2a3b4c
        |  genre tragedy
        |  acts 5
        |  lines 4024
        |  author
        |    name  William Shakespeare
        |    born 1564
        |  stage
        |    theatre Globe
        |    city London
        |  revised
        |  revised
        |  canon
        |  role Hamlet
        |  role Ophelia
        |  year 1603
        |  year 1623
        |  death
        |  starred
        |  note quarto
        |
        |play
        |  title  Twelfth Night
        |  id 00000000-0000-0000-0000-000000000001
        |  genre comedy
        |  acts 5
        |  author
        |    name  William Shakespeare
        |  canon
        |  role Viola
        |  marriage Orsino
        |
        |play
        |  title Cardenio
        |  id 00000000-0000-0000-0000-000000000002
        |  genre tragedy
        |  acts 4
        |  author
        |    name  Nick Bottom
        |    born 1579
        |  stage
        |    theatre Blackfriars
        |  lost
        |  canon
        |  staged
        |    theatre Whitehall
        |    city London
        |""".stripMargin).read[Tel]

object FolioRecords extends Ratel.Provider(RatelFixture.folioSchema)

// A schema whose record contains itself, and a provider of each kind over it.
object LineageFixture:
  val schema: Tels = Tels.Validation.validate:
    Text("""|tel 1.0 specification.tel/tels:2.1.0
        |
        |name lineage
        |
        |record Heir
        |  field name String
        |  field heir Heir optional
        |
        |document
        |  field house String
        |  field heir Heir optional
        |""".stripMargin).read[Tel].as[Tels]

  val lineage: Tel =
    t"tel 1.0\n\nhouse Lancaster\nheir\n  name Hal\n  heir\n    name Harry\n".read[Tel]

object LineageRecords extends Ratel.Provider(LineageFixture.schema)
object LineageTelRecords extends Tel.Provider(LineageFixture.schema)
