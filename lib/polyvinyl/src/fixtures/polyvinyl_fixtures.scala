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
package polyvinyl

import scala.quoted.*

import anticipation.*
import gossamer.*

// Specification objects for the tests. Each lives in this module, compiled before `test`, so the
// `record` macro can evaluate it while the call sites are being compiled.

object PersonRecords extends TreeBlueprint(List(
  t"name"   -> Member.Value(t"text"),
  t"size"   -> Member.Value(t"length"),
  t"active" -> Member.Value(t"flag"),
  t"raw"    -> Member.Value(t"tree"),
  t"extras" -> Member.Value(t"params", t"alpha", t"beta"),
  t"count"  -> Member.Value(t"counted"))):
  transparent inline def record(tree: Tree): Record = ${build('tree)}
  transparent inline def tuple(tree: Tree): NamedTuple.AnyNamedTuple = ${tuple('tree)}

object NestedRecords extends TreeBlueprint(List(
  t"owner" -> Member.Record(t"node", List(
    t"name"    -> Member.Value(t"text"),
    t"address" -> Member.Record(t"node", List(t"city" -> Member.Value(t"text"))))),
  t"tags"  -> Member.Record(t"items", List(t"label" -> Member.Value(t"text"))))):
  transparent inline def record(tree: Tree): Record = ${build('tree)}
  transparent inline def tuple(tree: Tree): NamedTuple.AnyNamedTuple = ${tuple('tree)}

object TitleRecords extends TreeBlueprint(List(t"title" -> Member.Value(t"text"))):
  transparent inline def record(tree: Tree): Record = ${build('tree)}
  transparent inline def tuple(tree: Tree): NamedTuple.AnyNamedTuple = ${tuple('tree)}

// The remaining specifications are ill-formed: each compiles, but expanding its `record` macro
// fails, which the compiletime tests check.

object UnknownValueRecords extends TreeBlueprint(List(t"mystery" -> Member.Value(t"mystery"))):
  transparent inline def record(tree: Tree): Record = ${build('tree)}
  transparent inline def tuple(tree: Tree): NamedTuple.AnyNamedTuple = ${tuple('tree)}

object UnknownRecordRecords
extends TreeBlueprint(List(t"mystery" -> Member.Record(t"mystery", List()))):
  transparent inline def record(tree: Tree): Record = ${build('tree)}
  transparent inline def tuple(tree: Tree): NamedTuple.AnyNamedTuple = ${tuple('tree)}

// Instances are keyed by the exact label: `"Text"` is not `"text"`
object MiscasedRecords extends TreeBlueprint(List(t"name" -> Member.Value(t"Text"))):
  transparent inline def record(tree: Tree): Record = ${build('tree)}
  transparent inline def tuple(tree: Tree): NamedTuple.AnyNamedTuple = ${tuple('tree)}
