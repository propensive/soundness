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

object PersonRecords extends TreeProvider(List(
  t"name"   -> Member.Value(t"text"),
  t"size"   -> Member.Value(t"length"),
  t"active" -> Member.Value(t"flag"),
  t"raw"    -> Member.Value(t"tree"),
  t"extras" -> Member.Value(t"params", List(t"alpha", t"beta")),
  t"count"  -> Member.Value(t"counted"))):
  transparent inline def record(tree: Tree): Record = ${build('tree)}
  transparent inline def tuple(tree: Tree): NamedTuple.AnyNamedTuple = ${tuple('tree)}

object NestedRecords extends TreeProvider(List(
  t"owner" -> Member.Record(List(
    t"name"    -> Member.Value(t"text"),
    t"address" -> Member.Record(List(t"city" -> Member.Value(t"text"))))),
  t"tags"  -> Member.Record(List(t"label" -> Member.Value(t"text"))).many)):
  transparent inline def record(tree: Tree): Record = ${build('tree)}
  transparent inline def tuple(tree: Tree): NamedTuple.AnyNamedTuple = ${tuple('tree)}

object TitleRecords extends TreeProvider(List(t"title" -> Member.Value(t"text"))):
  transparent inline def record(tree: Tree): Record = ${build('tree)}
  transparent inline def tuple(tree: Tree): NamedTuple.AnyNamedTuple = ${tuple('tree)}

// Unions chosen by the value's kind, and keyed members read as maps
object ShapeRecords extends TreeProvider(List(
  t"id"     -> Member.Union(List(
    t"leaf" -> Member.Value(t"text"),
    t"node" -> Member.Record(List(t"name" -> Member.Value(t"text"))))),
  t"tags"   -> Member.Union(List(
    t"leaf"  -> Member.Value(t"text"),
    t"items" -> Member.Value(t"text").many)),
  t"sizes"  -> Member.Union(List(
    t"leaf"  -> Member.Value(t"length"),
    t"items" -> Member.Value(t"length").many)).optional,
  t"labels" -> Member.Value(t"text").keyed,
  t"owners" -> Member.Record(List(t"name" -> Member.Value(t"text"))).keyed,
  t"rows"   -> Member.Union(List(t"items" -> Member.Value(t"text").many)).many)):
  transparent inline def record(tree: Tree): Record = ${build('tree)}
  transparent inline def tuple(tree: Tree): NamedTuple.AnyNamedTuple = ${tuple('tree)}

// The remaining specifications are ill-formed: each compiles, but expanding its `record` macro
// fails, which the compiletime tests check.

object UnknownValueRecords extends TreeProvider(List(t"mystery" -> Member.Value(t"mystery"))):
  transparent inline def record(tree: Tree): Record = ${build('tree)}
  transparent inline def tuple(tree: Tree): NamedTuple.AnyNamedTuple = ${tuple('tree)}

// A member under each multiplicity, scalar and record
object MultiplicityRecords extends TreeProvider(List(
  t"nickname" -> Member.Value(t"text").optional,
  t"aliases"  -> Member.Value(t"text").many,
  t"partner"  -> Member.Record(List(t"name" -> Member.Value(t"text"))).optional,
  t"pets"     -> Member.Record(List(t"name" -> Member.Value(t"text"))).many)):
  transparent inline def record(tree: Tree): Record = ${build('tree)}
  transparent inline def tuple(tree: Tree): NamedTuple.AnyNamedTuple = ${tuple('tree)}

// Instances are keyed by the exact label: `"Text"` is not `"text"`
object MiscasedRecords extends TreeProvider(List(t"name" -> Member.Value(t"Text"))):
  transparent inline def record(tree: Tree): Record = ${build('tree)}
  transparent inline def tuple(tree: Tree): NamedTuple.AnyNamedTuple = ${tuple('tree)}
