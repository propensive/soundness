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
package ypsiloid

import soundness.*

import strategies.throwUnsafely

object PositionTests extends Suite(m"Ypsiloid position-index tests"):

  private def at(line: Int, column: Int, length: Int): Yaml.Ast.Position =
    Yaml.Ast.Position(line, column, length = length)

  // Tracking-on context — every test in this suite parses under
  // `Yaml.Tracking.On` so `.read[Yaml]` produces a tracked `Yaml`.
  private given Yaml.Tracking = Yaml.Tracking.On

  def run(): Unit =
    suite(m"Single-line root primitives"):
      test(m"Locate a root integer"):
        t"42".read[Yaml].locate(YamlPath())
      . assert(_ == at(1, 1, 2))

      test(m"Locate a root quoted string"):
        t""""hello"""".read[Yaml].locate(YamlPath())
      . assert(_ == at(1, 1, 7))

      test(m"Locate a root boolean"):
        t"true".read[Yaml].locate(YamlPath())
      . assert(_ == at(1, 1, 4))

      test(m"Locate a root null"):
        t"null".read[Yaml].locate(YamlPath())
      . assert(_ == at(1, 1, 4))

      test(m"Locate a negative number includes the minus sign in length"):
        t"-42".read[Yaml].locate(YamlPath())
      . assert(_ == at(1, 1, 3))

    suite(m"Flow mappings"):
      test(m"Locate the root flow mapping"):
        t"{a: 1, b: 2}".read[Yaml].locate(YamlPath())
      . assert(_ == at(1, 1, 12))

      test(m"Locate the value at key 'a'"):
        t"{a: 1, b: 2}".read[Yaml].locate(YamlPath()(t"a"))
      . assert(_ == at(1, 5, 1))

      test(m"Locate the value at key 'b'"):
        t"{a: 1, b: 2}".read[Yaml].locate(YamlPath()(t"b"))
      . assert(_ == at(1, 11, 1))

      test(m"Locate the key 'a' itself"):
        t"{a: 1, b: 2}".read[Yaml].locateKey(YamlPath()(t"a"))
      . assert(_ == at(1, 2, 1))

      test(m"Locate the key 'b' itself"):
        t"{a: 1, b: 2}".read[Yaml].locateKey(YamlPath()(t"b"))
      . assert(_ == at(1, 8, 1))

      test(m"Missing key returns Unset"):
        t"{a: 1}".read[Yaml].locate(YamlPath()(t"missing"))
      . assert(_ == Unset)

    suite(m"Flow sequences"):
      test(m"Locate the root flow sequence"):
        t"[10, 20, 30]".read[Yaml].locate(YamlPath())
      . assert(_ == at(1, 1, 12))

      test(m"Locate the first element"):
        t"[10, 20, 30]".read[Yaml].locate(YamlPath()(Prim))
      . assert(_ == at(1, 2, 2))

      test(m"Locate the last element"):
        t"[10, 20, 30]".read[Yaml].locate(YamlPath()(Ter))
      . assert(_ == at(1, 10, 2))

      test(m"Out-of-bounds index returns Unset"):
        t"[10, 20]".read[Yaml].locate(YamlPath()(Ter))
      . assert(_ == Unset)

      test(m"Empty flow sequence locates only the root"):
        t"[]".read[Yaml].locate(YamlPath())
      . assert(_ == at(1, 1, 2))

    suite(m"Block mappings"):
      test(m"Block mapping: first key value is on line 1"):
        val source = t"foo: 1\nbar: 2\n"
        source.read[Yaml].locate(YamlPath()(t"foo")).let(_.line)
      . assert(_ == 1)

      test(m"Block mapping: second key value is on line 2"):
        val source = t"foo: 1\nbar: 2\n"
        source.read[Yaml].locate(YamlPath()(t"bar")).let(_.line)
      . assert(_ == 2)

      test(m"Block mapping: first key starts at column 1"):
        val source = t"foo: 1\nbar: 2\n"
        source.read[Yaml].locateKey(YamlPath()(t"foo")).let(_.column)
      . assert(_ == 1)

      test(m"Block mapping: value column is past `key: `"):
        val source = t"foo: 1\nbar: 2\n"
        source.read[Yaml].locate(YamlPath()(t"foo")).let(_.column)
      . assert(_ == 6)

      test(m"Indented block mapping: inner key on the right line"):
        val source = t"outer:\n  inner: 99\n"
        source.read[Yaml].locate(YamlPath()(t"outer")(t"inner")).let(_.line)
      . assert(_ == 2)

      test(m"Indented block mapping: inner key past the indent"):
        val source = t"outer:\n  inner: 99\n"
        source.read[Yaml].locate(YamlPath()(t"outer")(t"inner")).let(_.column)
      . assert(_ == 10)

    suite(m"Block sequences"):
      test(m"Block sequence: third element on line 3"):
        val source = t"- 1\n- 2\n- 3\n"
        source.read[Yaml].locate(YamlPath()(Ter)).let(_.line)
      . assert(_ == 3)

      test(m"Block sequence: first element column past the dash"):
        val source = t"- 1\n- 2\n- 3\n"
        source.read[Yaml].locate(YamlPath()(Prim)).let(_.column)
      . assert(_ == 3)

      test(m"Block sequence inside block mapping"):
        val source = t"items:\n  - one\n  - two\n  - three\n"
        source.read[Yaml].locate(YamlPath()(t"items")(Sec)).let(_.line)
      . assert(_ == 3)

    suite(m"Subsequence property"):
      test(m"A nested mapping's descriptor slice has length == slot 0"):
        val yaml = t"{a: {b: 42}}".read[Yaml]
        val data = yaml.positionIndex.vouch.ints
        // Root composite header: [size, line, col, len, n=1, off_0, ...]
        // The single entry's offset is at data(5), entry = [keyLine,
        // keyColumn, keyLength, <valueDescriptor>]. The value descriptor
        // starts at offset(0) + 3 from the root descriptor.
        val firstEntryOff = data(5)
        val valueDescOff = firstEntryOff + 3
        val valueSize = data(valueDescOff)
        val slice = data.slice(valueDescOff, valueDescOff + valueSize)
        slice.length == valueSize
      . assert(identity)

    suite(m"Non-tracking mode"):
      test(m"Default Tracking.Off leaves positionIndex Unset"):
        // Locally override the suite-level `Tracking.On` to verify the
        // untracked path.
        given Yaml.Tracking = Yaml.Tracking.Off
        t"42".read[Yaml].positionIndex
      . assert(_ == Unset)
