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

import soundness.*

import strategies.throwUnsafely
import errorDiagnostics.stackTraces

case class Person(name: Text, age: Int) derives CanEqual
case class Inner(n: Int) derives CanEqual
case class Outer(inner: Inner) derives CanEqual
case class NamedOuter(name: Text, inner: Inner) derives CanEqual
case class WithDefault(name: Text, age: Int = 18) derives CanEqual

object Tests extends Suite(m"Ypsiloid Tests"):
  def run(): Unit =
    suite(m"Plain scalar parsing"):
      test(m"Parse a plain integer"):
        t"42".read[Yaml].as[Int]
      . assert(_ == 42)

      test(m"Parse a negative integer"):
        t"-99".read[Yaml].as[Int]
      . assert(_ == -99)

      test(m"Parse zero"):
        t"0".read[Yaml].as[Int]
      . assert(_ == 0)

      test(m"Parse a long"):
        t"1234567890123".read[Yaml].as[Long]
      . assert(_ == 1234567890123L)

      test(m"Parse a hexadecimal integer"):
        t"0x2A".read[Yaml].as[Int]
      . assert(_ == 42)

      test(m"Parse an octal integer"):
        t"0o17".read[Yaml].as[Int]
      . assert(_ == 15)

      test(m"Parse a float"):
        t"3.1415".read[Yaml].as[Float]
      . assert(_ == 3.1415f)

      test(m"Parse a double"):
        t"3.1415926".read[Yaml].as[Double]
      . assert(_ == 3.1415926)

      test(m"Parse a negative float"):
        t"-2.5".read[Yaml].as[Double]
      . assert(_ == -2.5)

      test(m"Parse positive infinity"):
        t".inf".read[Yaml].as[Double]
      . assert(_ == Double.PositiveInfinity)

      test(m"Parse negative infinity"):
        t"-.inf".read[Yaml].as[Double]
      . assert(_ == Double.NegativeInfinity)

      test(m"Parse NaN"):
        t".nan".read[Yaml].as[Double]
      . assert(_.isNaN)

      test(m"Parse true"):
        t"true".read[Yaml].as[Boolean]
      . assert(identity)

      test(m"Parse false"):
        t"false".read[Yaml].as[Boolean]
      . assert(!_)

      test(m"Parse null literal"):
        t"null".read[Yaml].as[Unit]
      . assert(_ == ())

      test(m"Parse tilde as null"):
        t"~".read[Yaml].as[Unit]
      . assert(_ == ())

      test(m"Parse empty document as YamlAst.Null"):
        t"".read[Yaml].root
      . assert(_ == YamlAst.Null)

      test(m"Parse a plain (unquoted) string"):
        t"hello".read[Yaml].as[Text]
      . assert(_ == t"hello")

      test(m"Plain string containing a number-like prefix is text"):
        t"3things".read[Yaml].as[Text]
      . assert(_ == t"3things")

    suite(m"Quoted strings"):
      test(m"Parse a single-quoted string"):
        t"'hello'".read[Yaml].as[Text]
      . assert(_ == t"hello")

      test(m"Parse a double-quoted string"):
        t"\"hello\"".read[Yaml].as[Text]
      . assert(_ == t"hello")

      test(m"Single-quoted string preserves backslashes"):
        t"'a\\b'".read[Yaml].as[Text]
      . assert(_ == t"a\\b")

      test(m"Double-quoted string with newline escape"):
        t"\"line1\\nline2\"".read[Yaml].as[Text]
      . assert(_ == t"line1\nline2")

      test(m"Double-quoted string with tab escape"):
        t"\"a\\tb\"".read[Yaml].as[Text]
      . assert(_ == t"a\tb")

      test(m"Double-quoted string with escaped backslash"):
        t"\"a\\\\b\"".read[Yaml].as[Text]
      . assert(_ == t"a\\b")

      test(m"Double-quoted string with escaped quote"):
        t"\"a\\\"b\"".read[Yaml].as[Text]
      . assert(_ == t"a\"b")

      test(m"Double-quoted string with unicode escape"):
        t"\"\\u00e9\"".read[Yaml].as[Text]
      . assert(_ == t"é")

      test(m"Double-quoted string with hex escape"):
        t"\"\\x41\"".read[Yaml].as[Text]
      . assert(_ == t"A")

      test(m"Single-quoted string with embedded apostrophe"):
        t"'don''t'".read[Yaml].as[Text]
      . assert(_ == t"don't")

      test(m"Hash inside a quoted string is not a comment"):
        t"\"a # b\"".read[Yaml].as[Text]
      . assert(_ == t"a # b")

      test(m"Empty single-quoted string"):
        t"''".read[Yaml].as[Text]
      . assert(_ == t"")

      test(m"Empty double-quoted string"):
        t"\"\"".read[Yaml].as[Text]
      . assert(_ == t"")

    suite(m"Flow sequences"):
      test(m"Parse an empty flow sequence"):
        t"[]".read[Yaml].as[List[Int]]
      . assert(_ == Nil)

      test(m"Parse a flow sequence of integers"):
        t"[1, 2, 3]".read[Yaml].as[List[Int]]
      . assert(_ == List(1, 2, 3))

      test(m"Parse a flow sequence of strings"):
        t"[alice, bob, carol]".read[Yaml].as[List[Text]]
      . assert(_ == List(t"alice", t"bob", t"carol"))

      test(m"Parse a flow sequence with mixed whitespace"):
        t"[ 1 ,  2  , 3 ]".read[Yaml].as[List[Int]]
      . assert(_ == List(1, 2, 3))

      test(m"Parse a nested flow sequence"):
        t"[[1, 2], [3, 4]]".read[Yaml].as[List[List[Int]]]
      . assert(_ == List(List(1, 2), List(3, 4)))

      test(m"Parse a flow sequence of quoted strings with commas"):
        t"[\"a,b\", \"c,d\"]".read[Yaml].as[List[Text]]
      . assert(_ == List(t"a,b", t"c,d"))

      test(m"Parse a flow sequence into a Vector"):
        t"[10, 20, 30]".read[Yaml].as[Vector[Int]]
      . assert(_ == Vector(10, 20, 30))

      test(m"Parse a flow sequence into a Set"):
        t"[1, 2, 3]".read[Yaml].as[Set[Int]]
      . assert(_ == Set(1, 2, 3))

      test(m"Empty flow sequence parses to YamlAst.Sequence with no items"):
        t"[]".read[Yaml].root match
          case YamlAst.Sequence(items) => items.length
          case _                       => -1
      . assert(_ == 0)

      test(m"Flow sequence parses to YamlAst.Sequence"):
        t"[1, 2, 3]".read[Yaml].root match
          case YamlAst.Sequence(items) => items.length
          case _                       => -1
      . assert(_ == 3)

    suite(m"Flow mappings"):
      test(m"Parse an empty flow mapping"):
        t"{}".read[Yaml].as[Map[Text, Int]]
      . assert(_.isEmpty)

      test(m"Parse a single-pair flow mapping"):
        t"{a: 1}".read[Yaml].as[Map[Text, Int]]
      . assert(_ == Map(t"a" -> 1))

      test(m"Parse a multi-pair flow mapping"):
        t"{a: 1, b: 2, c: 3}".read[Yaml].as[Map[Text, Int]]
      . assert(_ == Map(t"a" -> 1, t"b" -> 2, t"c" -> 3))

      test(m"Parse a flow mapping with text values"):
        t"{name: Alice, role: admin}".read[Yaml].as[Map[Text, Text]]
      . assert(_ == Map(t"name" -> t"Alice", t"role" -> t"admin"))

      test(m"Parse a nested flow mapping"):
        t"{outer: {inner: 7}}".read[Yaml].as[Map[Text, Map[Text, Int]]]
      . assert(_ == Map(t"outer" -> Map(t"inner" -> 7)))

      test(m"Parse a flow mapping with sequence value"):
        t"{xs: [1, 2, 3]}".read[Yaml].as[Map[Text, List[Int]]]
      . assert(_ == Map(t"xs" -> List(1, 2, 3)))

      test(m"Parse a flow mapping with quoted string values"):
        t"{a: \"hello\", b: \"world\"}".read[Yaml].as[Map[Text, Text]]
      . assert(_ == Map(t"a" -> t"hello", t"b" -> t"world"))

      test(m"Parse a flow mapping with quoted-string keys containing commas"):
        t"{\"a,b\": 1, \"c,d\": 2}".read[Yaml].as[Map[Text, Int]]
      . assert(_ == Map(t"a,b" -> 1, t"c,d" -> 2))

      test(m"Empty flow mapping parses to YamlAst.Mapping with no entries"):
        t"{}".read[Yaml].root match
          case YamlAst.Mapping(entries) => entries.length
          case _                        => -1
      . assert(_ == 0)

      test(m"Flow mapping parses to YamlAst.Mapping"):
        t"{a: 1, b: 2}".read[Yaml].root match
          case YamlAst.Mapping(entries) => entries.length
          case _                        => -1
      . assert(_ == 2)

    suite(m"Case-class derivation"):
      test(m"Decode a flat case class from a flow mapping"):
        t"{name: Alice, age: 30}".read[Yaml].as[Person]
      . assert(_ == Person(t"Alice", 30))

      test(m"Decode a flat case class with quoted strings"):
        t"{name: \"Bob Smith\", age: 25}".read[Yaml].as[Person]
      . assert(_ == Person(t"Bob Smith", 25))

      test(m"Decode a nested case class"):
        t"{inner: {n: 42}}".read[Yaml].as[Outer]
      . assert(_ == Outer(Inner(42)))

      test(m"Decode a deeper nested case class"):
        t"{name: hello, inner: {n: 7}}".read[Yaml].as[NamedOuter]
      . assert(_ == NamedOuter(t"hello", Inner(7)))

      test(m"Decode a sequence of case classes"):
        t"[{name: Alice, age: 30}, {name: Bob, age: 25}]".read[Yaml].as[List[Person]]
      . assert(_ == List(Person(t"Alice", 30), Person(t"Bob", 25)))

      test(m"Decode a case class with a default field omitted"):
        t"{name: Eve}".read[Yaml].as[WithDefault]
      . assert(_ == WithDefault(t"Eve", 18))

      test(m"Decode a case class with all fields supplied"):
        t"{name: Eve, age: 99}".read[Yaml].as[WithDefault]
      . assert(_ == WithDefault(t"Eve", 99))

    suite(m"Block sequences"):
      test(m"Parse a block sequence of integers"):
        t"- 1\n- 2\n- 3".read[Yaml].as[List[Int]]
      . assert(_ == List(1, 2, 3))

      test(m"Parse a block sequence of strings"):
        t"- alice\n- bob".read[Yaml].as[List[Text]]
      . assert(_ == List(t"alice", t"bob"))

      test(m"Parse a block sequence with quoted strings"):
        t"- \"hello world\"\n- 'goodbye'".read[Yaml].as[List[Text]]
      . assert(_ == List(t"hello world", t"goodbye"))

      test(m"Block sequence parses to YamlAst.Sequence"):
        t"- 1\n- 2".read[Yaml].root match
          case YamlAst.Sequence(items) => items.length
          case _                       => -1
      . assert(_ == 2)

      test(m"Parse a block sequence with leading and trailing blank lines"):
        t"\n- 1\n- 2\n".read[Yaml].as[List[Int]]
      . assert(_ == List(1, 2))

      test(m"Parse a block sequence with comments interleaved"):
        t"# comment\n- 1\n# more\n- 2".read[Yaml].as[List[Int]]
      . assert(_ == List(1, 2))

    suite(m"Block mappings"):
      test(m"Parse a single-pair block mapping"):
        t"name: Alice".read[Yaml].as[Map[Text, Text]]
      . assert(_ == Map(t"name" -> t"Alice"))

      test(m"Parse a multi-pair block mapping"):
        t"a: 1\nb: 2\nc: 3".read[Yaml].as[Map[Text, Int]]
      . assert(_ == Map(t"a" -> 1, t"b" -> 2, t"c" -> 3))

      test(m"Parse a block mapping into a case class"):
        t"name: Alice\nage: 30".read[Yaml].as[Person]
      . assert(_ == Person(t"Alice", 30))

      test(m"Parse a nested block mapping"):
        t"inner:\n  n: 42".read[Yaml].as[Outer]
      . assert(_ == Outer(Inner(42)))

      test(m"Parse a deeper nested block mapping"):
        t"name: hello\ninner:\n  n: 7".read[Yaml].as[NamedOuter]
      . assert(_ == NamedOuter(t"hello", Inner(7)))

      test(m"Parse a block mapping containing a block sequence"):
        t"items:\n  - 1\n  - 2\n  - 3".read[Yaml].as[Map[Text, List[Int]]]
      . assert(_ == Map(t"items" -> List(1, 2, 3)))

      test(m"Parse a block mapping containing a flow sequence"):
        t"xs: [1, 2, 3]".read[Yaml].as[Map[Text, List[Int]]]
      . assert(_ == Map(t"xs" -> List(1, 2, 3)))

      test(m"Parse a block sequence of case classes"):
        t"- name: Alice\n  age: 30\n- name: Bob\n  age: 25".read[Yaml].as[List[Person]]
      . assert(_ == List(Person(t"Alice", 30), Person(t"Bob", 25)))

      test(m"Block mapping parses to YamlAst.Mapping"):
        t"a: 1\nb: 2".read[Yaml].root match
          case YamlAst.Mapping(entries) => entries.length
          case _                        => -1
      . assert(_ == 2)

      test(m"Block mapping with comments interleaved"):
        t"a: 1\n# comment\nb: 2".read[Yaml].as[Map[Text, Int]]
      . assert(_ == Map(t"a" -> 1, t"b" -> 2))

    suite(m"Block scalars"):
      test(m"Literal block scalar preserves newlines"):
        t"text: |\n  line1\n  line2".read[Yaml].as[Map[Text, Text]]
      . assert(_ == Map(t"text" -> t"line1\nline2\n"))

      test(m"Folded block scalar joins lines with spaces"):
        t"text: >\n  line1\n  line2".read[Yaml].as[Map[Text, Text]]
      . assert(_ == Map(t"text" -> t"line1 line2\n"))

      test(m"Literal scalar with strip indicator drops trailing newline"):
        t"text: |-\n  line1\n  line2".read[Yaml].as[Map[Text, Text]]
      . assert(_ == Map(t"text" -> t"line1\nline2"))

      test(m"Folded scalar with strip indicator drops trailing newline"):
        t"text: >-\n  line1\n  line2".read[Yaml].as[Map[Text, Text]]
      . assert(_ == Map(t"text" -> t"line1 line2"))

      test(m"Literal block scalar in a sequence item"):
        t"- |\n  line1\n  line2".read[Yaml].as[List[Text]]
      . assert(_ == List(t"line1\nline2\n"))

      test(m"Block mapping with literal scalar then another field"):
        val yaml = t"a: |\n  hello\n  world\nb: tail"
        yaml.read[Yaml].as[Map[Text, Text]]
      . assert(_ == Map(t"a" -> t"hello\nworld\n", t"b" -> t"tail"))

      test(m"Single-line literal block scalar"):
        t"text: |\n  only".read[Yaml].as[Map[Text, Text]]
      . assert(_ == Map(t"text" -> t"only\n"))

      test(m"Single-line literal scalar with strip"):
        t"text: |-\n  only".read[Yaml].as[Map[Text, Text]]
      . assert(_ == Map(t"text" -> t"only"))

    suite(m"Comment handling"):
      test(m"Comment after a scalar is ignored"):
        t"42 # the answer".read[Yaml].as[Int]
      . assert(_ == 42)

      test(m"Standalone comment line is ignored"):
        t"# a comment\n42".read[Yaml].as[Int]
      . assert(_ == 42)

    suite(m"Whitespace"):
      test(m"Leading and trailing newlines are ignored"):
        t"\n\n42\n\n".read[Yaml].as[Int]
      . assert(_ == 42)

      test(m"Trailing spaces on a scalar line are stripped"):
        t"hello   ".read[Yaml].as[Text]
      . assert(_ == t"hello")

    suite(m"Direct AST inspection"):
      test(m"Plain integer parses to YamlAst.Integer"):
        t"42".read[Yaml].root
      . assert(_ == YamlAst.Integer(42L))

      test(m"Float parses to YamlAst.Decimal"):
        t"3.14".read[Yaml].root
      . assert(_ == YamlAst.Decimal(3.14))

      test(m"Boolean parses to YamlAst.Bool"):
        t"true".read[Yaml].root
      . assert(_ == YamlAst.Bool(true))

      test(m"Null parses to YamlAst.Null"):
        t"null".read[Yaml].root
      . assert(_ == YamlAst.Null)

      test(m"Tilde parses to YamlAst.Null"):
        t"~".read[Yaml].root
      . assert(_ == YamlAst.Null)

      test(m"Plain string parses to YamlAst.Str"):
        t"hello".read[Yaml].root
      . assert(_ == YamlAst.Str(t"hello"))

    suite(m"Type errors"):
      test(m"Decoding a string as Int raises a YamlError"):
        capture[YamlError](t"hello".read[Yaml].as[Int])
      . assert(_ => true)

      test(m"Decoding a number as Boolean raises a YamlError"):
        capture[YamlError](t"42".read[Yaml].as[Boolean])
      . assert(_ => true)
