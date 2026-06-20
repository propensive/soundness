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
import errorDiagnostics.stackTracesDiagnostics

case class Person(name: Text, age: Int) derives CanEqual
case class Inner(n: Int) derives CanEqual
case class Outer(inner: Inner) derives CanEqual
case class NamedOuter(name: Text, inner: Inner) derives CanEqual
case class WithDefault(name: Text, age: Int = 18) derives CanEqual
case class WithOption(name: Text, age: Option[Int]) derives CanEqual
case class YRenamed(@name[Yaml](t"full_name") fullName: Text, @name(t"yob") year: Int)
derives CanEqual

enum Shape derives CanEqual:
  case Circle(radius: Double)
  case Square(side: Double)
  case Triangle(a: Double, b: Double, c: Double)

enum YStatus derives CanEqual:
  @name[Yaml](t"ok") case Active(since: Int)
  @name(t"gone")     case Removed(at: Int)
                     case Pending(at: Int)

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

      test(m"Parse empty document as Yaml.Ast.Null"):
        Yaml.unseal(t"".read[Yaml])
      . assert(_ == Yaml.Ast.Null)

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

      test(m"Multi-line double-quoted string folds newline to space"):
        t"\"first\n  second\"".read[Yaml].as[Text]
      . assert(_ == t"first second")

      test(m"Multi-line single-quoted string folds newline to space"):
        t"'first\n  second'".read[Yaml].as[Text]
      . assert(_ == t"first second")

      test(m"Multi-line double-quoted string with three lines"):
        t"\"a\n  b\n  c\"".read[Yaml].as[Text]
      . assert(_ == t"a b c")

      test(m"Multi-line double-quoted with empty line preserves a single newline"):
        t"\"first\n\n  second\"".read[Yaml].as[Text]
      . assert(_ == t"first\nsecond")

      test(m"Multi-line double-quoted string with three blank lines yields two newlines"):
        t"\"a\n\n\n  b\"".read[Yaml].as[Text]
      . assert(_ == t"a\n\nb")

      test(m"Multi-line double-quoted string as block-mapping value"):
        t"key: \"first\n  second\"".read[Yaml].as[Map[Text, Text]]
      . assert(_ == Map(t"key" -> t"first second"))

      test(m"Multi-line single-quoted string as flow-sequence element"):
        t"['first\n  second', other]".read[Yaml].as[List[Text]]
      . assert(_ == List(t"first second", t"other"))

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

      test(m"Parse a flow sequence into a Series"):
        t"[10, 20, 30]".read[Yaml].as[Series[Int]]
      . assert(_ == Series(10, 20, 30))

      test(m"Parse a flow sequence into a Set"):
        t"[1, 2, 3]".read[Yaml].as[Set[Int]]
      . assert(_ == Set(1, 2, 3))

      test(m"Empty flow sequence parses to Yaml.Ast.Sequence with no items"):
        Yaml.unseal(t"[]".read[Yaml]) match
          case Yaml.Ast.Sequence(items) => items.length
          case _                       => -1
      . assert(_ == 0)

      test(m"Flow sequence parses to Yaml.Ast.Sequence"):
        Yaml.unseal(t"[1, 2, 3]".read[Yaml]) match
          case Yaml.Ast.Sequence(items) => items.length
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

      test(m"Empty flow mapping parses to Yaml.Ast.Mapping with no entries"):
        Yaml.unseal(t"{}".read[Yaml]) match
          case Yaml.Ast.Mapping(entries) => entries.length
          case _                        => -1
      . assert(_ == 0)

      test(m"Flow mapping parses to Yaml.Ast.Mapping"):
        Yaml.unseal(t"{a: 1, b: 2}".read[Yaml]) match
          case Yaml.Ast.Mapping(entries) => entries.length
          case _                        => -1
      . assert(_ == 2)

    suite(m"`in Yaml` decoder shorthand"):
      test(m"`read[T in Yaml]` resolves a value directly from text"):
        t"{name: Alice, age: 30}".read[Person in Yaml]
      . assert(_ == Person(t"Alice", 30))

      test(m"`read[T in Yaml]` works for nested case classes"):
        t"{inner: {n: 7}}".read[Outer in Yaml]
      . assert(_ == Outer(Inner(7)))

      test(m"`read[List[T] in Yaml]` decodes a sequence directly"):
        t"[{name: Alice, age: 30}, {name: Bob, age: 25}]".read[List[Person] in Yaml]
      . assert(_ == List(Person(t"Alice", 30), Person(t"Bob", 25)))

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

      test(m"Decode honours @name[Yaml] and bare @name keys"):
        t"{full_name: Ann, yob: 1984}".read[Yaml].as[YRenamed]
      . assert(_ == YRenamed(t"Ann", 1984))

      test(m"@name renames round-trip"):
        YRenamed(t"Ann", 1984).yaml.as[YRenamed]
      . assert(_ == YRenamed(t"Ann", 1984))

      test(m"Decode a present Option field"):
        t"{name: Frank, age: 40}".read[Yaml].as[WithOption]
      . assert(_ == WithOption(t"Frank", Some(40)))

      test(m"Decode a missing Option field as None"):
        t"{name: Grace}".read[Yaml].as[WithOption]
      . assert(_ == WithOption(t"Grace", None))

      test(m"Decode an explicitly null Option field as None"):
        t"{name: Hank, age: null}".read[Yaml].as[WithOption]
      . assert(_ == WithOption(t"Hank", None))

      test(m"Decode Option[Int] from a top-level integer"):
        t"42".read[Yaml].as[Option[Int]]
      . assert(_ == Some(42))

      test(m"Decode Option[Int] from null"):
        t"null".read[Yaml].as[Option[Int]]
      . assert(_ == None)

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

      test(m"Block sequence parses to Yaml.Ast.Sequence"):
        Yaml.unseal(t"- 1\n- 2".read[Yaml]) match
          case Yaml.Ast.Sequence(items) => items.length
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

      test(m"Block mapping parses to Yaml.Ast.Mapping"):
        Yaml.unseal(t"a: 1\nb: 2".read[Yaml]) match
          case Yaml.Ast.Mapping(entries) => entries.length
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

      test(m"Literal scalar with explicit indent indicator"):
        t"text: |2\n   indented".read[Yaml].as[Map[Text, Text]]
      . assert(_ == Map(t"text" -> t" indented\n"))

      test(m"Folded scalar with explicit indent indicator"):
        t"text: >2\n   indented\n   more".read[Yaml].as[Map[Text, Text]]
      . assert(_ == Map(t"text" -> t" indented\n more\n"))

      test(m"Literal scalar with explicit indent then strip"):
        t"text: |2-\n   indented".read[Yaml].as[Map[Text, Text]]
      . assert(_ == Map(t"text" -> t" indented"))

      test(m"Literal scalar with strip then explicit indent (chomp first)"):
        t"text: |-2\n   indented".read[Yaml].as[Map[Text, Text]]
      . assert(_ == Map(t"text" -> t" indented"))

    suite(m"Tags"):
      test(m"!!str forces a number-looking value to a string"):
        t"!!str 42".read[Yaml].as[Text]
      . assert(_ == t"42")

      test(m"!!str on an already-quoted string is a no-op"):
        t"!!str \"hello\"".read[Yaml].as[Text]
      . assert(_ == t"hello")

      test(m"!!int forces a quoted-string value to an integer"):
        t"!!int \"42\"".read[Yaml].as[Int]
      . assert(_ == 42)

      test(m"!!float forces an integer value to a decimal"):
        t"!!float 7".read[Yaml].as[Double]
      . assert(_ == 7.0)

      test(m"!!float forces a quoted-string value to a decimal"):
        t"!!float \"3.14\"".read[Yaml].as[Double]
      . assert(_ == 3.14)

      test(m"!!bool forces a quoted-string value to a boolean"):
        t"!!bool \"true\"".read[Yaml].as[Boolean]
      . assert(identity)

      test(m"!!null tags any value as null"):
        Yaml.unseal(t"!!null whatever".read[Yaml])
      . assert(_ == Yaml.Ast.Null)

      test(m"Unknown tag passes the value through unchanged"):
        t"!myTag 42".read[Yaml].as[Int]
      . assert(_ == 42)

      test(m"!!str on a tagged block-mapping value with continuation"):
        t"key: !!str\n  hello world".read[Yaml].as[Map[Text, Text]]
      . assert(_ == Map(t"key" -> t"hello world"))

      test(m"!!str on a tagged block-mapping value with multi-line content"):
        t"key: !!str\n  first\n  second".read[Yaml].as[Map[Text, Text]]
      . assert(_ == Map(t"key" -> t"first second"))

    suite(m"Anchors and aliases"):
      test(m"Alias resolves to anchored scalar"):
        t"a: &x 1\nb: *x".read[Yaml].as[Map[Text, Int]]
      . assert(_ == Map(t"a" -> 1, t"b" -> 1))

      test(m"Alias resolves to anchored flow sequence"):
        t"a: &xs [1, 2, 3]\nb: *xs".read[Yaml].as[Map[Text, List[Int]]]
      . assert(_ == Map(t"a" -> List(1, 2, 3), t"b" -> List(1, 2, 3)))

      test(m"Alias resolves to anchored flow mapping"):
        t"a: &m {n: 1}\nb: *m".read[Yaml].as[Map[Text, Inner]]
      . assert(_ == Map(t"a" -> Inner(1), t"b" -> Inner(1)))

      test(m"Alias resolves to anchored block mapping"):
        t"defaults: &d\n  n: 7\nuse: *d".read[Yaml].as[Map[Text, Inner]]
      . assert(_ == Map(t"defaults" -> Inner(7), t"use" -> Inner(7)))

      test(m"Alias resolves to anchored block sequence"):
        t"a: &xs\n  - 1\n  - 2\nb: *xs".read[Yaml].as[Map[Text, List[Int]]]
      . assert(_ == Map(t"a" -> List(1, 2), t"b" -> List(1, 2)))

      test(m"Alias resolves to anchored string"):
        t"a: &name Alice\nb: *name".read[Yaml].as[Map[Text, Text]]
      . assert(_ == Map(t"a" -> t"Alice", t"b" -> t"Alice"))

      test(m"Unknown alias raises a ParseError"):
        capture[ParseError](t"a: *missing".read[Yaml].as[Map[Text, Int]])
      . assert(_ => true)

    suite(m"Multi-document streams"):
      test(m"Parse a single document with explicit start marker"):
        t"---\n42".read[Yaml].as[Int]
      . assert(_ == 42)

      test(m"Parse a single document with start and end markers"):
        t"---\n42\n...".read[Yaml].as[Int]
      . assert(_ == 42)

      test(m"Stream of three documents"):
        t"---\n1\n---\n2\n---\n3".read[List[Yaml]].map(_.as[Int])
      . assert(_ == List(1, 2, 3))

      test(m"Empty stream yields no documents"):
        t"".read[List[Yaml]].length
      . assert(_ == 0)

      test(m"Stream of mixed-type documents"):
        t"---\nname: Alice\n---\n[1, 2, 3]".read[List[Yaml]].length
      . assert(_ == 2)

      test(m"Single-document stream without leading separator"):
        t"42".read[List[Yaml]].map(_.as[Int])
      . assert(_ == List(42))

      test(m"Stream with trailing end marker"):
        t"1\n---\n2\n...".read[List[Yaml]].map(_.as[Int])
      . assert(_ == List(1, 2))

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
      test(m"Plain integer parses to Yaml.Ast.Integer"):
        Yaml.unseal(t"42".read[Yaml])
      . assert(_ == Yaml.Ast.Integer(42L))

      test(m"Float parses to Yaml.Ast.Decimal"):
        Yaml.unseal(t"3.14".read[Yaml])
      . assert(_ == Yaml.Ast.Decimal(3.14))

      test(m"Boolean parses to Yaml.Ast.Bool"):
        Yaml.unseal(t"true".read[Yaml])
      . assert(_ == Yaml.Ast.Bool(true))

      test(m"Null parses to Yaml.Ast.Null"):
        Yaml.unseal(t"null".read[Yaml])
      . assert(_ == Yaml.Ast.Null)

      test(m"Tilde parses to Yaml.Ast.Null"):
        Yaml.unseal(t"~".read[Yaml])
      . assert(_ == Yaml.Ast.Null)

      test(m"Plain string parses to Yaml.Ast.Str"):
        Yaml.unseal(t"hello".read[Yaml])
      . assert(_ == Yaml.Ast.Str(t"hello"))

    suite(m"AST equality"):
      test(m"Two equal sequence Yaml.Asts compare equal via Yaml"):
        Yaml.Ast.deepEquals
                ( Yaml.unseal(t"[1, 2, 3]".read[Yaml]),
                  Yaml.unseal(t"[1, 2, 3]".read[Yaml]) )
      . assert(identity)

      test(m"Two equal mapping Yaml.Asts compare equal via Yaml"):
        Yaml.Ast.deepEquals
                ( Yaml.unseal(t"{a: 1, b: 2}".read[Yaml]),
                  Yaml.unseal(t"{a: 1, b: 2}".read[Yaml]) )
      . assert(identity)

      test(m"Two equal Yaml documents compare equal"):
        t"{a: [1, 2], b: 3}".read[Yaml] == t"{a: [1, 2], b: 3}".read[Yaml]
      . assert(identity)

      test(m"Different sequences compare unequal"):
        t"[1, 2, 3]".read[Yaml] == t"[1, 2, 4]".read[Yaml]
      . assert(!_)

      test(m"Equal sequences hash to the same value"):
        t"[1, 2, 3]".read[Yaml].hashCode == t"[1, 2, 3]".read[Yaml].hashCode
      . assert(identity)

    suite(m"Type errors"):
      test(m"Decoding a string as Int raises a YamlError"):
        capture[YamlError](t"hello".read[Yaml].as[Int])
      . assert(_ => true)

      test(m"Decoding a number as Boolean raises a YamlError"):
        capture[YamlError](t"42".read[Yaml].as[Boolean])
      . assert(_ => true)

    suite(m"Encodable derivation"):
      test(m"Encode an Int"):
        42.yaml.as[Int]
      . assert(_ == 42)

      test(m"Encode a Long"):
        42L.yaml.as[Long]
      . assert(_ == 42L)

      test(m"Encode a Double"):
        3.14.yaml.as[Double]
      . assert(_ == 3.14)

      test(m"Encode a Boolean"):
        true.yaml.as[Boolean]
      . assert(identity)

      test(m"Encode a Text"):
        t"hello".yaml.as[Text]
      . assert(_ == t"hello")

      test(m"Encode a List"):
        List(1, 2, 3).yaml.as[List[Int]]
      . assert(_ == List(1, 2, 3))

      test(m"Encode a Map"):
        Map(t"a" -> 1, t"b" -> 2).yaml.as[Map[Text, Int]]
      . assert(_ == Map(t"a" -> 1, t"b" -> 2))

      test(m"Encode Some(value)"):
        (Some(42): Option[Int]).yaml.as[Option[Int]]
      . assert(_ == Some(42))

      test(m"Encode None to absent and decode back"):
        WithOption(t"x", None).yaml.as[WithOption]
      . assert(_ == WithOption(t"x", None))

      test(m"Round-trip a simple case class"):
        Person(t"Alice", 30).yaml.as[Person]
      . assert(_ == Person(t"Alice", 30))

      test(m"Round-trip a nested case class"):
        NamedOuter(t"x", Inner(7)).yaml.as[NamedOuter]
      . assert(_ == NamedOuter(t"x", Inner(7)))

      test(m"Round-trip a list of case classes"):
        List(Person(t"A", 1), Person(t"B", 2)).yaml.as[List[Person]]
      . assert(_ == List(Person(t"A", 1), Person(t"B", 2)))

      test(m"Encoded case class produces a Yaml.Ast.Mapping"):
        Person(t"Alice", 30).yaml.root match
          case Yaml.Ast.Mapping(entries) => entries.length
          case _                        => -1
      . assert(_ == 2)

      test(m"Encoded list produces a Yaml.Ast.Sequence"):
        List(1, 2, 3).yaml.root match
          case Yaml.Ast.Sequence(items) => items.length
          case _                       => -1
      . assert(_ == 3)

      test(m"Encoding skips Optional fields that are Unset/None"):
        WithOption(t"x", None).yaml.root match
          case Yaml.Ast.Mapping(entries) => entries.length
          case _                        => -1
      . assert(_ == 1)

    suite(m"Sum-type derivation"):
      import discriminables.yamlByTypeDiscriminable

      test(m"Round-trip a sum-type variant (Circle)"):
        val shape: Shape = Shape.Circle(2.0)
        shape.yaml.as[Shape]
      . assert(_ == Shape.Circle(2.0))

      test(m"Round-trip a sum-type variant (Square)"):
        val shape: Shape = Shape.Square(5.0)
        shape.yaml.as[Shape]
      . assert(_ == Shape.Square(5.0))

      test(m"Round-trip a multi-field variant (Triangle)"):
        val shape: Shape = Shape.Triangle(3.0, 4.0, 5.0)
        shape.yaml.as[Shape]
      . assert(_ == Shape.Triangle(3.0, 4.0, 5.0))

      test(m"Encoded sum-type variant carries the `type` discriminator"):
        val shape: Shape = Shape.Circle(2.0)
        shape.yaml.root match
          case Yaml.Ast.Mapping(entries) =>
            entries.collectFirst:
              case (Yaml.Ast.Str(k), Yaml.Ast.Str(v)) if k == t"type" => v.s
            . getOrElse("none")
          case _ => "none"
      . assert(_ == "Circle")

      test(m"@name renames a variant's `type` discriminator"):
        (YStatus.Active(5): YStatus).yaml.root match
          case Yaml.Ast.Mapping(entries) =>
            entries.collectFirst:
              case (Yaml.Ast.Str(k), Yaml.Ast.Str(v)) if k == t"type" => v.s
            . getOrElse("none")
          case _ => "none"
      . assert(_ == "ok")

      test(m"@name variants round-trip"):
        List(YStatus.Active(5), YStatus.Removed(9), YStatus.Pending(1)).map(_.yaml.as[YStatus])
      . assert(_ == List(YStatus.Active(5), YStatus.Removed(9), YStatus.Pending(1)))

      test(m"Decode a sum-type variant from a flow mapping"):
        t"{type: Circle, radius: 2.5}".read[Yaml].as[Shape]
      . assert(_ == Shape.Circle(2.5))

      test(m"Decode a sum-type variant from a block mapping"):
        t"type: Square\nside: 7.0".read[Yaml].as[Shape]
      . assert(_ == Shape.Square(7.0))

      test(m"Round-trip a list of sum-type variants"):
        val shapes: List[Shape] = List(Shape.Circle(1.0), Shape.Square(2.0))
        shapes.yaml.as[List[Shape]]
      . assert(_ == List(Shape.Circle(1.0), Shape.Square(2.0)))

    suite(m"Direct accessors"):
      test(m"Index a sequence by integer"):
        val ys = t"[10, 20, 30]".read[Yaml]
        ys(1).as[Int]
      . assert(_ == 20)

      test(m"Look up a mapping field by Text"):
        val m = t"{name: Alice, age: 30}".read[Yaml]
        m(t"age").as[Int]
      . assert(_ == 30)

      test(m"Missing mapping field decodes as None via Option"):
        val m = t"{name: Alice}".read[Yaml]
        m(t"age").as[Option[Int]]
      . assert(_ == None)

      test(m"Indexing a non-sequence raises a YamlError"):
        val y = t"42".read[Yaml]
        capture[YamlError](y(0))
      . assert(_ => true)

    suite(m"Dynamic access"):
      import dynamicYamlAccess.enabled

      test(m"Read a field via selectDynamic"):
        val y = t"{name: Alice, age: 30}".read[Yaml]
        y.name.as[Text]
      . assert(_ == t"Alice")

      test(m"Read a nested field by chaining"):
        val y = t"outer: {inner: {n: 7}}".read[Yaml]
        y.outer.inner.n.as[Int]
      . assert(_ == 7)

      test(m"Read a sequence-valued field by index"):
        val y = t"items: [10, 20, 30]".read[Yaml]
        y.items(1).as[Int]
      . assert(_ == 20)

      test(m"Update a mapping field dynamically"):
        val y = t"{name: Alice, age: 30}".read[Yaml]
        val updated = y.age = 31
        updated.as[Person]
      . assert(_ == Person(t"Alice", 31))

      test(m"Add a new field via dynamic assignment"):
        val y = t"{name: Alice}".read[Yaml]
        val updated = y.age = 18
        updated.as[Person]
      . assert(_ == Person(t"Alice", 18))

      test(m"Delete a field by assigning Unset"):
        val y = t"{name: Alice, age: 30}".read[Yaml]
        val updated = y.age = Unset
        updated.as[WithDefault]
      . assert(_ == WithDefault(t"Alice", 18))

      test(m"Update a sequence element by index"):
        val y = t"[1, 2, 3]".read[Yaml]
        val updated = y(1) = 5
        updated.as[List[Int]]
      . assert(_ == List(1, 5, 3))

    suite(m"Yaml.make construction"):
      test(m"Yaml.make with one field"):
        Yaml.make(name = t"Anna".yaml).as[Map[Text, Text]]
      . assert(_ == Map(t"name" -> t"Anna"))

      test(m"Yaml.make with multiple fields"):
        Yaml.make(name = t"Anna".yaml, age = 30.yaml).as[Person]
      . assert(_ == Person(t"Anna", 30))

      test(m"Nested Yaml.make"):
        Yaml.make(inner = Yaml.make(n = 7.yaml)).as[Outer]
      . assert(_ == Outer(Inner(7)))

    suite(m"Bytes decoder"):
      test(m"Decode a numeric value as Bytes"):
        t"255".read[Yaml].as[Bytes]
      . assert(_ == 255L.b)

    suite(m"BCD arbitrary-precision numbers"):
      test(m"21-digit integer parses as BCD"):
        Yaml.unseal(t"123456789012345678901".read[Yaml]).isBcd
      . assert(identity)

      test(m"Long-range integer still parses as Long"):
        Yaml.unseal(t"123456789012345".read[Yaml]).isLong
      . assert(identity)

      test(m"Decimal with > 17 significant digits parses as BCD"):
        Yaml.unseal(t"3.14159265358979323846".read[Yaml]).isBcd
      . assert(identity)

      test(m"Decimal with low precision still parses as Double"):
        Yaml.unseal(t"3.14".read[Yaml]).isDouble
      . assert(identity)

      test(m"BCD round-trips through .as[BigDecimal] when Decodable provided"):
        // No BCD-specific decoder yet, but the BCD value is reachable
        // via the AST extensions for callers who need full precision.
        val ast = Yaml.unseal(t"99999999999999999999999999".read[Yaml])
        ast.isBcd && {
          import strategies.throwUnsafely
          ast.bcd.toBigDecimal == BigDecimal("99999999999999999999999999")
        }
      . assert(identity)

      test(m"isNumber accepts Long, Double, and BCD"):
        val l = Yaml.unseal(t"42".read[Yaml]).isNumber
        val d = Yaml.unseal(t"3.14".read[Yaml]).isNumber
        val b = Yaml.unseal(t"123456789012345678901".read[Yaml]).isNumber
        l && d && b
      . assert(identity)

    suite(m"y\"...\" interpolator"):
      test(m"Interpolate a simple scalar"):
        val v = 42
        y"$v".as[Int]
      . assert(_ == 42)

      test(m"Interpolate a value into a mapping"):
        val name = t"Alice"
        y"name: $name".as[Map[Text, Text]]
      . assert(_ == Map(t"name" -> t"Alice"))

      test(m"Interpolate a value into a flow sequence"):
        val a = 1
        val b = 2
        y"[$a, $b, 3]".as[List[Int]]
      . assert(_ == List(1, 2, 3))

      test(m"Interpolate two values into a mapping"):
        val name = t"Alice"
        val age = 30
        y"""
          name: $name
          age: $age
        """.as[Person]
      . assert(_ == Person(t"Alice", 30))

      test(m"Interpolate inside a string-valued field"):
        val name = t"Alice"
        y"greeting: hello $name".as[Map[Text, Text]]
      . assert(_ == Map(t"greeting" -> t"hello Alice"))

    suite(m"y\"...\" extractor"):
      test(m"Match a literal flow mapping"):
        t"{a: 1, b: 2}".read[Yaml] match
          case y"{a: 1, b: 2}" => true
          case _               => false
      . assert(identity)

      test(m"Capture a single value"):
        t"name: Alice".read[Yaml] match
          case y"name: $n" => Yaml.unseal(n) == Yaml.Ast.Str(t"Alice")
          case _           => false
      . assert(identity)

    suite(m"YamlPath"):
      test(m"Empty path encodes with #"):
        YamlPath().encode.contains(t"#")
      . assert(identity)

      test(m"Path with one segment includes the segment"):
        YamlPath()(t"foo").encode.contains(t"foo")
      . assert(identity)

      test(m"Path with multiple segments includes all"):
        val p = YamlPath()(t"a")(t"b")(t"c").encode
        p.contains(t"a") && p.contains(t"b") && p.contains(t"c")
      . assert(identity)

      test(m"Path escapes ~ as ~0 in segment"):
        val p = YamlPath()(t"a~b").encode
        p.contains(t"~0")
      . assert(identity)

      test(m"Path escapes / as ~1 in segment"):
        val p = YamlPath()(t"a/b").encode
        p.contains(t"~1")
      . assert(identity)

      test(m"Path with ordinal segment encodes the index"):
        val p = YamlPath()(Prim).encode
        p.contains(t"0")
      . assert(identity)

      test(m"YamlPathError reason describes itself"):
        val err = YamlPathError(YamlPathError.Reason.UnknownDocument, 0)
        err.message.text.s.contains("registry")
      . assert(identity)

    suite(m"yp\"...\" interpolator"):
      test(m"a same-document path parses"):
        yp"#/foo/bar".encode
      . assert(_ == t"#/foo/bar")

      test(m"the whole-document path parses"):
        yp"#".encode
      . assert(_ == t"#")

      test(m"a path not beginning with '#' is rejected at the first character"):
        demilitarize:
          yp"/foo/bar"
        . map(_.focus)
      . assert(_ == List("/"))

      test(m"a malformed '~' escape is rejected at the '~'"):
        demilitarize:
          yp"#/foo~2bar"
        . map(_.focus)
      . assert(_ == List("~"))

    suite(m"Lens"):
      import dynamicYamlAccess.enabled, yamlConversion.encodable

      val org = Yaml.ast(NamedOuter(t"a", Inner(7)).yaml.root)

      test(m"Lens update on a nested mapping"):
        val updated = org.lens(_.inner.n = 99.yaml)
        updated.as[NamedOuter]
      . assert(_ == NamedOuter(t"a", Inner(99)))

      test(m"Lens update of top-level field"):
        val updated = org.lens(_.name = t"b")
        updated.as[NamedOuter]
      . assert(_ == NamedOuter(t"b", Inner(7)))

      test(m"Optical update on a sequence element"):
        val y = t"items: [10, 20, 30]".read[Yaml]
        val updated = y.lens(_.items(Prim) = 99)
        updated.items.as[List[Int]]
      . assert(_ == List(99, 20, 30))

      test(m"Lens reads a field by name"):
        summon["name" is Lens from Yaml onto Yaml](org).as[Text]
      . assert(_ == t"a")

      test(m"Lens.modify transforms a field through a function"):
        val lens = summon["n" is Lens from Yaml onto Yaml]
        val inner = Yaml.ast(Inner(7).yaml.root)
        lens.modify(inner)(yaml => (yaml.as[Int] + 1).yaml).as[Inner]
      . assert(_ == Inner(8))

      test(m"Each optic updates every sequence element"):
        val y = t"items: [10, 20, 30]".read[Yaml]
        y.lens(_.items(Each) = 0).items.as[List[Int]]
      . assert(_ == List(0, 0, 0))

      test(m"Filter optic updates only matching elements"):
        val y = t"items: [10, 20, 30]".read[Yaml]
        y.lens(_.items(Filter[Yaml](_.as[Int] > 15)) = 0).items.as[List[Int]]
      . assert(_ == List(10, 0, 0))

      test(m"Setting an absent field inserts it"):
        org.lens(_.extra = 7).selectDynamic("extra").as[Int]
      . assert(_ == 7)

    suite(m"Serializer roundtrip"):
      import formatting.blockYamlFormatting

      // Encode a value to `Yaml`, render it with the printer, parse the text
      // back, and decode: the full encode → print → parse → decode loop.
      def roundtrip[value: {Encodable in Yaml, Decodable in Yaml}](value: value): value =
        value.yaml.show.read[Yaml].as[value]

      // Print a parsed AST and re-parse it: print ∘ parse must be the identity
      // on the structural AST (compared by `deepEquals`).
      def astStable(source: Text): Boolean =
        val original = source.read[Yaml]
        Yaml.Ast.deepEquals(Yaml.unseal(original), Yaml.unseal(original.show.read[Yaml]))

      test(m"Roundtrip a positive integer"):
        roundtrip(42)
      . assert(_ == 42)

      test(m"Roundtrip a negative integer"):
        roundtrip(-99)
      . assert(_ == -99)

      test(m"Roundtrip a long"):
        roundtrip(1234567890123L)
      . assert(_ == 1234567890123L)

      test(m"Roundtrip a double"):
        roundtrip(3.1415926)
      . assert(_ == 3.1415926)

      test(m"Roundtrip positive infinity"):
        roundtrip(Double.PositiveInfinity)
      . assert(_ == Double.PositiveInfinity)

      test(m"Roundtrip negative infinity"):
        roundtrip(Double.NegativeInfinity)
      . assert(_ == Double.NegativeInfinity)

      test(m"Roundtrip NaN"):
        roundtrip(Double.NaN)
      . assert(_.isNaN)

      test(m"Roundtrip true"):
        roundtrip(true)
      . assert(identity)

      test(m"Roundtrip false"):
        roundtrip(false)
      . assert(!_)

      test(m"Roundtrip a simple string"):
        roundtrip(t"hello")
      . assert(_ == t"hello")

      test(m"Roundtrip a string with a space"):
        roundtrip(t"hello world")
      . assert(_ == t"hello world")

      test(m"Roundtrip a string that looks like an integer"):
        roundtrip(t"42")
      . assert(_ == t"42")

      test(m"Roundtrip a string that looks like a boolean"):
        roundtrip(t"true")
      . assert(_ == t"true")

      test(m"Roundtrip a string that looks like null"):
        roundtrip(t"null")
      . assert(_ == t"null")

      test(m"Roundtrip an empty string"):
        roundtrip(t"")
      . assert(_ == t"")

      test(m"Roundtrip a string with a colon"):
        roundtrip(t"key: value")
      . assert(_ == t"key: value")

      test(m"Roundtrip a string with a newline"):
        roundtrip(t"line1\nline2")
      . assert(_ == t"line1\nline2")

      test(m"Roundtrip a string with a quote and backslash"):
        roundtrip(t"a\"b\\c")
      . assert(_ == t"a\"b\\c")

      test(m"Roundtrip a string with leading indicator"):
        roundtrip(t"- not a list")
      . assert(_ == t"- not a list")

      test(m"Roundtrip a list of integers"):
        roundtrip(List(1, 2, 3))
      . assert(_ == List(1, 2, 3))

      test(m"Roundtrip a list of strings"):
        roundtrip(List(t"alice", t"bob"))
      . assert(_ == List(t"alice", t"bob"))

      test(m"Roundtrip an empty list"):
        roundtrip(List[Int]())
      . assert(_ == Nil)

      test(m"Roundtrip a map"):
        roundtrip(Map(t"a" -> 1, t"b" -> 2))
      . assert(_ == Map(t"a" -> 1, t"b" -> 2))

      test(m"Roundtrip a case class"):
        roundtrip(Person(t"Jon", 42))
      . assert(_ == Person(t"Jon", 42))

      test(m"Roundtrip a nested case class"):
        roundtrip(NamedOuter(t"a", Inner(7)))
      . assert(_ == NamedOuter(t"a", Inner(7)))

      test(m"Roundtrip a list of case classes"):
        roundtrip(List(Person(t"a", 1), Person(t"b", 2)))
      . assert(_ == List(Person(t"a", 1), Person(t"b", 2)))

      test(m"Roundtrip a list of lists"):
        roundtrip(List(List(1, 2), List(3, 4)))
      . assert(_ == List(List(1, 2), List(3, 4)))

      test(m"AST stable: nested mapping"):
        astStable(t"outer:\n  inner: 1\n  other: two")
      . assert(identity)

      test(m"AST stable: sequence of mappings"):
        astStable(t"- a: 1\n  b: 2\n- a: 3\n  b: 4")
      . assert(identity)

      test(m"AST stable: empty flow mapping"):
        astStable(t"{}")
      . assert(identity)

      test(m"AST stable: empty flow sequence"):
        astStable(t"[]")
      . assert(identity)

    suite(m"HTTP content-type integration"):
      import charEncoders.utf8Encoder
      import formatting.blockYamlFormatting

      test(m"serialises with an application/yaml media type"):
        Person(t"Jon", 42).yaml.generic(0)
      . assert(_.starts(t"application/yaml"))

      test(m"request body parses back via Instantiable"):
        val instantiable = summon[Yaml is Instantiable across HttpRequests from Text]
        instantiable(t"name: Jon\nage: 42").as[Person]
      . assert(_ == Person(t"Jon", 42))

    ConformanceTests.all()

    PositionTests()
    FocusTests()
    AccrualTests()
    DefaultTests()
