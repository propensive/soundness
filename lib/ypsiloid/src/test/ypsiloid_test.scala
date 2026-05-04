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

import charEncoders.utf8
import strategies.throwUnsafely
import autopsies.contrastExpectations
import errorDiagnostics.stackTraces

case class Person(name: Text, age: Int) derives CanEqual
case class Inner(n: Int) derives CanEqual
case class Outer(inner: Inner) derives CanEqual
case class NamedOuter(name: Text, inner: Inner) derives CanEqual
case class OptFoo(x: Option[Int]) derives CanEqual
case class OptionalFoo(x: Optional[Int]) derives CanEqual

object Tests extends Suite(m"Ypsiloid Tests"):
  def run(): Unit =
    suite(m"Scalar parsing"):
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

      test(m"Parse empty document as null"):
        t"".read[Yaml].as[Optional[Int]]
      . assert(_ == Unset)

    suite(m"String parsing"):
      test(m"Parse a plain (unquoted) string"):
        t"hello".read[Yaml].as[Text]
      . assert(_ == t"hello")

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

      test(m"Double-quoted string with unicode escape"):
        t"\"\\u00e9\"".read[Yaml].as[Text]
      . assert(_ == t"é")

      test(m"Single-quoted string with embedded apostrophe"):
        t"'don''t'".read[Yaml].as[Text]
      . assert(_ == t"don't")

      test(m"Plain string containing a number-like prefix is text"):
        t"3things".read[Yaml].as[Text]
      . assert(_ == t"3things")

    suite(m"Block sequences"):
      test(m"Parse an empty block sequence (explicit)"):
        t"[]".read[Yaml].as[List[Int]]
      . assert(_ == Nil)

      test(m"Parse a block sequence of integers"):
        t"- 1\n- 2\n- 3".read[Yaml].as[List[Int]]
      . assert(_ == List(1, 2, 3))

      test(m"Parse a block sequence of strings"):
        t"- alice\n- bob".read[Yaml].as[List[Text]]
      . assert(_ == List(t"alice", t"bob"))

      test(m"Parse a block sequence of mixed primitives"):
        t"- 1\n- two\n- true".read[Yaml].root match
          case YamlAst.Sequence(items) => items.length
          case _                       => 0
      . assert(_ == 3)

      test(m"Parse a nested block sequence"):
        t"- - 1\n  - 2\n- - 3\n  - 4".read[Yaml].as[List[List[Int]]]
      . assert(_ == List(List(1, 2), List(3, 4)))

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

    suite(m"Block mappings"):
      test(m"Parse an empty block mapping"):
        t"{}".read[Yaml].as[Map[Text, Int]]
      . assert(_.isEmpty)

      test(m"Parse a single-key block mapping"):
        t"name: Alice".read[Yaml].as[Map[Text, Text]]
      . assert(_ == Map(t"name" -> t"Alice"))

      test(m"Parse a multi-key block mapping"):
        t"a: 1\nb: 2\nc: 3".read[Yaml].as[Map[Text, Int]]
      . assert(_ == Map(t"a" -> 1, t"b" -> 2, t"c" -> 3))

      test(m"Parse a nested block mapping"):
        t"inner:\n  n: 42".read[Yaml].as[Outer]
      . assert(_ == Outer(Inner(42)))

      test(m"Parse a block mapping with mixed value types"):
        t"name: Alice\nage: 30".read[Yaml].as[Person]
      . assert(_ == Person(t"Alice", 30))

      test(m"Parse a block mapping containing a sequence"):
        t"items:\n  - 1\n  - 2\n  - 3".read[Yaml].as[Map[Text, List[Int]]]
      . assert(_ == Map(t"items" -> List(1, 2, 3)))

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

      test(m"Parse a flow mapping into a case class"):
        t"{name: Alice, age: 30}".read[Yaml].as[Person]
      . assert(_ == Person(t"Alice", 30))

      test(m"Parse a nested flow mapping"):
        t"{inner: {n: 42}}".read[Yaml].as[Outer]
      . assert(_ == Outer(Inner(42)))

    suite(m"Block scalars"):
      test(m"Parse a literal block scalar preserves newlines"):
        t"text: |\n  line1\n  line2".read[Yaml].as[Map[Text, Text]]
      . assert(_ == Map(t"text" -> t"line1\nline2\n"))

      test(m"Parse a folded block scalar joins lines with spaces"):
        t"text: >\n  line1\n  line2".read[Yaml].as[Map[Text, Text]]
      . assert(_ == Map(t"text" -> t"line1 line2\n"))

      test(m"Literal scalar with strip indicator drops trailing newline"):
        t"text: |-\n  line1\n  line2".read[Yaml].as[Map[Text, Text]]
      . assert(_ == Map(t"text" -> t"line1\nline2"))

      test(m"Literal scalar with keep indicator preserves trailing blank lines"):
        t"text: |+\n  line1\n\n".read[Yaml].as[Map[Text, Text]]
      . assert(_ == Map(t"text" -> t"line1\n\n"))

      test(m"Folded scalar with explicit indentation"):
        t"text: >2\n   indented".read[Yaml].as[Map[Text, Text]]
      . assert(_ == Map(t"text" -> t" indented\n"))

    suite(m"Comments"):
      test(m"Comment after scalar is ignored"):
        t"42 # the answer".read[Yaml].as[Int]
      . assert(_ == 42)

      test(m"Standalone comment line is ignored"):
        t"# a comment\n42".read[Yaml].as[Int]
      . assert(_ == 42)

      test(m"Comments inside a block mapping are ignored"):
        t"a: 1\n# comment\nb: 2".read[Yaml].as[Map[Text, Int]]
      . assert(_ == Map(t"a" -> 1, t"b" -> 2))

      test(m"Comments inside a block sequence are ignored"):
        t"- 1\n# comment\n- 2".read[Yaml].as[List[Int]]
      . assert(_ == List(1, 2))

      test(m"Hash inside a quoted string is not a comment"):
        t"\"a # b\"".read[Yaml].as[Text]
      . assert(_ == t"a # b")

    suite(m"Anchors and aliases"):
      test(m"Alias resolves to anchored scalar"):
        t"a: &x 1\nb: *x".read[Yaml].as[Map[Text, Int]]
      . assert(_ == Map(t"a" -> 1, t"b" -> 1))

      test(m"Alias resolves to anchored mapping"):
        t"defaults: &d\n  n: 7\nuse: *d".read[Yaml].as[Map[Text, Inner]]
      . assert(_ == Map(t"defaults" -> Inner(7), t"use" -> Inner(7)))

      test(m"Alias resolves to anchored sequence"):
        t"a: &xs [1, 2, 3]\nb: *xs".read[Yaml].as[Map[Text, List[Int]]]
      . assert(_ == Map(t"a" -> List(1, 2, 3), t"b" -> List(1, 2, 3)))

      test(m"Unknown alias raises an error"):
        capture[YamlError](t"a: *missing".read[Yaml].as[Map[Text, Int]])
      . assert(_ => true)

    suite(m"Multi-document streams"):
      test(m"Parse a single document with explicit start marker"):
        t"---\n42".read[Yaml].as[Int]
      . assert(_ == 42)

      test(m"Parse a single document with start and end markers"):
        t"---\n42\n...".read[Yaml].as[Int]
      . assert(_ == 42)

      test(m"Stream of three documents"):
        t"---\n1\n---\n2\n---\n3".readAll[Yaml].map(_.as[Int]).to(List)
      . assert(_ == List(1, 2, 3))

      test(m"Empty stream yields no documents"):
        t"".readAll[Yaml].length
      . assert(_ == 0)

    suite(m"Generic derivation"):
      test(m"Decode a case class from block mapping"):
        t"name: Alice\nage: 30".read[Yaml].as[Person]
      . assert(_ == Person(t"Alice", 30))

      test(m"Decode a case class from flow mapping"):
        t"{name: Alice, age: 30}".read[Yaml].as[Person]
      . assert(_ == Person(t"Alice", 30))

      test(m"Decode a nested case class"):
        t"name: hello\ninner:\n  n: 7".read[Yaml].as[NamedOuter]
      . assert(_ == NamedOuter(t"hello", Inner(7)))

      test(m"Missing optional field decodes as None"):
        t"y: 1".read[Yaml].as[OptFoo].x
      . assert(_ == None)

      test(m"Present optional field decodes as Some"):
        t"x: 1".read[Yaml].as[OptFoo].x
      . assert(_ == Some(1))

      test(m"Missing Optional field decodes as Unset"):
        t"y: 1".read[Yaml].as[OptionalFoo].x
      . assert(_ == Unset)

      test(m"Present Optional field decodes as the value"):
        t"x: 1".read[Yaml].as[OptionalFoo].x
      . assert(_ == 1)

      test(m"Decode a list of case classes"):
        val yaml = t"- name: Alice\n  age: 30\n- name: Bob\n  age: 25"
        yaml.read[Yaml].as[List[Person]]
      . assert(_ == List(Person(t"Alice", 30), Person(t"Bob", 25)))

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

      test(m"Flow sequence parses to YamlAst.Sequence"):
        t"[1, 2, 3]".read[Yaml].root match
          case YamlAst.Sequence(items) => items.length
          case _                       => 0
      . assert(_ == 3)

      test(m"Flow mapping parses to YamlAst.Mapping"):
        t"{a: 1, b: 2}".read[Yaml].root match
          case YamlAst.Mapping(entries) => entries.length
          case _                        => 0
      . assert(_ == 2)

      test(m"Mapping with non-string key is allowed"):
        t"? [1, 2]\n: pair".read[Yaml].root match
          case YamlAst.Mapping(entries) => entries(0)(0) match
            case YamlAst.Sequence(items) => items.length
            case _                       => 0
          case _ => 0
      . assert(_ == 2)

    suite(m"Whitespace and indentation"):
      test(m"Leading and trailing newlines are ignored"):
        t"\n\n42\n\n".read[Yaml].as[Int]
      . assert(_ == 42)

      test(m"Trailing spaces on a scalar line are stripped"):
        t"hello   ".read[Yaml].as[Text]
      . assert(_ == t"hello")

      test(m"Inconsistent indentation raises an error"):
        capture[YamlError](t"a: 1\n b: 2".read[Yaml].as[Map[Text, Int]])
      . assert(_ => true)

    suite(m"Error handling"):
      test(m"Decode wrong type raises a YamlError"):
        capture[YamlError](t"hello".read[Yaml].as[Int])
      . assert(_ => true)

      test(m"Decode missing required field raises a YamlError"):
        capture[YamlError](t"{}".read[Yaml].as[Person])
      . assert(_ => true)

      test(m"Malformed flow sequence raises a parse error"):
        capture[YamlError](t"[1, 2,".read[Yaml].as[List[Int]])
      . assert(_ => true)

      test(m"Malformed flow mapping raises a parse error"):
        capture[YamlError](t"{a: 1,".read[Yaml].as[Map[Text, Int]])
      . assert(_ => true)

      test(m"Tab indentation in a block context raises an error"):
        capture[YamlError](t"a:\n\tb: 1".read[Yaml].as[Map[Text, Map[Text, Int]]])
      . assert(_ => true)
