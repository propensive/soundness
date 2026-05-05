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
