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
┃    Soundness, version 0.27.0.                                                                    ┃
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
package serpentine

import soundness.*

given String is Encodable in Text = Text(_)

object Tests extends Suite(t"Serpentine Benchmarks"):
  def run(): Unit =
    suite(t"Constructions"):
      test(t"Create a two-element path"):
        % / "foo" / "bar"

      . assert(_ == Path(t"/", t"bar", t"foo"))

      test(t"Create a one-element path"):
        % / "foo"

      . assert(_ == Path(t"/", t"foo"))

      test(t"Ensure path has correct type"):
        val path: Path of ("bar", "foo") = % / "foo" / "bar"
      . assert()

      test(t"Badly-typed path produces error"):
        demilitarize:
          val path: Path of ("bar", "foo") = % / "foo" / "baz"

      . assert(_.nonEmpty)

      test(t"Specificity of path is not obligatory"):
        val path: Path = % / "foo" / "baz"

      . assert()

      test(t"Construct a path on Linux"):
        val path: Path on Linux = (% / "foo" / "baz").on[Linux]

      . assert()

      test(t"Construct a path with unknown label"):
        val dir: Text = ""
        val path = (% / dir / "baz")

      . assert()

      test(t"Construct a path with unknown label not permitted on Linux without Tactic"):
        demilitarize:
          val dir: Text = "dir"
          val path = (% / dir / "baz").on[Linux]

      . assert(_.length > 0)

      test(t"Construct a path with unknown label is permitted on Linux with Tactic"):
        demilitarize:
          val dir: Text = "dir"
          mend:
            case NameError(_, _, _) => ()

          . within:
              val path = (% / dir / "baz").on[Linux]

      . assert(_ == Nil)

      test(t"Construct a path with a label of a bad type is not permitted"):
        demilitarize:
          var dir: Char = 'x'
          mend:
            case NameError(_, _, _) => ()

          . within:
              val path = (% / dir / "baz")

      . assert(_.nonEmpty)

      test(t"Forbidden characters are forbidden"):
        demilitarize:
          val path: Path on Linux = (% / "fo/o" / "baz").on[Linux]

      . assert(_.nonEmpty)

      test(t"Autoconvert known `Path` to `Path on Linux`"):
        def receive(path: into Path on Linux): Unit = ()
        receive(% / "foo" / "bar")

      .assert()

      test(t"Can't construct invalid path"):
        demilitarize:
          (Drive('D') / "Foo ")

      . assert(_.nonEmpty)

      test(t"Can construct invalid path on platformless path"):
        val path = % / "./."

      . assert()

      test(t"Platformless path retains platformlessness"):
        demilitarize:
          val path = % / "foo"
          summon[path.type <:< Path on Linux]
      . assert(_.nonEmpty)

      test(t"Platformed path retains platformedness"):
        demilitarize:
          val path = Drive('C') / "foo"
          path: Int
        . map(_.message)
      .assert(_.head.tt.contains(t"Windows"))

    suite(t"Serialization"):
      test(t"Serialize simple Linux path"):
        val path: Path on Linux = % / "foo"
        path.encode

      . assert(_ == t"/foo")

      test(t"Serialize simple Windows path"):
        val path: Path on Windows = (Drive('D') / "Foo")
        path.encode

      . assert(_ == t"D:\\Foo")
