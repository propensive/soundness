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
      . assert(_.head.tt.contains(t"Windows"))

      test(t"Windows path can't be converted to Linux"):
        demilitarize:
          val path = (Drive('C') / "foo").on[Linux]
      . assert(_.nonEmpty)

      test(t"Linux path can't be converted to Windows"):
        demilitarize:
          val path = (% / "foo").on[Windows]
      . assert(_.nonEmpty)

      test(t"Linux path can be converted to Mac OS"):
        demilitarize:
          val path = (% / "foo").on[Linux].on[MacOs]
      . assert(_.isEmpty)

    suite(t"Relative paths"):
      test(t"Create a relative path"):
        val relative: Relation of Mono["foo"] = ? / "foo"
        relative

      . assert(_ == Relation(0, List(t"foo")))

      test(t"Create a deeper relative path"):
        val relative: Relation of ("bar", "foo") = ? / "foo" / "bar"
        relative

      . assert(_ == Relation(0, List(t"bar", t"foo")))

      test(t"Create a relative path with ascent"):
        val relative: Relation of Mono["foo"] under 1 = ? / ^ / "foo"
        relative

      . assert(_ == Relation(1, List(t"foo")))

      test(t"Create a relative path with double ascent"):
        val relative: Relation of Mono["foo"] under 2 = ? / ^ / ^ / "foo"
        relative

      . assert(_ == Relation(2, List(t"foo")))

    suite(t"Encoding"):
      test(t"Serialize simple Linux path"):
        val path: Path on Linux = % / "foo"
        path.encode

      . assert(_ == t"/foo")

      test(t"Serialize simple Windows path"):
        val path: Path on Windows = (Drive('D') / "Foo")
        path.encode

      . assert(_ == t"D:\\Foo")

      test(t"Encode a relative path"):
        val relative: Relation on Linux = ? / ^ / "foo"
        relative.encode

      . assert(_ == t"../foo")

      test(t"Encode a relative path with double ascent"):
        val relative: Relation on Linux = ? / ^ / ^ / "foo"
        relative.encode

      . assert(_ == t"../../foo")

      test(t"Encode a peer"):
        val relative: Relation on Linux = ? / "foo"
        relative.encode

      . assert(_ == t"foo")

      test(t"Encode a relative path on Windows"):
        val relative: Relation on Windows = ? / ^ / "foo"
        relative.encode

      . assert(_ == t"..\\foo")

      test(t"Encode a relative path with double ascent on Windows"):
        val relative: Relation on Windows = ? / ^ / ^ / "foo"
        relative.encode

      . assert(_ == t"..\\..\\foo")

      test(t"Encode a peer on Windows"):
        val relative: Relation on Windows = ? / "foo"
        relative.encode

      . assert(_ == t"foo")

    suite(t"Decoding"):
      test(t"Decode a simple Linux path with a terminal slash"):
        t"/home/work/".decode[Path on Linux]

      . assert(_ == % / "home" / "work")

      test(t"Decode a simple Linux path without a terminal slash"):
        t"/home/work".decode[Path on Linux]

      . assert(_ == % / "home" / "work")

      test(t"Decode a simple Mac OS path without a terminal slash"):
        unsafely:
          t"/Users/Admin".decode[Path on MacOs]

      . assert(_ == % / "Users" / "Admin")

      test(t"Decode a simple Mac OS path with a terminal slash"):
        unsafely:
          t"/Users/Admin/".decode[Path on MacOs]

      . assert(_ == % / "Users" / "Admin")

      val windowsSystem: Path = Drive('C') / "Windows" / "System"

      test(t"Decode a simple Windows path without a terminal slash"):
        unsafely:
          t"C:\\Windows\\System".decode[Path on Windows]

      . assert(_ == windowsSystem)

      test(t"Decode a simple Windows path with a terminal slash"):
        unsafely:
          t"C:\\Windows\\System\\".decode[Path on Windows]

      . assert(_ == windowsSystem)

      test(t"Can't decode a path without knowing platform"):
        demilitarize:
          t"C:\\Windows\\System\\".decode[Path]

      . assert(_.nonEmpty)
