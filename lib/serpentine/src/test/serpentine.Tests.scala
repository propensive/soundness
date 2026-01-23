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
┃    Soundness, version 0.53.0.                                                                    ┃
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

import autopsies.contrastExpectations

object Tests extends Suite(m"Serpentine Benchmarks"):
  def run(): Unit =
    suite(m"Constructions"):
      test(m"Create a two-element path"):
        % / "foo" / "bar"

      . assert(_ == Path(t"/", t"bar", t"foo"))

      test(m"Create a one-element path"):
        % / "foo"

      . assert(_ == Path(t"/", t"foo"))

      test(m"Ensure path has correct type"):
        val path: Path of ("bar", "foo") = % / "foo" / "bar"
        path
      . assert(_ == Path(t"/", t"bar", t"foo"))

      test(m"Badly-typed path produces error"):
        demilitarize:
          val path: Path of ("bar", "foo") = % / "foo" / "baz"
        .map(_.message)

      . assert(_.nonEmpty)

      test(m"Specificity of path is not obligatory"):
        val path: Path = % / "foo" / "baz"
        path

      . assert(_ == Path(t"/", t"baz", t"foo"))

      test(m"Construct a path on Linux"):
        val path: Path on Linux = (% / "foo" / "baz").on[Linux]
        path

      . assert(_ == Path(t"/", t"baz", t"foo"))

      test(m"Construct a path with unknown label"):
        val dir: Text = t""
        val path = (% / dir / "baz")
        path

      . assert(_ == Path(t"/", t"baz", t""))

      test(m"Construct a path with unknown text label is permitted without Tactic"):
        val dir: Text = t"dir"
        val path = (% / dir)

      . assert()

      test(m"Construct a path with unknown label not permitted on Linux without Tactic"):
        demilitarize:
          val dir: Text = t"dir"
          val path = (% / dir / "baz").on[Linux]

        . map(_.message)

      . assert(_.length == 1)

      test(m"Construct a path with unknown label is permitted on Linux with Tactic"):
        val dir: Text = t"dir"
        given Tactic[NameError] = strategies.throwUnsafely
        val path = (% / dir).on[Linux]

      . assert()

      test(m"Construct a path with unknown label is permitted on top of Linux Path"):
        val dir: Text = t"dir"
        given Tactic[NameError] = strategies.throwUnsafely
        val path = (% / dir).on[Linux] / "other"

      . assert()

      test(m"Construct a path with a label of a bad type is not permitted"):
        demilitarize:
          var dir: Char = 'x'
          recover:
            case NameError(_, _, _) => ()

          . within:
              val path = (% / dir / "baz")
        .map(_.message)

      . assert(_.nonEmpty)

      test(m"Forbidden characters are forbidden"):
        demilitarize:
          val path: Path on Linux = (% / "fo/o" / "baz").on[Linux]
        . map(_.message)

      . assert(_.nonEmpty)

      test(m"Autoconvert known `Path` to `Path on Linux`"):
        def receive(path: into[Path on Linux]): Unit = ()
        receive(% / "foo" / "bar")

      .assert()

      test(m"Can't construct invalid path"):
        demilitarize:
          (Drive('D') / "Foo ")

      . assert(_.nonEmpty)

      test(m"Can construct invalid path on platformless path"):
        val path = % / "./."

      . assert()

      test(m"Platformless path retains platformlessness"):
        demilitarize:
          val path = % / "foo"
          summon[path.type <:< Path on Linux]
      . assert(_.nonEmpty)

      test(m"Platformed path retains platformedness"):
        demilitarize:
          val path = Drive('C') / "foo"
          path: Int
        . map(_.message)
      . assert(_.head.tt.contains(t"Windows"))

      test(m"Windows path can't be converted to Linux"):
        demilitarize:
          val path = (Drive('C') / "foo").on[Linux]
        . map(_.message)
      . assert(_.nonEmpty)

      test(m"Linux path can't be converted to Windows"):
        demilitarize:
          val path = (% / "foo").on[Windows]
      . assert(_.nonEmpty)

      test(m"Linux path can be converted to Mac OS"):
        demilitarize:
          val path = (% / "foo").on[Linux].on[MacOs]
        .map(_.message)
      . assert(_ == Nil)

    suite(m"Relative paths"):
      test(m"Create a relative path"):
        val relative: Relative of Mono["foo"] = ? / "foo"
        relative

      . assert(_ == Relative(0, List(t"foo")))

      test(m"Create a deeper relative path"):
        val relative: Relative of ("bar", "foo") = ? / "foo" / "bar"
        relative

      . assert(_ == Relative(0, List(t"bar", t"foo")))

      test(m"Create a relative path with ascent"):
        val relative: Relative of Mono["foo"] under 1 = ? / ^ / "foo"
        relative

      . assert(_ == Relative(1, List(t"foo")))

      test(m"Create a relative path with double ascent"):
        val relative: Relative of Mono["foo"] under 2 = ? / ^ / ^ / "foo"
        relative

      . assert(_ == Relative(2, List(t"foo")))

    suite(m"Encoding"):
      test(m"Serialize simple Linux path"):
        val path: Path on Linux = % / "foo"
        path.encode

      . assert(_ == t"/foo")

      test(m"Serialize simple Windows path"):
        val path: Path on Windows = (Drive('D') / "Foo")
        path.encode

      . assert(_ == t"D:\\Foo")

      test(m"Encode a relative path"):
        val relative: Relative on Linux = ? / ^ / "foo"
        relative.encode

      . assert(_ == t"../foo")

      test(m"Encode a relative path with double ascent"):
        val relative: Relative on Linux = ? / ^ / ^ / "foo"
        relative.encode

      . assert(_ == t"../../foo")

      test(m"Encode a peer"):
        val relative: Relative on Linux = ? / "foo"
        relative.encode

      . assert(_ == t"foo")

      test(m"Encode a relative path on Windows"):
        val relative: Relative on Windows = ? / ^ / "foo"
        relative.encode

      . assert(_ == t"..\\foo")

      test(m"Encode a relative path with double ascent on Windows"):
        val relative: Relative on Windows = ? / ^ / ^ / "foo"
        relative.encode

      . assert(_ == t"..\\..\\foo")

      test(m"Encode a peer on Windows"):
        val relative: Relative on Windows = ? / "foo"
        relative.encode

      . assert(_ == t"foo")

    suite(m"Decoding"):
      test(m"Decode a simple Linux path with a terminal slash"):
        given Tactic[PathError] = strategies.throwUnsafely
        t"/home/work/".decode[Path on Linux]

      . assert(_ == % / "home" / "work")

      test(m"Decode a simple Linux path without a terminal slash"):
        given Tactic[PathError] = strategies.throwUnsafely
        t"/home/work".decode[Path on Linux]

      . assert(_ == % / "home" / "work")

      test(m"Decode a simple Mac OS path without a terminal slash"):
        unsafely:
          t"/Users/Admin".decode[Path on MacOs]

      . assert(_ == % / "Users" / "Admin")

      test(m"Decode a simple Mac OS path with a terminal slash"):
        unsafely:
          t"/Users/Admin/".decode[Path on MacOs]

      . assert(_ == % / "Users" / "Admin")

      val windowsSystem: Path = Drive('C') / "Windows" / "System"

      test(m"Decode a simple Windows path without a terminal slash"):
        unsafely:
          t"C:\\Windows\\System".decode[Path on Windows]

      . assert(_ == windowsSystem)

      test(m"Decode a simple Windows path with a terminal slash"):
        unsafely:
          t"C:\\Windows\\System\\".decode[Path on Windows]

      . assert(_ == windowsSystem)

      test(m"Can't decode a path without knowing plane"):
        demilitarize:
          t"C:\\Windows\\System\\".decode[Path]

      . assert(_.nonEmpty)

      test(m"Decode a simple relative path"):
        t"foo".decode[Relative on Linux]
      . assert(_ == ? / "foo")

      test(m"Decode a deeper relative path"):
        t"foo/bar".decode[Relative on Linux]
      . assert(_ == ? / "foo" / "bar")

      test(m"Decode a relative path with ascent"):
        t"../foo/bar".decode[Relative on Linux]
      . assert(_ == ? / ^ / "foo" / "bar")

      test(m"Decode a relative path self-reference"):
        t".".decode[Relative on Linux]
      . assert(_ == ?)

      test(m"Decode a relative path with greater ascent"):
        t"../../foo/bar".decode[Relative on Linux]
      . assert(_ == ? / ^ / ^ / "foo" / "bar")

      test(m"Decode a relative path with greater ascent on Windows"):
        t"..\\..\\foo\\bar".decode[Relative on Windows]
      . assert(_ == ? / ^ / ^ / "foo" / "bar")

      test(m"Cannot decode a relative path without knowing plane"):
        demilitarize:
          t"..\\..\\foo\\bar".decode[Relative]
      . assert(_.nonEmpty)

    suite(m"Compiletime tests"):
      test(m"Specific path has known elements"):
        val path = % / "foo" / "bar"
        path.knownElements

      . assert(identity)

      test(m"Specific path on plane has known elements"):
        val path = % / "foo" / "bar"
        path.on[Linux].knownElements

      . assert(identity)

      test(m"Specific path auto-converted to plane does not have known elements"):
        val path: Path on Linux = % / "foo" / "bar"
        path.knownElements

      . assert(!_)

      test(m"Path with variable does not have known elements"):
        var user: Text = t"user"
        val path = % / "home" / user
        path.knownElements

      . assert(!_)

      test(m"Path with variable still has known element types"):
        def user: Text = t"user"
        val path = % / user
        path.knownElementTypes

      . assert(identity)

    suite(m"Conjunction tests"):
      test(m"Conjunction of two Linux paths"):
        val path1: Path on Linux = % / "home" / "work" / "data"
        val path2: Path on Linux = % / "home" / "data" / "work"
        path1.conjunction(path2)
      . assert(_ == % / "home")

      test(m"Conjunction of two different Linux paths"):
        val path1: Path on Linux = % / "home" / "work" / "data"
        val path2: Path on Linux = % / "home" / "work" / "more"
        path1.conjunction(path2)
      . assert(_ == % / "home" / "work")

      test(m"Typelevel conjunction"):
        val path1: Path of ("data", "work", "home") on Linux = % / "home" / "work" / "data"
        val path2: Path of ("more", "work", "home") on Linux = % / "home" / "work" / "more"
        val result: Path of ("work" , "home") = path1.conjunction(path2)
      . assert()

      test(m"Different typelevel conjunction of two unplatformed Linux paths"):
        val path1 = % / "home" / "work" / "data" / "foo" / "bar"
        val path2 = % / "home" / "more"
        val result: Path of Mono["home"] = path1.conjunction(path2)
      . assert()

      test(m"Precedence check"):
        val path1: Path on Linux = % / "foo" / "bar" / "baz"
        val path2: Path on Linux = % / "foo" / "bar"
        path2.precedes(path1)
      . assert(_ == true)

      test(m"False precedence check"):
        val path1: Path on Linux = % / "foo" / "bar" / "baz"
        val path2: Path on Linux = % / "foo" / "bar"
        path1.precedes(path2)
      . assert(_ == false)

      test(m"Precedence check for equal paths"):
        val path1: Path on Linux = % / "foo" / "bar" / "baz"
        val path2: Path on Linux = % / "foo" / "bar" / "baz"
        path1.precedes(path2)
      . assert(_ == true)

      test(m"Shift a path by 1"):
        val path: Path on Linux = % / "foo" / "bar" / "baz"
        path.shift(1)
      . assert(_ == % / "bar" / "baz")

      test(m"Shift a path by 2"):
        val path: Path on Linux = % / "foo" / "bar" / "baz"
        path.shift(2)
      . assert(_ == % / "baz")

    suite(m"Relative tests"):
      test(m"Relative type retains plane"):
        demilitarize:
          val path1: Path on Linux = % / "home" / "work" / "data" / "foo"
          val path2: Path on Linux = % / "home" / "more"
          val relative = path2.relativeTo(path1)
          relative: Optional[Relative on Linux]
        . map(_.message)

      . assert(_ == Nil)


      test(m"Relative types on unique-root System have certain `Relation`"):
        val path1: Path on Linux = % / "home" / "work" / "data" / "foo"
        val path2: Path on Linux = % / "home" / "more"
        path2.relativeTo(path1)

      . assert()


      test(m"Relative types on non-unique system still retain Plane"):
          val path1: Path on Windows = Drive('C') / "home" / "work" / "data" / "foo"
          val path2: Path on Windows = Drive('C') / "home" / "more"
          val relative = path2.relativeTo(path1)
          relative: Optional[Relative on Windows]


      test(m"Relative types on different Systems are platformless"):
        demilitarize:
          val path1: Path on Linux = % / "home" / "work" / "data" / "foo"
          val path2: Path on MacOs = % / "home" / "more"
          val relative = path2.relativeTo(path1)
          relative: Path on MacOs
        . map(_.message)

      . assert(_.nonEmpty)


      test(m"Calculate simple relative path"):
        val path1 = % / "home" / "work" / "data" / "foo"
        val path2 = % / "home" / "more"
        path2.relativeTo(path1)

      . assert(_ == ? / ^ / ^ / ^ / "more")


      test(m"Calculate simple relative path in reverse"):
        val path1 = % / "home" / "work" / "data" / "foo"
        val path2 = % / "home" / "more"
        path1.relativeTo(path2)

      . assert(_ == ? / ^ / "work" / "data" / "foo")


      test(m"Calculate simple relative path on plane"):
        val path1 = (% / "home" / "work" / "data" / "foo").on[Linux]
        val path2 = (% / "home" / "more").on[Linux]
        path2.relativeTo(path1)

      . assert(_ == ? / ^ / ^ / ^ / "more")


      test(m"Calculate simple relative path on plane in reverse"):
        val path1 = (% / "home" / "work" / "data" / "foo").on[Linux]
        val path2 = (% / "home" / "more").on[Linux]
        path1.relativeTo(path2)

      . assert(_ == ? / ^ / "work" / "data" / "foo")


      test(m"Calculate simple relative path on plane statically"):
        val path1 = (% / "home" / "work" / "data" / "foo").on[Linux]
        val path2 = (% / "home" / "more").on[Linux]
        val path3: Relative of ("foo", "data", "work") under 1 = path1.relativeTo(path2)
        path3

      . assert(_ == ? / ^ / "work" / "data" / "foo")


      test(m"Calculate simple relative path in reverse, statically"):
        val path1 = (% / "home" / "work" / "data" / "foo").on[Linux]
        val path2 = (% / "home" / "more").on[Linux]
        val path3: Relative of Mono["more"] under 3 = path2.relativeTo(path1)
        path3

      . assert(_ == ? / ^ / ^ / ^ / "more")


      test(m"Add relative parent"):
        val path1 = (% / "home" / "work" / "data" / "foo").on[Linux]
        path1 + (? / ^)

      . assert(_ == % / "home" / "work" / "data")


      test(m"Add relative grandparent"):
        val path1 = (% / "home" / "work" / "data" / "foo").on[Linux]
        path1 + (? / ^ / ^)

      . assert(_ == % / "home" / "work")


      test(m"Add relative cousin"):
        val path1 = (% / "home" / "work" / "data" / "foo").on[Linux]
        path1 + (? / ^ / ^ / "baz" / "quux")

      . assert(_ == % / "home" / "work" / "baz" / "quux")


      test(m"Add relative cousin, statically"):
        val path1 = (% / "home" / "work" / "data" / "foo").on[Linux]
        val p2: Path of ("quux", "baz", "work", "home") = path1 + (? / ^ / ^ / "baz" / "quux")
        p2

      . assert(_ == % / "home" / "work" / "baz" / "quux")

    suite(m"Pattern matching"):

      val shortPath: Path on Linux under %.type = % / "home"
      val path: Path on Linux under %.type = % / "home" / "work" / "data"

      given Tactic[PathError] = strategies.throwUnsafely
      summon[%.type is Radical on Linux]

      test(m"Match root on a simple path"):
        path.only:
          case root /: right => root

      . assert(_ == %)

      test(m"Match root on a short path"):
        shortPath.only:
          case root /: right => root

      . assert(_ == %)

      test(m"Match descent on a simple path"):
        path.only:
          case root /: right => right

      . assert(_ == ? / "home" / "work" / "data")

      test(m"Match descent on a short path"):
        shortPath match
          case root /: t"home" => true
          case _               => false

      . assert(identity(_))

      test(m"Match top elementon a simple path"):
        path.only:
          case root /: right0 /: right1 => right0

      . assert(_ == t"home")


      test(m"Match further descent on a simple path"):
        path.only:
          case root /: right0 /: right1 => right1

      . assert(_ == ? / "work" / "data")


      test(m"Match last descent on a simple path"):
        path.only:
          case root /: right0 /: right1 /: right2 => right2

      . assert(_ == t"data")


      test(m"Further elements don't match"):
        path.only:
          case root /: right0 /: right1 /: right2 /: right3 => right3

      . assert(_ == Unset)

      test(m"Multi-level matching"):
        path match
          case root /: t"home" /: more => more match
            case t"work"            => false
            case t"work" /: t"data" => true
            case t"data"            => false
            case other              => false
          case _ => false

      . assert(identity(_))

    suite(m"Interpolation tests"):
      test(m"Windows path"):
        p"""C:\Windows\System32\file.txt""": Path on Windows
      . assert(_ == Drive('C') / "Windows" / "System32" / "file.txt")

      test(m"Linux path"):
        p"/home/user/data.csv": Path on Posix
      . assert(_ == % / "home" / "user" / "data.csv")

      test(m"Linux path does not conform to Windows"):
        demilitarize:
          p"/home/user/data.csv": Path on Windows
      . assert(_.length > 0)

      test(m"Windows path does not conform to Linux"):
        demilitarize:
          p"""D:\Foo\Bar""": Path on Linux
      . assert(_.length > 0)

      test(m"Invalid path"):
        demilitarize(p"123").map(_.message)
      . assert(_ == List("serpentine: The path 123 is not a valid Windows or POSIX path"))
