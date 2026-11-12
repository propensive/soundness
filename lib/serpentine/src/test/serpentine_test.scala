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
package serpentine

import soundness.*
import denominative.dysasymptotics.linearSize // the suite counts small error lists

object Tests extends Suite("paths", m"internal Benchmarks"):
  def run(): Unit =
    suite(m"Constructions"):
      test(m"Create a two-element path"):
        % / "verona" / "capulet"

      . assert(_ == Path(t"/", t"capulet", t"verona"))

      test(m"Create a one-element path"):
        % / "verona"

      . assert(_ == Path(t"/", t"verona"))

      test(m"Ensure path has correct type"):
        val path: Path of ("capulet", "verona") = % / "verona" / "capulet"
        path
      . assert(_ == Path(t"/", t"capulet", t"verona"))

      test(m"Badly-typed path produces error"):
        demilitarize:
          val path: Path of ("capulet", "verona") = % / "verona" / "juliet"
        .map(_.message)

      . assert(!_.nil)

      test(m"Specificity of path is not obligatory"):
        val path: Path = % / "verona" / "juliet"
        path

      . assert(_ == Path(t"/", t"juliet", t"verona"))

      test(m"Construct a path on Linux"):
        val path: Path on Linux = (% / "verona" / "juliet").on[Linux]
        path

      . assert(_ == Path(t"/", t"juliet", t"verona"))

      test(m"A peer replaces the leaf"):
        val path: Path on Linux = (% / "verona" / "capulet").on[Linux]
        path.peer("juliet")

      . assert(_ == Path(t"/", t"juliet", t"verona"))

      test(m"A peer of a platformed path replaces the leaf"):
        unsafely:
          val path: Path on Linux = (% / "verona" / "capulet").on[Linux]
          val leaf: Text = t"juliet"
          path.peer(leaf)

      . assert(_ == Path(t"/", t"juliet", t"verona"))

      test(m"Construct a path with unknown label"):
        val dir: Text = t""
        val path = (% / dir / "juliet")
        path

      . assert(_ == Path(t"/", t"juliet", t""))

      test(m"Path with unknown label permitted without Tactic"):
        val dir: Text = t"dir"
        val path = (% / dir)

      . assert()

      test(m"Unknown label not permitted on Linux without Tactic"):
        demilitarize:
          val dir: Text = t"dir"
          val path = (% / dir / "juliet").on[Linux]

        . map(_.message)

      . assert(_.size == 1)

      test(m"Unknown label permitted on Linux with Tactic"):
        val dir: Text = t"dir"
        given Tactic[Name.Error] = strategies.throwUnsafely
        val path = (% / dir).on[Linux]

      . assert()

      test(m"Unknown label permitted on top of Linux Path"):
        val dir: Text = t"dir"
        given Tactic[Name.Error] = strategies.throwUnsafely
        val path = (% / dir).on[Linux] / "other"

      . assert()

      test(m"Construct a path with a label of a bad type is not permitted"):
        demilitarize:
          var dir: Char = 'x'
          recover:
            case Name.Error(_, _, _) => ()

          . protect:
              val path = (% / dir / "juliet")
        .map(_.message)

      . assert(!_.nil)

      test(m"Forbidden characters are forbidden"):
        demilitarize:
          val path: Path on Linux = (% / "ver/ona" / "juliet").on[Linux]
        . map(_.message)

      . assert(!_.nil)

      test(m"Autoconvert known `Path` to `Path on Linux`"):
        def receive(path: into[Path on Linux]): Unit = ()
        receive(% / "verona" / "capulet")

      . assert()

      test(m"Can't construct invalid path"):
        demilitarize:
          (Drive('D') / "Globe ")

      . assert(!_.nil)

      test(m"Can construct invalid path on platformless path"):
        val path = % / "./."

      . assert()

      test(m"Platformless path retains platformlessness"):
        demilitarize:
          val path = % / "verona"
          summon[path.type <:< Path on Linux]
      . assert(!_.nil)

      test(m"Platformed path retains platformedness"):
        demilitarize:
          val path = Drive('C') / "verona"
          path: Int
        . map(_.message)
      . assert(_.head.lay(false)(_.tt.contains(t"Windows")))

      test(m"Windows path can't be converted to Linux"):
        demilitarize:
          val path = (Drive('C') / "verona").on[Linux]
        . map(_.message)
      . assert(!_.nil)

      test(m"Linux path can't be converted to Windows"):
        demilitarize:
          val path = (% / "verona").on[Windows]
      . assert(!_.nil)

      test(m"Linux path can be converted to Mac OS"):
        demilitarize:
          val path = (% / "verona").on[Linux].on[MacOs]
        .map(_.message)
      . assert(_ == Nil)

    suite(m"Relative paths"):
      test(m"Create a relative path"):
        val relative: Relative of Mono["verona"] = ? / "verona"
        relative

      . assert(_ == Relative(0, List(t"verona")))

      test(m"Create a deeper relative path"):
        val relative: Relative of ("capulet", "verona") = ? / "verona" / "capulet"
        relative

      . assert(_ == Relative(0, List(t"capulet", t"verona")))

      test(m"Create a relative path with ascent"):
        val relative: Relative of Mono["verona"] under 1 = ? / ^ / "verona"
        relative

      . assert(_ == Relative(1, List(t"verona")))

      test(m"Create a relative path with double ascent"):
        val relative: Relative of Mono["verona"] under 2 = ? / ^ / ^ / "verona"
        relative

      . assert(_ == Relative(2, List(t"verona")))

    // A missing `Inspectable` is never a compile error — `derived` always succeeds and
    // substitutes a marked `toString`, `Showable` or `Encodable` rendering — so coverage can
    // only be held in place by asserting on the renderings themselves.
    suite(m"Native-rendering coverage"):
      test(m"serpentine's types inspect natively"):
        Inspectable.fallbacks
          ((% / "verona" / "capulet").inspect, (? / ^ / "verona").inspect, ?.inspect)
      . assert(_ == Nil)

      test(m"a path inspects as its path text"):
        (% / "verona" / "capulet").inspect
      . assert(_ == t"/verona/capulet")

      test(m"a relative path inspects with its ascent"):
        (? / ^ / ^ / "verona").inspect
      . assert(_ == t"../../verona")

      test(m"the self-relative path inspects as a dot"):
        ?.inspect
      . assert(_ == t".")

    suite(m"Encoding"):
      test(m"Serialize simple Linux path"):
        val path: Path on Linux = % / "verona"
        path.encode

      . assert(_ == t"/verona")

      test(m"Serialize simple Windows path"):
        val path: Path on Windows = (Drive('D') / "Globe")
        path.encode

      . assert(_ == t"D:\\Globe")

      test(m"Encode a relative path"):
        val relative: Relative on Linux = ? / ^ / "verona"
        relative.encode

      . assert(_ == t"../verona")

      test(m"Encode a relative path with double ascent"):
        val relative: Relative on Linux = ? / ^ / ^ / "verona"
        relative.encode

      . assert(_ == t"../../verona")

      test(m"Encode a peer"):
        val relative: Relative on Linux = ? / "verona"
        relative.encode

      . assert(_ == t"verona")

      test(m"Encode a relative path on Windows"):
        val relative: Relative on Windows = ? / ^ / "verona"
        relative.encode

      . assert(_ == t"..\\verona")

      test(m"Encode a relative path with double ascent on Windows"):
        val relative: Relative on Windows = ? / ^ / ^ / "verona"
        relative.encode

      . assert(_ == t"..\\..\\verona")

      test(m"Encode a peer on Windows"):
        val relative: Relative on Windows = ? / "verona"
        relative.encode

      . assert(_ == t"verona")

    suite(m"Decoding"):
      test(m"Decode a simple Linux path with a terminal slash"):
        given Tactic[Path.Error] = strategies.throwUnsafely
        t"/home/work/".as[Path on Linux]

      . assert(_ == % / "home" / "work")

      test(m"Decode a simple Linux path without a terminal slash"):
        given Tactic[Path.Error] = strategies.throwUnsafely
        t"/home/work".as[Path on Linux]

      . assert(_ == % / "home" / "work")

      test(m"Decode a simple Mac OS path without a terminal slash"):
        unsafely:
          t"/Users/Admin".as[Path on MacOs]

      . assert(_ == % / "Users" / "Admin")

      test(m"Decode a simple Mac OS path with a terminal slash"):
        unsafely:
          t"/Users/Admin/".as[Path on MacOs]

      . assert(_ == % / "Users" / "Admin")

      val windowsSystem: Path = Drive('C') / "Windows" / "System"

      test(m"Decode a simple Windows path without a terminal slash"):
        unsafely:
          t"C:\\Windows\\System".as[Path on Windows]

      . assert(_ == windowsSystem)

      test(m"Decode a simple Windows path with a terminal slash"):
        unsafely:
          t"C:\\Windows\\System\\".as[Path on Windows]

      . assert(_ == windowsSystem)

      test(m"Can't decode a path without knowing plane"):
        demilitarize:
          t"C:\\Windows\\System\\".as[Path]

      . assert(!_.nil)

      test(m"Decode a simple relative path"):
        t"verona".as[Relative on Linux]
      . assert(_ == ? / "verona")

      test(m"Decode a deeper relative path"):
        t"verona/capulet".as[Relative on Linux]
      . assert(_ == ? / "verona" / "capulet")

      test(m"Decode a relative path with ascent"):
        t"../verona/capulet".as[Relative on Linux]
      . assert(_ == ? / ^ / "verona" / "capulet")

      test(m"Decode a relative path self-reference"):
        t".".as[Relative on Linux]
      . assert(_ == ?)

      test(m"Decode a relative path with greater ascent"):
        t"../../verona/capulet".as[Relative on Linux]
      . assert(_ == ? / ^ / ^ / "verona" / "capulet")

      test(m"Decode a relative path with greater ascent on Windows"):
        t"..\\..\\verona\\capulet".as[Relative on Windows]
      . assert(_ == ? / ^ / ^ / "verona" / "capulet")

      test(m"Cannot decode a relative path without knowing plane"):
        demilitarize:
          t"..\\..\\verona\\capulet".as[Relative]
      . assert(!_.nil)

    suite(m"Compiletime tests"):
      test(m"Specific path has known elements"):
        val path = % / "verona" / "capulet"
        path.knownElements

      . assert(identity)

      test(m"Specific path on plane has known elements"):
        val path = % / "verona" / "capulet"
        path.on[Linux].knownElements

      . assert(identity)

      test(m"Auto-converted path has no known elements"):
        val path: Path on Linux = % / "verona" / "capulet"
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
        val result: Path of ("work", "home") = path1.conjunction(path2)
      . assert()

      test(m"Typelevel conjunction of unplatformed Linux paths"):
        val path1 = % / "home" / "work" / "data" / "hamlet" / "ophelia"
        val path2 = % / "home" / "more"
        val result: Path of Mono["home"] = path1.conjunction(path2)
      . assert()

      test(m"Precedence check"):
        val path1: Path on Linux = % / "verona" / "capulet" / "juliet"
        val path2: Path on Linux = % / "verona" / "capulet"
        path2.precedes(path1)
      . assert(_ == true)

      test(m"False precedence check"):
        val path1: Path on Linux = % / "verona" / "capulet" / "juliet"
        val path2: Path on Linux = % / "verona" / "capulet"
        path1.precedes(path2)
      . assert(_ == false)

      test(m"Precedence check for equal paths"):
        val path1: Path on Linux = % / "verona" / "capulet" / "juliet"
        val path2: Path on Linux = % / "verona" / "capulet" / "juliet"
        path1.precedes(path2)
      . assert(_ == true)

      test(m"Shift a path by 1"):
        val path: Path on Linux = % / "verona" / "capulet" / "juliet"
        path.shift(1)
      . assert(_ == % / "capulet" / "juliet")

      test(m"Shift a path by 2"):
        val path: Path on Linux = % / "verona" / "capulet" / "juliet"
        path.shift(2)
      . assert(_ == % / "juliet")

    suite(m"Relative tests"):
      test(m"Relative type retains plane"):
        demilitarize:
          val path1: Path on Linux = % / "home" / "work" / "data" / "hamlet"
          val path2: Path on Linux = % / "home" / "more"
          val relative = path1.toward(path2)
          relative: Optional[Relative on Linux]
        . map(_.message)

      . assert(_ == Nil)


      test(m"Relative types on unique-root System have certain `Relation`"):
        val path1: Path on Linux = % / "home" / "work" / "data" / "hamlet"
        val path2: Path on Linux = % / "home" / "more"
        path1.toward(path2)

      . assert()


      test(m"Relative types on non-unique system still retain Plane"):
          val path1: Path on Windows = Drive('C') / "home" / "work" / "data" / "hamlet"
          val path2: Path on Windows = Drive('C') / "home" / "more"
          val relative = path1.toward(path2)
          relative: Optional[Relative on Windows]


      test(m"Relative types on different Systems are platformless"):
        demilitarize:
          val path1: Path on Linux = % / "home" / "work" / "data" / "hamlet"
          val path2: Path on MacOs = % / "home" / "more"
          val relative = path1.toward(path2)
          relative: Path on MacOs
        . map(_.message)

      . assert(!_.nil)


      test(m"Calculate simple relative path"):
        val path1 = % / "home" / "work" / "data" / "hamlet"
        val path2 = % / "home" / "more"
        path1.toward(path2)

      . assert(_ == ? / ^ / ^ / ^ / "more")


      test(m"Calculate simple relative path in reverse"):
        val path1 = % / "home" / "work" / "data" / "hamlet"
        val path2 = % / "home" / "more"
        path2.toward(path1)

      . assert(_ == ? / ^ / "work" / "data" / "hamlet")


      test(m"Calculate simple relative path on plane"):
        val path1 = (% / "home" / "work" / "data" / "hamlet").on[Linux]
        val path2 = (% / "home" / "more").on[Linux]
        path1.toward(path2)

      . assert(_ == ? / ^ / ^ / ^ / "more")


      test(m"Calculate simple relative path on plane in reverse"):
        val path1 = (% / "home" / "work" / "data" / "hamlet").on[Linux]
        val path2 = (% / "home" / "more").on[Linux]
        path2.toward(path1)

      . assert(_ == ? / ^ / "work" / "data" / "hamlet")


      // test(m"Calculate simple relative path on plane statically"):
      //   val path1 = (% / "home" / "work" / "data" / "hamlet").on[Linux]
      //   val path2 = (% / "home" / "more").on[Linux]
      //   val path3: Relative of ("hamlet", "data", "work") under 1 = path1.relativeTo(path2)
      //   path3

      // . assert(_ == ? / ^ / "work" / "data" / "hamlet")


      // test(m"Calculate simple relative path in reverse, statically"):
      //   val path1 = (% / "home" / "work" / "data" / "hamlet").on[Linux]
      //   val path2 = (% / "home" / "more").on[Linux]
      //   val path3: Relative of Mono["more"] under 3 = path2.relativeTo(path1)
      //   path3

      // . assert(_ == ? / ^ / ^ / ^ / "more")


      test(m"Add relative parent"):
        val path1 = (% / "home" / "work" / "data" / "hamlet").on[Linux]
        path1 + (? / ^)

      . assert(_ == % / "home" / "work" / "data")


      test(m"Add relative grandparent"):
        val path1 = (% / "home" / "work" / "data" / "hamlet").on[Linux]
        path1 + (? / ^ / ^)

      . assert(_ == % / "home" / "work")


      test(m"Add relative cousin"):
        val path1 = (% / "home" / "work" / "data" / "hamlet").on[Linux]
        path1 + (? / ^ / ^ / "laertes" / "polonius")

      . assert(_ == % / "home" / "work" / "laertes" / "polonius")


      test(m"Add relative cousin, statically"):
        val path1 = (% / "home" / "work" / "data" / "hamlet").on[Linux]
        val p2: Path of ("polonius", "laertes", "work", "home") =
          path1 + (? / ^ / ^ / "laertes" / "polonius")
        p2

      . assert(_ == % / "home" / "work" / "laertes" / "polonius")

    suite(m"Pattern matching"):

      val shortPath: Path on Linux under %.type = % / "home"
      val path: Path on Linux under %.type = % / "home" / "work" / "data"

      given Tactic[Path.Error] = strategies.throwUnsafely
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
          case root /: t"home" /: more =>
            more match
              case t"work"            => false
              case t"work" /: t"data" => true
              case t"data"            => false
              case other              => false

          case _ =>
            false

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
      . assert(!_.nil)

      test(m"Windows path does not conform to Linux"):
        demilitarize:
          p"""D:\Globe\Stage""": Path on Linux
      . assert(!_.nil)

      test(m"Invalid path"):
        demilitarize(p"123").map(_.message)
      . assert(_ == List("[↯SN-066] The path 123 is not a valid Windows or POSIX path"))
