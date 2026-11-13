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
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package stratiform

import soundness.*
import strategies.throwUnsafely
import codepages.utf8Codepage

// The derived TEL codecs exercised from a capture-checked module (#2197): every shape of
// field that reached a pure slot or a re-freshened array in the derivation — scalars, a nested
// product, repeated `List`/`Set` fields with and without defaults, `Optional`, `Map`, an enum
// and a recursive type. The values asserted are secondary; the suite's job is to compile.
object CheckedTests extends Suite("tel-checked", m"Stratiform capture-checked derivation tests"):
  case class Language(form: Text, flag: List[Text] = Nil)
  case class Script(language: Language, classpath: List[Text] = Nil)
  case class Plain(form: Text, count: Int)
  case class Sets(names: Set[Text])
  case class Table(entries: Map[Text, Int])
  case class Counted(name: Text, count: Optional[Int])

  enum Shape derives CanEqual:
    case Circle(radius: Double)
    case Square(side: Double)

  case class Shapes(shape: Shape, tags: List[Text] = Nil)
  case class Tree(name: Text, children: List[Tree] = Nil)

  def run(): Unit =
    suite(m"product decoders"):
      test(m"scalar fields"):
        t"form scala\ncount 3\n".read[Tel].as[Plain]

      . assert(_ == Plain(t"scala", 3))

      test(m"the issue's script header: nested product and repeated fields"):
        t"language scala\n  flag -deprecation\n  flag -feature\nclasspath a.jar\nclasspath b.jar\n"
        . read[Tel].as[Script]

      . assert(_ == Script(Language(t"scala", List(t"-deprecation", t"-feature")),
                           List(t"a.jar", t"b.jar")))

      test(m"a repeated field left to its default"):
        t"language scala\n".read[Tel].as[Script]

      . assert(_ == Script(Language(t"scala")))

      test(m"a Set field"):
        t"names alpha\nnames beta\nnames alpha\n".read[Tel].as[Sets]

      . assert(_ == Sets(Set(t"alpha", t"beta")))

      test(m"a Map field"):
        t"entries\n  entries\n    key one\n    value 1\n  entries\n    key two\n    value 2\n"
        . read[Tel].as[Table]

      . assert(_ == Table(Map(t"one" -> 1, t"two" -> 2)))

      test(m"a present Optional field"):
        t"name x\ncount 7\n".read[Tel].as[Counted]

      . assert(_ == Counted(t"x", 7))

      test(m"an absent Optional field"):
        t"name x\n".read[Tel].as[Counted]

      . assert(_ == Counted(t"x", Unset))

    suite(m"sum and recursive decoders"):
      test(m"an enum field"):
        t"shape\n  circle\n    radius 2.5\ntags round\n".read[Tel].as[Shapes]

      . assert(_ == Shapes(Shape.Circle(2.5), List(t"round")))

      test(m"a recursive type"):
        t"name root\nchildren\n  name a\nchildren\n  name b\n  children\n    name c\n"
        . read[Tel].as[Tree]

      . assert(_ == Tree(t"root", List(Tree(t"a"), Tree(t"b", List(Tree(t"c"))))))

    // RaTEL from a capture-checked module: an image, and a view which is a capability, are
    // opened, and rows are found, read and decoded within the block which lends the reader.
    suite(m"reading a RaTEL image in place"):
      val schema: Tels =
        val player = t"record Player\n  field name String key\n  field part String optional\n"
        t"name troupe\n\n$player\ndocument\n  field player Player repeatable\n".read[Tel].as[Tels]

      val troupe: Tel = t"player Bottom\n  part Pyramus\nplayer Snug\n".read[Tel]
      val image: Ratel = troupe.ratel(schema)

      case class Player(name: Text, part: Optional[Text])

      // A view as a file lends it: tracked, and confined to whoever was lent it.
      def parts(view: Terrain^): List[Optional[Text]] =
        view.open[Ratel]():
          (ratel.root / t"player").sorted.map(_.text(t"part"))

      test(m"an image is opened and a record found by its key"):
        image.open[Ratel]()((ratel.root / t"player").find(t"Bottom").let(_.text(t"part")))

      . assert(_ == t"Pyramus")

      test(m"a tracked view is opened"):
        parts(image.terrain)

      . assert(_ == List(t"Pyramus", Unset))

      test(m"a record is decoded from its row"):
        image.open[Ratel](Ratel.Flag.Verify):
          (ratel.root / t"player").find(t"Snug").let(_.as[Player])

      . assert(_ == Player(t"Snug", Unset))

      test(m"a row is held as a value, and read later in the block"):
        image.open[Ratel]():
          val row = (ratel.root / t"player").find(t"Snug")
          row.let(_.text(t"name"))

      . assert(_ == t"Snug")
