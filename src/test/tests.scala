/*
    Serpentine, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package serpentine

import probably.*
import rudiments.*
import digression.*
import anticipation.*
import kaleidoscope.*
import perforate.*
import gossamer.*
import spectacular.*
import larceny.*
import symbolism.*

object Example:
  type Forbidden = ".*abc" | ".*\\\\.*" | ".*/.*" | "lpt1.*" | ".* "
  
  object TestPath:
    given (using Raises[PathError]): Decoder[TestPath] = Reachable.decode[TestPath](_)
    given pathCreator: PathCreator[TestPath, Forbidden, Drive] = TestPath(_, _)
    given Show[TestPath] = _.render
    def parse(text: Text)(using Raises[PathError]): TestPath = text.decodeAs[TestPath]

    given rootParser: RootParser[TestPath, Drive] with
      def parse(text: Text): Optional[(Drive, Text)] = text.only:
        case r"$letter([a-zA-Z]):\\.*" => (Drive(unsafely(letter(0)).toUpper), text.drop(3))

    given reachable: Reachable[TestPath, Forbidden, Drive] with
      def separator(path: TestPath): Text = t"\\"
      def root(path: TestPath): Drive = path.root
      def prefix(drive: Drive): Text = t"${drive.letter}:\\"
      def descent(path: TestPath): List[PathName[Forbidden]] = path.descent
  
  case class TestPath(root: Drive, descent: List[PathName[Forbidden]])

  object TestLink:
    inline given (using Raises[PathError]): Decoder[TestLink] = Followable.decoder[TestLink]
    given linkCreator: PathCreator[TestLink, Forbidden, Int] = TestLink(_, _)
    inline given add: Operator["+", TestLink, TestLink] = Followable.add[TestLink, Forbidden]

    given Show[TestLink] = _.render
    def parse(text: Text)(using Raises[PathError]): TestLink = text.decodeAs[TestLink]

    given pathlike: Followable[TestLink, Forbidden, "..", "."] with
      def separator(path: TestLink): Text = t"\\"
      val separators: Set[Char] = Set('/')
      def ascent(path: TestLink): Int = path.ascent
      def descent(path: TestLink): List[PathName[Forbidden]] = path.descent

  case class TestLink(ascent: Int, descent: List[PathName[Forbidden]])

  object Drive:
    given Default[Drive](Drive('C'))

  case class Drive(letter: Char):
    @targetName("child")
    def /(name: PathName[Forbidden]): TestPath = TestPath(this, List(name))

  given hierarchy: Hierarchy[TestPath, TestLink] = new Hierarchy[TestPath, TestLink] {}

import Example.*

object Tests extends Suite(t"Serpentine Tests"):
  given CanThrow[PathError] = unsafeExceptions.canThrowAny
  given Default[Root.type](Root)
  import errorHandlers.throwUnsafely

  def run(): Unit =
    suite(t"Absolute parsing"):
      test(t"parse simple absolute path"):
        unsafely(SimplePath.parse(t"/home"))
      .assert(_ == SimplePath(List(PathName(t"home"))))
      
      test(t"parse deeper absolute path"):
        unsafely(SimplePath.parse(t"/home/work"))
      .assert(_ == SimplePath(List(PathName(t"work"), PathName(t"home"))))
      
      test(t"parse even deeper absolute path"):
        unsafely(SimplePath.parse(t"/home/work/data"))
      .assert(_ == SimplePath(List(PathName(t"data"), PathName(t"work"), PathName(t"home"))))
      
      test(t"parse even absolute directory-style path"):
        unsafely(SimplePath.parse(t"/home/work/"))
      .assert(_ == SimplePath(List(PathName(t"work"), PathName(t"home"))))
      
      test(t"try to parse path without prefix"):
        unsafely(capture[PathError](SimplePath.parse(t"home/work/")))
      .assert(_ == PathError(PathError.Reason.NotRooted(t"home/work/")))

      test(t"Show a simple path"):
        val path: PathName[".*/.*"] = p"abc"
        path.show
      .assert(_ == t"abc")
      
      
    suite(t"Parsing absolute paths with root"):
      test(t"parse simple rooted absolute path"):
        unsafely(TestPath.parse(t"C:\\Windows"))
      .assert(_ == TestPath(Drive('C'), List(PathName(t"Windows"))))
      
      test(t"parse deeper rooted absolute path"):
        unsafely(TestPath.parse(t"D:\\Windows\\System"))
      .assert(_ == TestPath(Drive('D'), List(PathName(t"System"), PathName(t"Windows"))))
      
      test(t"parse even deeper rooted absolute path"):
        unsafely(TestPath.parse(t"e:\\Windows\\System\\Data"))
      .assert(_ == TestPath(Drive('E'), List(PathName(t"Data"), PathName(t"System"), PathName(t"Windows"))))
      
      test(t"parse even absolute rooted directory-style path"):
        unsafely(TestPath.parse(t"f:\\Windows\\System\\"))
      .assert(_ == TestPath(Drive('F'), List(PathName(t"System"), PathName(t"Windows"))))
      
    suite(t"Relative parsing"):
      test(t"parse simple relative path"):
        unsafely(SimpleLink.parse(t"peer"))
      .assert(_ == SimpleLink(0, List(PathName(t"peer"))))

      test(t"parse three-part relative subpath"):
        unsafely(SimpleLink.parse(t"path/to/child"))
      .assert(_ == SimpleLink(0, List(t"child", t"to", t"path").map(PathName(_))))

      test(t"parse parent relative path"):
        unsafely(SimpleLink.parse(t".."))
      .assert(_ == SimpleLink(1, List()))

      test(t"parse ancestor relative path"):
        unsafely(SimpleLink.parse(t"../../.."))
      .assert(_ == SimpleLink(3, List()))
    
      test(t"parse relative link to current path"):
        unsafely(SimpleLink.parse(t"."))
      .assert(_ == SimpleLink(0, List()))
      
      test(t"parse relative link to uncle path"):
        unsafely(SimpleLink.parse(t"../path"))
      .assert(_ == SimpleLink(1, List(PathName(t"path"))))
      
      test(t"parse relative link to cousin path"):
        unsafely(SimpleLink.parse(t"../path/child"))
      .assert(_ == SimpleLink(1, List(t"child", t"path").map(PathName(_))))

    suite(t"Show paths"):
      import hierarchies.simple
      
      test(t"show simple relative path"):
        (? / p"hello").show
      .assert(_ == t"hello")
      
      test(t"show two-level relative path"):
        (? / p"hello" / p"world").show
      .assert(_ == t"hello/world")
      
      test(t"show self"):
        ?.show
      .assert(_ == t".")
      
      test(t"show self's parent"):
        ?.parent.show
      .assert(_ == t"..")
      
      test(t"show self's grandparent"):
        ?.parent.parent.show
      .assert(_ == t"../..")
      
      test(t"show sister path"):
        (?.parent / p"foo").show
      .assert(_ == t"../foo")
      
      test(t"show uncle path"):
        (?.parent.parent / p"foo").show
      .assert(_ == t"../../foo")
      
      test(t"show cousin path"):
        (?.parent.parent / p"foo" / p"bar").show
      .assert(_ == t"../../foo/bar")

      test(t"show a simple generic path"):
        (% / p"foo").show
      .assert(_ == t"/foo")

      test(t"show a deeper generic path"):
        (% / p"foo" / p"bar").show
      .assert(_ == t"/foo/bar")
      
      test(t"show the root path"):
        summon[Show[%.type]](%)
      .assert(_ == t"/")
    
    suite(t"Path tests"):
      import hierarchies.simple

      test(t"Check keeping part of a path"):
        (% / p"home" / p"user" / p"work" / p"data").keep(2)
      .assert(_ == % / p"home" / p"user")
      
      test(t"Access the root of a path"):
        (% / p"home" / p"user" / p"work").root
      .assert(_ == Root)
      
      test(t"Access the ascent of a link"):
        (?^^ / p"home" / p"user" / p"work").ascent
      .assert(_ == 2)

      test(t"simple path from forbidden string does not compile"):
        demilitarize:
          val elem: PathName["bad"] = p"bad"
        .map(_.message)
      .assert(_ == List(t"serpentine: a path element may not be bad"))
      
      test(t"simple path from forbidden set of strings does not compile"):
        demilitarize:
          val elem: PathName["bad" | "awful"] = p"bad"
        .map(_.message)
      .assert(_ == List(t"serpentine: a path element may not be bad"))
      
      test(t"simple path not in forbidden set of strings does compile"):
        val elem: PathName["bad" | "awful"] = p"safe"
      .assert()
      
      test(t"path with forbidden character does not compile"):
        demilitarize:
          val elem: PathName["bad" | ".*n.*"] = p"unsafe"
        .map(_.message)
      .assert(_ == List(t"serpentine: a path element may not contain the character n"))
      
      test(t"path with forbidden character does not compile"):
        demilitarize:
          val elem: PathName[".*a.*" | ".*e.*" | ".*i.*" | ".*o.*" | ".*u.*"] = p"unsafe"
        .map(_.message)
      .assert(_ == List(t"serpentine: a path element may not contain the character a"))

      test(t"Parse a path name with an invalid character"):
        unsafely(capture[PathError](PathName[".*x.*"](t"excluded")))
      .assert(_ == PathError(PathError.Reason.InvalidChar('x')))
      
      test(t"Parse a path name with an invalid suffix"):
        unsafely(capture[PathError](PathName[".*txt"](t"bad.txt")))
      .assert(_ == PathError(PathError.Reason.InvalidSuffix(t"txt")))

      test(t"Parse a path name with an invalid prefix"):
        unsafely(capture[PathError](PathName["bad.*"](t"bad.txt")))
      .assert(_ == PathError(PathError.Reason.InvalidPrefix(t"bad")))
      
      test(t"Parse a path name with an invalid name"):
        unsafely(capture[PathError](PathName["bad\\.txt"](t"bad.txt")))
      .assert(_ == PathError(PathError.Reason.InvalidName(t"bad\\.txt")))

    suite(t"Pattern matching"):
      import hierarchies.simple
      val root = %
      val path = % / p"home"
      val path2 = path / p"work"
      
      test(t"Pattern match on short path"):
        path match
          case _ / p"home" => true
          case _           => false
      .assert(identity(_))
      
      test(t"Error on invalid path match"):
        demilitarize:
          path match
            case _ / p"ho/me" => true
            case _            => false
        .map(_.message)
      .assert(_ == List("serpentine: a path element may not contain the character /"))
      
      test(t"Pattern match on longer path"):
        path2 match
          case _ / p"home" / p"work" => true
          case _                     => false
      .assert(identity(_))
      
      test(t"Does not match incorrect path"):
        path2 match
          case _ / p"work" / p"home" => false
          case _                     => true
      .assert(identity(_))
      
      test(t"Pattern match on longer path with root"):
        path2 match
          case Root / p"home" / p"work" => true
          case _                        => false
      .assert(identity(_))
      
      test(t"Extract path element"):
        path2 match
          case Root / top / secondary => top.render
          case _                      => t""
      .assert(_ == t"home")
      
      test(t"Extract final path element"):
        path2 match
          case Root / top / secondary => secondary.render
          case _                      => t""
      .assert(_ == t"work")

    suite(t"Relative path tests"):
      import hierarchies.simple
      
      test(t"Relative path has correct parent"):
        (? / p"foo" / p"bar").parent
      .assert(_ == (? / p"foo"))

      test(t"Relativepath has correct parent 2"):
        (? / p"foo").parent
      .assert(_ == ?)

      test(t"Parent of Relative path root has correct parent"):
        ?^
      .assert(_ == SimpleLink(1, Nil))
      
      test(t"Parent of Relative path root has correct parent"):
        ?^^
      .assert(_ == SimpleLink(2, Nil))
      
      test(t"Parent of cousin keeps correct ascent"):
        (?^^ / p"foo" / p"bar").parent
      .assert(_ == ?^^ / p"foo")
      
      test(t"Parent of cousin keeps correct ascent 2"):
        (?^^ / p"foo").parent
      .assert(_ == ?^^)
      
      test(t"Triple parent has correct ascent"):
        ?^^^.ascent
      .assert(_ == 3)
      
      test(t"Triple parent of child has correct ascent"):
        (?^^^ / p"child").ascent
      .assert(_ == 3)

    suite(t"Relative path tests"):
      import hierarchies.simple
      
      test(t"Find conjunction of distinct paths"):
        val p1 = % / p"foo" / p"bar"
        val p2 = % / p"bar" / p"baz"
        p1.conjunction(p2)
      .assert(_ == %)
      
      test(t"Find conjunction of paths with one common dir"):
        val p1 = % / p"foo" / p"bar"
        val p2 = % / p"foo" / p"baz"
        p1.conjunction(p2)
      .assert(_ == % / p"foo")
      
      test(t"Find conjunction of paths with two common dirs"):
        val p1 = % / p"foo" / p"bar" / p"baz"
        val p2 = % / p"foo" / p"bar" / p"quux"
        p1.conjunction(p2)
      .assert(_ == % / p"foo" / p"bar")
      
      test(t"Find conjunction of paths with common last name"):
        val p1 = % / p"foo" / p"bar" / p"quux"
        val p2 = % / p"foo" / p"baz" / p"quux"
        p1.conjunction(p2)
      .assert(_ == % / p"foo")
      
      test(t"Find conjunction of paths with different length"):
        val p1 = % / p"foo" / p"bar"
        val p2 = % / p"foo" / p"baz" / p"quux"
        p1.conjunction(p2)
      .assert(_ == % / p"foo")

      test(t"Find relation between identical paths"):
        val p1 = % / p"foo" / p"bar"
        val p2 = % / p"foo" / p"bar"
        p1.relativeTo(p2)
      .assert(_ == ?)
      
      test(t"Find relation to child path"):
        val p1 = % / p"foo" / p"bar"
        val p2 = % / p"foo" / p"bar" / p"baz"
        p1.relativeTo(p2)
      .assert(_ == ? / p"baz")
      
      test(t"Find relation to parent path"):
        val p1 = % / p"foo" / p"bar"
        val p2 = % / p"foo"
        p1.relativeTo(p2)
      .assert(_ == ?^)
      
      test(t"Find relation between distinct paths"):
        val p1 = % / p"foo" / p"bar"
        val p2 = % / p"bar" / p"baz"
        p1.relativeTo(p2)
      .assert(_ == ?^^ / p"bar" / p"baz")
      
      test(t"Find relation between paths with one common dir"):
        val p1 = % / p"foo" / p"bar"
        val p2 = % / p"foo" / p"baz"
        p1.relativeTo(p2)
      .assert(_ == ?^ / p"baz")
      
      test(t"Find relation between paths with two common dirs"):
        val p1 = % / p"foo" / p"bar" / p"baz"
        val p2 = % / p"foo" / p"bar" / p"quux"
        p1.relativeTo(p2)
      .assert(_ == ?^ / p"quux")
      
      test(t"Find relation between paths with common last name"):
        val p1 = % / p"foo" / p"bar" / p"quux"
        val p2 = % / p"foo" / p"baz" / p"quux"
        p1.relativeTo(p2)
      .assert(_ == ?^^ / p"baz" / p"quux")
      
      test(t"Find relation between paths with different length"):
        val p1 = % / p"foo" / p"bar"
        val p2 = % / p"foo" / p"baz" / p"quux"
        p1.relativeTo(p2)
      .assert(_ == ?^ / p"baz" / p"quux")
      
      test(t"precedence of root"):
        %.precedes(% / p"foo")
      .assert(_ == true)
      
      test(t"self-precedence"):
        (% / p"foo").precedes(% / p"foo")
      .assert(_ == true)
      
      test(t"non-precedence"):
        (% / p"foo").precedes(% / p"bar")
      .assert(_ == false)
      
      test(t"non-precedence with root"):
        (% / p"foo").precedes(%)
      .assert(_ == false)
      
      test(t"add relative parent"):
        unsafely:
          val rel = ?^
          (% / p"foo" / p"bar") + rel
      .assert(_ == % / p"foo")
      
      test(t"Parent of root throws exception"):
        unsafely:
          val rel = ?^^
          capture[PathError]((% / p"foo") + rel)
      .assert(_ == PathError(PathError.Reason.ParentOfRoot))
      
      test(t"add relative uncle"):
        unsafely:
          val rel = ?^^ / p"quux"
          (% / p"foo" / p"bar" / p"baz") + rel
      .assert(_ == % / p"foo" / p"quux")
      
      test(t"add relative cousin"):
        unsafely:
          val rel = ?^^ / p"quux" / p"bar"
          (% / p"foo" / p"bar" / p"baz") + rel
      .assert(_ == % / p"foo" / p"quux" / p"bar")

    suite(t"Test path tests"):
      test(t"Absolute path child"):
        TestPath(Drive('C'), List(p"Windows")) / p"System32"
      .assert(_ == TestPath(Drive('C'), List(p"System32", p"Windows")))
    
      test(t"Absolute path parent"):
        TestPath(Drive('C'), List(p"System32", p"Windows")).parent
      .assert(_ == TestPath(Drive('C'), List(p"Windows")))
      
      test(t"Absolute path root parent"):
        TestPath(Drive('C'), List()).parent
      .assert(_ == Unset)
  
      test(t"Relative path child"):
        TestLink(3, List(p"docs", p"work")) / p"images"
      .assert(_ == TestLink(3, List(p"images", p"docs", p"work")))
      
      test(t"Relative path parent"):
        TestLink(3, List(p"file", p"docs", p"work")).parent
      .assert(_ == TestLink(3, List(p"docs", p"work")))
  
      test(t"Relative root parent"):
        TestLink(3, List()).parent
      .assert(_ == TestLink(4, List()))

    suite(t"Path rendering"):
      test(t"Show a rooted absolute path"):
        TestPath(Drive('F'), List(p"System32", p"Windows")).show
      .assert(_ == t"F:\\Windows\\System32")
      
      test(t"Show a simple absolute path"):
        SimplePath(List(p"user", p"home")).show
      .assert(_ == t"/home/user")
      
      test(t"Show a rooted relative path"):
        TestLink(2, List(p"Data", p"Work")).show
      .assert(_ == t"..\\..\\Work\\Data")
      
      test(t"Show a simple relative path"):
        SimpleLink(2, List(p"file", p"user")).show
      .assert(_ == t"../../user/file")

    suite(t"Invalid paths"):
      given MainRoot[TestPath] = () => TestPath(Drive('C'), Nil)
      
      test(t"Path cannot contain /"):
        demilitarize:
          SimplePath(List()) / p"a/b"
        .map(_.message)
      .assert(_ == List(t"serpentine: a path element may not contain the character /"))
      
      test(t"Test Path cannot contain lpt1"):
        demilitarize:
          Drive('C') / p"lpt1"
        .map(_.message)
      .assert(_ == List(t"serpentine: a path element may not start with lpt1"))
      
      test(t"Test Path cannot contain lpt1.txt"):
        demilitarize:
          Drive('C') / p"lpt1.txt"
        .map(_.message)
      .assert(_ == List(t"serpentine: a path element may not start with lpt1"))
      
      test(t"Test Path cannot have a filename ending in space"):
        demilitarize:
          Drive('C') / p"abc.xyz "
        .map(_.message)
      .assert(_ == List(t"serpentine: a path element may not match the pattern .*"))
      
      test(t"Test Path cannot have a filename ending in period"):
        demilitarize:
          Drive('C') / p"abc.abc"
        .map(_.message)
      .assert(_ == List(t"serpentine: a path element may not end with abc"))
      
      test(t"Test Path can have an extensionless filename"):
        demilitarize:
          Drive('C') / p"xyz"
      .assert(_ == Nil)

