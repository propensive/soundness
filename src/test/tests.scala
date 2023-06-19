/*
    Serpentine, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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
import gossamer.*
import larceny.*

object Tests extends Suite(t"Serpentine Tests"):
  def run(): Unit =
    suite(t"Absolute parsing"):
      test(t"parse simple absolute path"):
        unsafely(Unix.AbsolutePath.parse(t"/home"))
      .assert(_ == Unix.AbsolutePath(Unix, List(PathName(t"home"))))
      
      test(t"parse deeper absolute path"):
        unsafely(Unix.AbsolutePath.parse(t"/home/work"))
      .assert(_ == Unix.AbsolutePath(Unix, List(PathName(t"work"), PathName(t"home"))))
      
      test(t"parse even deeper absolute path"):
        unsafely(Unix.AbsolutePath.parse(t"/home/work/data"))
      .assert(_ == Unix.AbsolutePath(Unix, List(PathName(t"data"), PathName(t"work"), PathName(t"home"))))
      
      test(t"parse even absolute directory-style path"):
        unsafely(Unix.AbsolutePath.parse(t"/home/work/"))
      .assert(_ == Unix.AbsolutePath(Unix, List(PathName(t"work"), PathName(t"home"))))
      
      test(t"parse simple Windows absolute path"):
        unsafely(Windows.AbsolutePath.parse(t"C:\\Windows"))
      .assert(_ == Windows.AbsolutePath(Drive('C'), List(PathName(t"Windows"))))
      
      test(t"parse deeper Windows absolute path"):
        unsafely(Windows.AbsolutePath.parse(t"D:\\Windows\\System"))
      .assert(_ == Windows.AbsolutePath(Drive('D'), List(PathName(t"System"), PathName(t"Windows"))))
      
      test(t"parse even deeper Windows absolute path"):
        unsafely(Windows.AbsolutePath.parse(t"e:\\Windows\\System\\Data"))
      .assert(_ == Windows.AbsolutePath(Drive('E'), List(PathName(t"Data"), PathName(t"System"), PathName(t"Windows"))))
      
      test(t"parse even absolute Windows directory-style path"):
        unsafely(Windows.AbsolutePath.parse(t"f:\\Windows\\System\\"))
      .assert(_ == Windows.AbsolutePath(Drive('F'), List(PathName(t"System"), PathName(t"Windows"))))
      
    suite(t"Relative parsing"):
      test(t"parse simple relative path"):
        unsafely(Unix.RelativePath.parse(t"peer"))
      .assert(_ == Unix.RelativePath(0, List(PathName(t"peer"))))

      test(t"parse three-part relative subpath"):
        unsafely(Unix.RelativePath.parse(t"path/to/child"))
      .assert(_ == Unix.RelativePath(0, List(t"child", t"to", t"path").map(PathName(_))))

      test(t"parse parent relative path"):
        unsafely(Unix.RelativePath.parse(t".."))
      .assert(_ == Unix.RelativePath(1, List()))

      test(t"parse ancestor relative path"):
        unsafely(Unix.RelativePath.parse(t"../../.."))
      .assert(_ == Unix.RelativePath(3, List()))
    
      test(t"parse relative link to current path"):
        unsafely(Unix.RelativePath.parse(t"."))
      .assert(_ == Unix.RelativePath(0, List()))
      
      test(t"parse relative link to uncle path"):
        unsafely(Unix.RelativePath.parse(t"../path"))
      .assert(_ == Unix.RelativePath(1, List(PathName(t"path"))))
      
      test(t"parse relative link to cousin path"):
        unsafely(Unix.RelativePath.parse(t"../path/child"))
      .assert(_ == Unix.RelativePath(1, List(t"child", t"path").map(PathName(_))))

    suite(t"Show paths"):
      given Unix.type = Unix
      
      test(t"show simple relative path"):
        (? / p"hello").text
      .assert(_ == t"hello")
      
      test(t"show two-level relative path"):
        (? / p"hello" / p"world").text
      .assert(_ == t"hello/world")
      
      test(t"show self"):
        ?.text
      .assert(_ == t".")
      
      test(t"show self's parent"):
        ?.parent.text
      .assert(_ == t"..")
      
      test(t"show self's grandparent"):
        ?.parent.parent.text
      .assert(_ == t"../..")
      
      test(t"show sister path"):
        (?.parent / p"foo").text
      .assert(_ == t"../foo")
      
      test(t"show uncle path"):
        (?.parent.parent / p"foo").text
      .assert(_ == t"../../foo")
      
      test(t"show cousin path"):
        (?.parent.parent / p"foo" / p"bar").text
      .assert(_ == t"../../foo/bar")

      test(t"show a simple generic path"):
        (% / p"foo").text
      .assert(_ == t"/foo")

      test(t"show a deeper generic path"):
        (% / p"foo" / p"bar").text
      .assert(_ == t"/foo/bar")
      
      test(t"show the root path"):
        %.text
      .assert(_ == t"/")
    
    suite(t"Path tests"):
      test(t"simple path from forbidden string does not compile"):
        captureCompileErrors:
          val elem: PathName["bad"] = p"bad"
        .map(_.message)
      .assert(_ == List(t"serpentine: a path element may not be 'bad'"))
      
      test(t"simple path from forbidden set of strings does not compile"):
        captureCompileErrors:
          val elem: PathName["bad" | "awful"] = p"bad"
        .map(_.message)
      .assert(_ == List(t"serpentine: a path element may not be 'bad'"))
      
      test(t"simple path not in forbidden set of strings does compile"):
        captureCompileErrors:
          val elem: PathName["bad" | "awful"] = p"safe"
      .assert(_ == Nil)
      
      test(t"path with forbidden character does compile"):
        captureCompileErrors:
          val elem: PathName["bad" | ".*n.*"] = p"unsafe"
        .map(_.message)
      .assert(_ == List(t"serpentine: a path element may not contain the character 'n'"))
      
      test(t"path with forbidden character does compile"):
        captureCompileErrors:
          val elem: PathName[".*a.*" | ".*e.*" | ".*i.*" | ".*o.*" | ".*u.*"] = p"unsafe"
        .map(_.message)
      .assert(_ == List(t"serpentine: a path element may not contain the character 'a'"))

      test(t"Parse a path name with an invalid character"):
        import unsafeExceptions.canThrowAny
        capture[PathError, PathName[".*x.*"]](PathName[".*x.*"](t"excluded"))
      .assert(_ == PathError(PathError.Reason.InvalidChar('x')))
      
      test(t"Parse a path name with an invalid suffix"):
        import unsafeExceptions.canThrowAny
        capture[PathError, PathName[".*txt"]](PathName[".*txt"](t"bad.txt"))
      .assert(_ == PathError(PathError.Reason.InvalidSuffix(t"txt")))

      test(t"Parse a path name with an invalid prefix"):
        import unsafeExceptions.canThrowAny
        capture[PathError, PathName["bad.*"]](PathName["bad.*"](t"bad.txt"))
      .assert(_ == PathError(PathError.Reason.InvalidPrefix(t"bad")))
      
      test(t"Parse a path name with an invalid name"):
        import unsafeExceptions.canThrowAny
        capture[PathError, PathName["bad\\.txt"]](PathName["bad\\.txt"](t"bad.txt"))
      .assert(_ == PathError(PathError.Reason.InvalidName(t"bad\\.txt")))

    suite(t"Relative path tests"):
      given Unix.type = Unix
      test(t"Relative path has correct parent"):
        (? / p"foo" / p"bar").parent
      .assert(_ == (? / p"foo"))

      test(t"Relativepath has correct parent 2"):
        (? / p"foo").parent
      .assert(_ == ?)

      test(t"Parent of Relative path root has correct parent"):
        ?^
      .assert(_ == Unix.RelativePath(1, Nil))
      
      test(t"Parent of Relative path root has correct parent"):
        ?^^
      .assert(_ == Unix.RelativePath(2, Nil))
      
      test(t"Parent of cousin keeps correct ascent"):
        (?^^ / p"foo" / p"bar").parent
      .assert(_ == ?^^ / p"foo")
      
      test(t"Parent of cousin keeps correct ascent 2"):
        (?^^ / p"foo").parent
      .assert(_ == ?^^)

    suite(t"Relative path tests"):
      given Unix.type = Unix
      
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
      .assert(_ == Unix() / p"foo" / p"bar")
      
      test(t"Find conjunction of paths with common last name"):
        val p1 = % / p"foo" / p"bar" / p"quux"
        val p2 = % / p"foo" / p"baz" / p"quux"
        p1.conjunction(p2)
      .assert(_ == Unix() / p"foo")
      
      test(t"Find conjunction of paths with different length"):
        val p1 = % / p"foo" / p"bar"
        val p2 = % / p"foo" / p"baz" / p"quux"
        p1.conjunction(p2)
      .assert(_ == Unix() / p"foo")

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
        given CanThrow[PathError] = unsafeExceptions.canThrowAny
        val rel = ?^
        (% / p"foo" / p"bar") + rel
      .assert(_ == % / p"foo")
      
      test(t"add relative grandparent"):
        given CanThrow[PathError] = unsafeExceptions.canThrowAny
        val rel = ?^^
        (% / p"foo" / p"bar" / p"baz") + rel
      .assert(_ == % / p"foo")
      
      test(t"add relative uncle"):
        given CanThrow[PathError] = unsafeExceptions.canThrowAny
        val rel = ?^^ / p"quux"
        (% / p"foo" / p"bar" / p"baz") + rel
      .assert(_ == % / p"foo" / p"quux")
      
      test(t"add relative cousin"):
        given CanThrow[PathError] = unsafeExceptions.canThrowAny
        val rel = ?^^ / p"quux" / p"bar"
        (% / p"foo" / p"bar" / p"baz") + rel
      .assert(_ == % / p"foo" / p"quux" / p"bar")



    suite(t"Windows path tests"):
      import Windows.{AbsolutePath, RelativePath, Drive}
      
      test(t"Absolute path child"):
        AbsolutePath(Drive('C'), List(p"Windows")) / p"System32"
      .assert(_ == AbsolutePath(Drive('C'), List(p"System32", p"Windows")))
    
      test(t"Absolute path parent"):
        AbsolutePath(Drive('C'), List(p"System32", p"Windows")).parent
      .assert(_ == AbsolutePath(Drive('C'), List(p"Windows")))
      
      test(t"Absolute path root parent"):
        AbsolutePath(Drive('C'), List()).parent
      .assert(_ == Unset)
  
      test(t"Relative path child"):
        RelativePath(3, List(p"docs", p"work")) / p"images"
      .assert(_ == RelativePath(3, List(p"images", p"docs", p"work")))
      
      test(t"Relative path parent"):
        RelativePath(3, List(p"file", p"docs", p"work")).parent
      .assert(_ == RelativePath(3, List(p"docs", p"work")))
  
      test(t"Relative root parent"):
        RelativePath(3, List()).parent
      .assert(_ == RelativePath(4, List()))

    suite(t"Path rendering"):
      test(t"Show a Windows absolute path"):
        Windows.AbsolutePath(Windows.Drive('F'), List(p"System32", p"Windows")).text
      .assert(_ == t"F:\\Windows\\System32")
      
      test(t"Show a UNIX absolute path"):
        Unix.AbsolutePath(Unix, List(p"user", p"home")).text
      .assert(_ == t"/home/user")
      
      test(t"Show a Windows relative path"):
        Windows.RelativePath(2, List(p"Data", p"Work")).text
      .assert(_ == t"..\\..\\Work\\Data")
      
      test(t"Show a UNIX relative path"):
        Unix.RelativePath(2, List(p"file", p"user")).text
      .assert(_ == t"../../user/file")

    suite(t"Invalid paths"):
      test(t"Path cannot contain /"):
        captureCompileErrors(Unix.AbsolutePath(Unix, List()) / p"a/b")
      .assert(_.length == 1)
      
      test(t"Windows Path cannot contain lpt1"):
        captureCompileErrors(Drive('C') / p"lpt1")
      .assert(_.length == 1)
      
      test(t"Windows Path cannot contain lpt1.txt"):
        captureCompileErrors(Drive('C') / p"lpt1.txt")
      .assert(_.length == 1)
      
      test(t"Linux can contain lpt1.txt"):
        given Unix.type = Unix
        (% / p"lpt1.txt").text
      .assert(_ == t"/lpt1.txt")
      
      test(t"Windows Path cannot have a filename ending in space"):
        captureCompileErrors(Drive('C') / p"abc.xyz ")
      .assert(_.length == 1)
      
      test(t"Windows Path cannot have a filename ending in period"):
        captureCompileErrors(Drive('C') / p"abc.")
      .assert(_.length == 1)
      
      test(t"Windows Path can have an extensionless filename"):
        captureCompileErrors(Drive('C') / p"abc")
      .assert(_.length == 0)

