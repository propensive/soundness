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
    suite(t"Relative parsing"):
      test(t"parse simple relative path"):
        unsafely(Relative.parse(t"peer"))
      .assert(_ == Relative(0, List(t"peer")))

      test(t"parse three-part relative subpath"):
        unsafely(Relative.parse(t"path/to/child"))
      .assert(_ == Relative(0, List(t"child", t"to", t"path")))

      test(t"parse parent relative path"):
        unsafely(Relative.parse(t".."))
      .assert(_ == Relative(1, List()))

      test(t"parse ancestor relative path"):
        unsafely(Relative.parse(t"../../.."))
      .assert(_ == Relative(3, List()))
    
      test(t"parse relative link to current path"):
        unsafely(Relative.parse(t"."))
      .assert(_ == Relative(0, List()))
      
      test(t"parse relative link to uncle path"):
        unsafely(Relative.parse(t"../path"))
      .assert(_ == Relative(1, List(t"path")))
      
      test(t"parse relative link to cousin path"):
        unsafely(Relative.parse(t"../path/child"))
      .assert(_ == Relative(1, List(t"child", t"path")))

    suite(t"Show paths"):
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
        (^ / p"foo").show
      .assert(_ == t"/foo")

      test(t"show a deeper generic path"):
        (^ / p"foo" / p"bar").show
      .assert(_ == t"/foo/bar")
      
      test(t"show the root path"):
        ^.show
      .assert(_ == t"/")
    
    suite(t"Path tests"):
      test(t"simple path from forbidden string does not compile"):
        captureCompileErrors:
          val elem: PathElement["bad"] = p"bad"
        .map(_.message)
      .assert(_ == List(t"serpentine: 'bad' is not a valid name for a path element"))
      
      test(t"simple path from forbidden set of strings does not compile"):
        captureCompileErrors:
          val elem: PathElement["bad" | "awful"] = p"bad"
        .map(_.message)
      .assert(_ == List(t"serpentine: 'bad' is not a valid name for a path element"))
      
      test(t"simple path not in forbidden set of strings does compile"):
        captureCompileErrors:
          val elem: PathElement["bad" | "awful"] = p"safe"
      .assert(_ == Nil)
      
      test(t"path with forbidden character does compile"):
        captureCompileErrors:
          val elem: PathElement["bad" | 'n'] = p"unsafe"
        .map(_.message)
      .assert(_ == List(t"serpentine: the character 'n' is not permitted in a path element"))
      
      test(t"path with forbidden character does compile"):
        captureCompileErrors:
          val elem: PathElement['a' | 'e' | 'i' | 'o' | 'u'] = p"unsafe"
        .map(_.message)
      .assert(_ == List(t"serpentine: the character 'a' is not permitted in a path element"))

      case class Address(elements: List[PathElement['!' | ',' | '*' | '/' | ""]])

      object Address extends Address(Nil)

      given Hierarchy[Address] with
        type ForbiddenType = '!' | ',' | '*' | '/' | ""
        def separator: Text = t"\n"
        def prefix(root: Address): Text = t""
        def elements(address: Address): List[PathElement[ForbiddenType]] = address.elements
        def root(address: Address): Address.type = Address
        def child(base: Address, child: PathElement[ForbiddenType]): Address = Address(child :: base.elements)
        def parent(base: Address): Address = Address(base.elements.tail)
      
      test(t"Simple path is permitted"):
        captureCompileErrors(Address / p"foo")
      .assert(_ == Nil)
      
      test(t"Child path is permitted"):
        captureCompileErrors(Address / p"foo" / p"baz")
      .assert(_ == Nil)
      
      test(t"Bad child path is forbidden"):
        captureCompileErrors(Address / p"foo" / p"ba*r").map(_.message)
      .assert(_ == List(t"serpentine: the character '*' is not permitted in a path element"))
      
      test(t"Forbidden path elements are inferred"):
        captureCompileErrors(Address / p"foo!").map(_.message)
      .assert(_ == List(t"serpentine: the character '!' is not permitted in a path element"))

      test(t"Relative path's parent is safe"):
        val relative = Relative(3, List(t"some"))
        relative.parent
      .assert(_ == Relative(3, Nil))
      
      test(t"Non-relative parent is not safe"):
        captureCompileErrors:
          val absolute = Address / p"foo" / p"bar"
          absolute.parent
      .assert(_.length == 1)
      
      test(t"Non-relative parent is safe in try/catch"):
        captureCompileErrors:
          given CanThrow[PathError] = unsafeExceptions.canThrowAny
          val absolute = Address / p"foo" / p"bar"
          absolute.parent
      .assert(_ == List())

      test(t"Relative path with safe path elements"):
        ? / p"foo" / p"bar"
      .assert(_ == Relative(0, List(t"bar", t"foo")))
      
      test(t"Relative path cannot have '..' path"):
        captureCompileErrors:
          ? / p"foo" / p"bar" / p".." / p"baz"
      .assert(_.length == 1)
      
      test(t"Relative path cannot have '/' in a path"):
        captureCompileErrors(? / p"foo" / p"bar/baz").map(_.errorId)
      .assert(_.length == 1)
    
    suite(t"Relative path tests"):
      test(t"Relative path has correct parent"):
        (? / p"foo" / p"bar").parent
      .assert(_ == (? / p"foo"))

      test(t"Relativepath has correct parent 2"):
        (? / p"foo").parent
      .assert(_ == ?)

      test(t"Parent of Relative path root has correct parent"):
        ?.parent
      .assert(_ == Relative(1, Nil))
      
      test(t"Parent of Relative path root has correct parent"):
        ?.parent.parent
      .assert(_ == Relative(2, Nil))
      
      test(t"Parent of cousin keeps correct ascent"):
        (?.parent.parent / p"foo" / p"bar").parent
      .assert(_ == ?.parent.parent / p"foo")
      
      test(t"Parent of cousin keeps correct ascent 2"):
        (?.parent.parent / p"foo").parent
      .assert(_ == ?.parent.parent)

    suite(t"Relative path tests"):
      test(t"Find conjunction of distinct paths"):
        val p1 = ^ / p"foo" / p"bar"
        val p2 = ^ / p"bar" / p"baz"
        p1.conjunction(p2)
      .assert(_ == ^)
      
      test(t"Find conjunction of paths with one common dir"):
        val p1 = ^ / p"foo" / p"bar"
        val p2 = ^ / p"foo" / p"baz"
        p1.conjunction(p2)
      .assert(_ == ^ / p"foo")
      
      test(t"Find conjunction of paths with two common dirs"):
        val p1 = ^ / p"foo" / p"bar" / p"baz"
        val p2 = ^ / p"foo" / p"bar" / p"quux"
        p1.conjunction(p2)
      .assert(_ == ^ / p"foo" / p"bar")
      
      test(t"Find conjunction of paths with common last name"):
        val p1 = ^ / p"foo" / p"bar" / p"quux"
        val p2 = ^ / p"foo" / p"baz" / p"quux"
        p1.conjunction(p2)
      .assert(_ == ^ / p"foo")
      
      test(t"Find conjunction of paths with different length"):
        val p1 = ^ / p"foo" / p"bar"
        val p2 = ^ / p"foo" / p"baz" / p"quux"
        p1.conjunction(p2)
      .assert(_ == ^ / p"foo")

      test(t"Find relation between identical paths"):
        val p1 = ^ / p"foo" / p"bar"
        val p2 = ^ / p"foo" / p"bar"
        p1.relativeTo(p2)
      .assert(_ == ?)
      
      test(t"Find relation to child path"):
        val p1 = ^ / p"foo" / p"bar"
        val p2 = ^ / p"foo" / p"bar" / p"baz"
        p1.relativeTo(p2)
      .assert(_ == ? / p"baz")
      
      test(t"Find relation to parent path"):
        val p1 = ^ / p"foo" / p"bar"
        val p2 = ^ / p"foo"
        p1.relativeTo(p2)
      .assert(_ == ?.parent)
      
      test(t"Find relation between distinct paths"):
        val p1 = ^ / p"foo" / p"bar"
        val p2 = ^ / p"bar" / p"baz"
        p1.relativeTo(p2)
      .assert(_ == ?.parent.parent / p"bar" / p"baz")
      
      test(t"Find relation between paths with one common dir"):
        val p1 = ^ / p"foo" / p"bar"
        val p2 = ^ / p"foo" / p"baz"
        p1.relativeTo(p2)
      .assert(_ == ?.parent / p"baz")
      
      test(t"Find relation between paths with two common dirs"):
        val p1 = ^ / p"foo" / p"bar" / p"baz"
        val p2 = ^ / p"foo" / p"bar" / p"quux"
        p1.relativeTo(p2)
      .assert(_ == ?.parent / p"quux")
      
      test(t"Find relation between paths with common last name"):
        val p1 = ^ / p"foo" / p"bar" / p"quux"
        val p2 = ^ / p"foo" / p"baz" / p"quux"
        p1.relativeTo(p2)
      .assert(_ == ?.parent.parent / p"baz" / p"quux")
      
      test(t"Find relation between paths with different length"):
        val p1 = ^ / p"foo" / p"bar"
        val p2 = ^ / p"foo" / p"baz" / p"quux"
        p1.relativeTo(p2)
      .assert(_ == ?.parent / p"baz" / p"quux")
      
      test(t"precedence of root"):
        ^.precedes(^ / p"foo")
      .assert(_ == true)
      
      test(t"self-precedence"):
        (^ / p"foo").precedes(^ / p"foo")
      .assert(_ == true)
      
      test(t"non-precedence"):
        (^ / p"foo").precedes(^ / p"bar")
      .assert(_ == false)
      
      test(t"non-precedence with root"):
        (^ / p"foo").precedes(^)
      .assert(_ == false)
      
      test(t"add relative parent"):
        given CanThrow[PathError] = unsafeExceptions.canThrowAny
        val rel = ?.parent
        (^ / p"foo" / p"bar") ++ rel
      .assert(_ == ^ / p"foo")
      
      test(t"add relative grandparent"):
        given CanThrow[PathError] = unsafeExceptions.canThrowAny
        val rel = ?.parent.parent
        (^ / p"foo" / p"bar" / p"baz") ++ rel
      .assert(_ == ^ / p"foo")
      
      test(t"add relative uncle"):
        given CanThrow[PathError] = unsafeExceptions.canThrowAny
        val rel = ?.parent.parent / p"quux"
        (^ / p"foo" / p"bar" / p"baz") ++ rel
      .assert(_ == ^ / p"foo" / p"quux")
      
      test(t"add relative cousin"):
        given CanThrow[PathError] = unsafeExceptions.canThrowAny
        val rel = ?.parent.parent / p"quux" / p"bar"
        (^ / p"foo" / p"bar" / p"baz") ++ rel
      .assert(_ == ^ / p"foo" / p"quux" / p"bar")