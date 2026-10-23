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

package degustation

import java.nio.charset.StandardCharsets

import scala.collection.immutable.Set
import scala.collection.mutable

import anticipation.*
import gossamer.*
import murmuration.*
import vacuous.*

import Pickled.*

// The little of a Java classfile that erasure and member lookup need: the class's name, kind and
// supertypes, and its members' names and descriptors — which, for a Java member, already *are*
// its erased signature. Nothing generic is read: a Java type parameter erases to its bound, which
// the descriptor spells out, and the erasure of a Scala reference to a Java class is the class.
object Classfile:
  case class Method(name: Text, descriptor: Text, flags: Int)
  case class Field(name: Text, descriptor: Text, flags: Int)

  case class Class
    ( name:       Text,
      flags:      Int,
      superclass: Optional[Text],
      interfaces: List[Text],
      fields:     List[Field],
      methods:    List[Method],
      // The binary name of the enclosing class, from the `InnerClasses` attribute, and whether
      // the class is static within it.
      outer:      Optional[Text],
      static:     Boolean ):

    def isInterface: Boolean = (flags & 0x0200) != 0

  private val Interface = 0x0200

  // Reads the constant pool, the class header and the member tables; attributes are skipped.
  def apply(data: Data): Class =
    val bytes = Array.unsafeJvm(data)
    var position = 0

    def u1(): Int =
      val value = bytes(position) & 0xff
      position += 1
      value

    def u2(): Int = (u1() << 8) | u1()
    def u4(): Int = (u2() << 16) | u2()

    if u4() != 0xcafebabe then throw Malformed(t"not a classfile")
    u2() // minor
    u2() // major

    val count = u2()
    val utf8 = mutable.HashMap[Int, Text]()
    val classes = mutable.HashMap[Int, Int]()
    var index = 1

    while index < count do
      u1() match
        case 1 =>
          val length = u2()
          utf8(index) = String(bytes, position, length, StandardCharsets.UTF_8).tt
          position += length

        case 3 | 4                      => position += 4
        case 5 | 6                      => position += 8; index += 1
        case 7                          => classes(index) = u2()
        case 8                          => position += 2
        case 9 | 10 | 11 | 12 | 17 | 18 => position += 4
        case 15                         => position += 3
        case 16                         => position += 2
        case 19 | 20                    => position += 2
        case other                      => throw Malformed(t"unknown constant pool tag $other")

      index += 1

    def className(reference: Int): Text =
      utf8.get(classes.getOrElse(reference, -1)) match
        case Some(name) => name.s.replace('/', '.').nn.tt
        case None       => throw Malformed(t"bad class reference $reference")

    val flags = u2()
    val name = className(u2())
    val superIndex = u2()
    val superclass: Optional[Text] = if superIndex == 0 then Unset else className(superIndex)
    val interfaceCount = u2()
    val interfaces = List.fill(interfaceCount)(className(u2()))

    def skipAttributes(): Unit =
      val attributes = u2()
      var attribute = 0

      // The length is read before `position` advances: `position += u4()` would read `position`
      // first and lose the four bytes `u4` consumes.
      while attribute < attributes do
        u2()
        val length = u4()
        position += length
        attribute += 1

    def member[result](make: (Text, Text, Int) => result): result =
      val memberFlags = u2()
      val memberName = utf8.getOrElse(u2(), t"")
      val descriptor = utf8.getOrElse(u2(), t"")
      skipAttributes()
      make(memberName, descriptor, memberFlags)

    val fields = List.fill(u2())(member(Field(_, _, _)))
    val methods = List.fill(u2())(member(Method(_, _, _)))

    // Of the class's attributes, only `InnerClasses` is read, for the entry describing the class
    // itself: its enclosing class and whether it is static there.
    var outer: Optional[Text] = Unset
    var static = false
    val attributes = u2()
    var attribute = 0

    while attribute < attributes do
      val attributeName = utf8.getOrElse(u2(), t"")
      val length = u4()
      val end = position + length

      if attributeName == t"InnerClasses" then
        val entries = u2()
        var entry = 0

        while entry < entries do
          val inner = u2()
          val outerIndex = u2()
          u2()
          val innerFlags = u2()

          if outerIndex != 0 && classes.contains(inner) && className(inner) == name then
            outer = className(outerIndex)
            static = (innerFlags & 0x0008) != 0

          entry += 1

      position = end
      attribute += 1

    Class(name, flags, superclass, interfaces, fields, methods, outer, static)

  // A descriptor's types as the model's types: references to the named classes, with primitives
  // as the Scala value classes they correspond to and arrays as `scala.Array` applications.
  object Descriptor:
    def parameters(descriptor: Text): List[Type] = parse(descriptor)(0)
    def result(descriptor: Text): Type = parse(descriptor)(1)

    def field(descriptor: Text): Type =
      val (parsed, _) = tpe(descriptor.s, 0)
      parsed

    private def parse(descriptor: Text): (List[Type], Type) =
      val text = descriptor.s
      var position = 1
      val params = mutable.ListBuffer[Type]()

      while text.charAt(position) != ')' do
        val (tpe, next) = this.tpe(text, position)
        params += tpe
        position = next

      val (result, _) = tpe(text, position + 1)
      (params.to(List), result)

    private def tpe(text: String, start: Int): (Type, Int) = text.charAt(start) match
      case 'B' => (scala(t"Byte"), start + 1)
      case 'C' => (scala(t"Char"), start + 1)
      case 'D' => (scala(t"Double"), start + 1)
      case 'F' => (scala(t"Float"), start + 1)
      case 'I' => (scala(t"Int"), start + 1)
      case 'J' => (scala(t"Long"), start + 1)
      case 'S' => (scala(t"Short"), start + 1)
      case 'Z' => (scala(t"Boolean"), start + 1)
      case 'V' => (scala(t"Unit"), start + 1)

      case '[' =>
        val (element, next) = tpe(text, start + 1)
        (Type.Applied(scala(t"Array"), List(element)), next)

      case 'L' =>
        val end = text.indexOf(';', start)
        val name = text.substring(start + 1, end).nn.replace('/', '.').nn.tt
        (Universe.classRef(name), end + 1)

      case other => throw Malformed(t"bad descriptor $text")

    private def scala(name: Text): Type = Type.TypeRef(name, Type.TermRefPkg(t"scala"))

  case class Malformed(detail: Text) extends RuntimeException(detail.s)
