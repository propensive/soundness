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
package mandible

import java.lang.classfile as jlc
import java.lang.classfile.attribute as jlca

import anticipation.*
import gossamer.*
import rudiments.*
import vacuous.*

// The *declaration* surface of a classfile, as distinct from the bytecode surface `Classfile`
// exposes. Everything a compiled consumer can bind to at link time is here — names, descriptors,
// access flags, the supertype chain and the generic signatures a recompiling consumer reads back
// — and nothing else is: `Code`, `SourceFile`, `LineNumberTable` and the annotation attributes
// are all dropped, since none of them is linkage surface, and two builds of the same sources may
// legitimately differ in all of them.
object ClassSurface:
  enum Kind:
    case Method, Field

  // A member reduced to what the JVM resolves against. `signature` is the generic signature
  // attribute, kept because erasure discards the type arguments a consumer's *next* compile
  // needs; `constant` is the `ConstantValue` a `static final` field may have had inlined into
  // consumers already.
  case class Member
    ( kind:       Kind,
      name:       Text,
      descriptor: Text,
      flags:      Int,
      signature:  Optional[Text] = Unset,
      exceptions: List[Text]     = Nil,
      constant:   Optional[Text] = Unset ):

    // The member's selector within its owning class; the atom key is the owner's name with this
    // appended, so methods and fields of the same name never collide.
    def selector: Text = kind match
      case Kind.Method => t"#$name:$descriptor"
      case Kind.Field  => t".$name:$descriptor"

    def public: Boolean    = (flags & jlc.ClassFile.ACC_PUBLIC) != 0
    def protect: Boolean   = (flags & jlc.ClassFile.ACC_PROTECTED) != 0
    def priv: Boolean      = (flags & jlc.ClassFile.ACC_PRIVATE) != 0
    def static: Boolean    = (flags & jlc.ClassFile.ACC_STATIC) != 0
    def abstrakt: Boolean  = (flags & jlc.ClassFile.ACC_ABSTRACT) != 0
    def bridge: Boolean    = (flags & jlc.ClassFile.ACC_BRIDGE) != 0
    def synthetic: Boolean = (flags & jlc.ClassFile.ACC_SYNTHETIC) != 0

    // Package-private and private members are not consumer surface: no consumer outside the
    // runtime package can resolve against them, so they never become atoms.
    def visible: Boolean = public || protect

    // A `static final` field of primitive or `String` type whose value javac may already have
    // copied into every consumer's constant pool (JLS 13.4.9). Its value is therefore
    // *replaceable* surface, not rigid — see `ClassfileAtomizer`.
    def inlinable: Boolean = kind == Kind.Field && static && constant.present

  private def text(entry: jlc.constantpool.Utf8Entry): Text = entry.stringValue.nn.tt

  private def signatureOf(attributes: List[jlc.Attribute[?]]): Optional[Text] =
    attributes.stdlib.collectFirst:
      case attribute: jlca.SignatureAttribute => attribute.signature.nn.stringValue.nn.tt

    . getOrElse(Unset)

  // The constant's *class* is folded alongside its value: `1` as an `Integer` and `"1"` as a
  // `String` are different contracts, and their `toString` forms are not.
  private def constantOf(attributes: List[jlc.Attribute[?]]): Optional[Text] =
    attributes.stdlib.collectFirst:
      case attribute: jlca.ConstantValueAttribute =>
        val value = attribute.constant.nn.constantValue.nn
        t"${value.getClass.nn.getName.nn.tt}:${value.toString.nn.tt}"

    . getOrElse(Unset)

  private def exceptionsOf(attributes: List[jlc.Attribute[?]]): List[Text] =
    attributes.stdlib.collectFirst:
      case attribute: jlca.ExceptionsAttribute =>
        // Sorted: the `Exceptions` attribute's order is source order, which is not contractual.
        List.from(attribute.exceptions.nn.to[List].stdlib.map(_.asInternalName.nn.tt).sorted)

    . getOrElse(Nil)

  def apply(data: scala.IArray[Byte]): ClassSurface =
    val model = jlc.ClassFile.of().nn.parse(data.asInstanceOf[scala.Array[Byte]]).nn

    val fields = model.fields.nn.to[List].map: field =>
      val attributes = field.attributes.nn.to[List]

      Member
        ( Kind.Field,
          text(field.fieldName.nn),
          text(field.fieldType.nn),
          field.flags.nn.flagsMask,
          signatureOf(attributes),
          Nil,
          constantOf(attributes) )

    val methods = model.methods.nn.to[List].map: method =>
      val attributes = method.attributes.nn.to[List]

      Member
        ( Kind.Method,
          text(method.methodName.nn),
          text(method.methodType.nn),
          method.flags.nn.flagsMask,
          signatureOf(attributes),
          exceptionsOf(attributes),
          Unset )

    // Members are sorted by selector: `java.lang.classfile` yields them in file order, and file
    // order is an artifact of the compilation run, not of the interface (§11.2 requirement 3).
    val members =
      List.from((fields.stdlib ++ methods.stdlib).sortBy { member => member.selector.s })

    ClassSurface
      ( model.thisClass.nn.asInternalName.nn.tt,
        model.flags.nn.flagsMask,
        Optional(model.superclass.nn.orElse(null)).let(_.asInternalName.nn.tt),
        List.from(model.interfaces.nn.to[List].stdlib.map(_.asInternalName.nn.tt).sorted),
        signatureOf(model.attributes.nn.to[List]),
        members )

// The parsed declaration surface of one class. `name`, `superclass` and `interfaces` are JVM
// internal names (`java/lang/String`), which is the form every reference in a classfile uses.
case class ClassSurface
  ( name:       Text,
    flags:      Int,
    superclass: Optional[Text],
    interfaces: List[Text],
    signature:  Optional[Text],
    members:    List[ClassSurface.Member] ):

  def public: Boolean    = (flags & jlc.ClassFile.ACC_PUBLIC) != 0
  def interface: Boolean = (flags & jlc.ClassFile.ACC_INTERFACE) != 0
  def isFinal: Boolean   = (flags & jlc.ClassFile.ACC_FINAL) != 0
  def synthetic: Boolean = (flags & jlc.ClassFile.ACC_SYNTHETIC) != 0

  // The supertypes named directly by this class, superclass first: the order the JVM's own
  // resolution walks, and the order membership keying inherits members in.
  def supertypes: List[Text] = superclass.lay(interfaces)(_ :: interfaces)

  def member(selector: Text): Optional[ClassSurface.Member] =
    members.stdlib.find { member => member.selector == selector }.getOrElse(Unset)
