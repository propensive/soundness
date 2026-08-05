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

import anticipation.*
import gossamer.*
import reliquary.*
import rudiments.*
import vacuous.*

// The atomization rules of `classfile/1`, over the declaration surface `ClassSurface` reads.
// Shared, deliberately, by two consumers with opposite relationships to the snapshot: the
// discipline, whose atoms *are* API identity, and the `jvm/1` profile, which compares the same
// atom sets out of band precisely so that they are not (LIRA §11.6).
//
// Two rules govern every decision below. First, the encoding is a function of the *interface*,
// never of the compilation run: file order, line tables, source names and the `Code` attribute
// are absent, and every list is sorted unless its order is itself semantic. Second, where it is
// unclear whether a property is linkage surface, it is folded in anyway: folding something
// inessential can only over-report a change, which grades a needless major, while omitting
// something essential under-reports a break, which is unsound.
object ClassfileAtomizer:
  val id: Text = t"classfile/1"

  // The result of atomizing one release's classes. `unresolved` names supertypes that were not
  // on the classpath: an unresolved supertype means an unknown member set, so the caller must
  // fail rather than atomize the smaller set it can see.
  case class Outcome(atoms: List[Atom], unresolved: List[Text])

  // Flag subsets, in a fixed bit order, chosen per element kind because the JVM reuses bit
  // values across kinds (`0x0040` is `ACC_BRIDGE` on a method and `ACC_VOLATILE` on a field).
  // `ACC_SUPER`, `ACC_SYNCHRONIZED`, `ACC_NATIVE` and `ACC_STRICT` are excluded: none is
  // resolvable surface, and all three of the latter vary with an implementation a consumer
  // cannot observe through linkage.
  private val classFlags: scala.List[Int] =
    scala.List
      ( jlc.ClassFile.ACC_PUBLIC, jlc.ClassFile.ACC_PROTECTED, jlc.ClassFile.ACC_FINAL,
        jlc.ClassFile.ACC_INTERFACE, jlc.ClassFile.ACC_ABSTRACT, jlc.ClassFile.ACC_SYNTHETIC,
        jlc.ClassFile.ACC_ANNOTATION, jlc.ClassFile.ACC_ENUM )

  private val methodFlags: scala.List[Int] =
    scala.List
      ( jlc.ClassFile.ACC_PUBLIC, jlc.ClassFile.ACC_PRIVATE, jlc.ClassFile.ACC_PROTECTED,
        jlc.ClassFile.ACC_STATIC, jlc.ClassFile.ACC_FINAL, jlc.ClassFile.ACC_BRIDGE,
        jlc.ClassFile.ACC_VARARGS, jlc.ClassFile.ACC_ABSTRACT, jlc.ClassFile.ACC_SYNTHETIC )

  private val fieldFlags: scala.List[Int] =
    scala.List
      ( jlc.ClassFile.ACC_PUBLIC, jlc.ClassFile.ACC_PRIVATE, jlc.ClassFile.ACC_PROTECTED,
        jlc.ClassFile.ACC_STATIC, jlc.ClassFile.ACC_FINAL, jlc.ClassFile.ACC_VOLATILE,
        jlc.ClassFile.ACC_TRANSIENT, jlc.ClassFile.ACC_SYNTHETIC, jlc.ClassFile.ACC_ENUM )

  private def bits(mask: Int, flags: scala.List[Int]): Long =
    flags.zipWithIndex.foldLeft(0L): (bits, pair) =>
      if (mask & pair(0)) != 0 then bits | (1L << pair(1)) else bits

  // --- canonical binary encoding ---------------------------------------------------------------
  // The same tag-length-value scheme `tasty/1` uses (`tasty.md` §7): unsigned LEB128 lengths,
  // length-prefixed UTF-8 strings, single-character constructor tags.

  private def uvarint(out: java.io.ByteArrayOutputStream, value0: Long): Unit =
    var value = value0

    while value >= 0x80L do
      out.write(((value & 0x7f) | 0x80).toInt)
      value >>>= 7

    out.write(value.toInt)

  private def utf8(out: java.io.ByteArrayOutputStream, text: Text): Unit =
    val bytes = text.s.getBytes("UTF-8").nn
    uvarint(out, bytes.length.toLong)
    out.write(bytes)

  private def tag(out: java.io.ByteArrayOutputStream, char: Char): Unit = out.write(char.toInt)

  private def optional(out: java.io.ByteArrayOutputStream, value: Optional[Text]): Unit =
    value.lay(tag(out, '0')):
      text =>
        tag(out, '1')
        utf8(out, text)

  private def texts(out: java.io.ByteArrayOutputStream, values: List[Text]): Unit =
    uvarint(out, values.stdlib.length.toLong)
    values.stdlib.foreach { value => utf8(out, value) }

  private def hash(encode: java.io.ByteArrayOutputStream => Unit): Data =
    val out = java.io.ByteArrayOutputStream()
    encode(out)
    LiraHash(LiraHash.Domain.Atom(id), Array.unsafeFrozen(out.toByteArray.nn))

  // --- the presented member set -----------------------------------------------------------------

  // Constructors are not inherited, and a class initializer is never resolvable surface.
  private def inheritable(member: ClassSurface.Member): Boolean =
    member.name != t"<init>" && member.name != t"<clinit>"

  private def declarable(member: ClassSurface.Member): Boolean = member.name != t"<clinit>"

  // Membership keying (§11.2 requirement 4): a JVM call site names the *receiver*, so the linkage
  // surface of a type includes every member it presents, not merely those it declares. The
  // presented set is this class's own visible members followed by those of its supertype closure,
  // nearest first, with the first occurrence of each selector winning — which is exactly how an
  // override, or a redeclaration that narrows nothing, shadows the inherited member.
  private def presented(surface: ClassSurface, index: ClasspathIndex)
  :   List[(ClassSurface.Member, Boolean)] =

    val (closure, _) = index.closure(surface.name)
    val seen = scala.collection.mutable.LinkedHashSet[Text]()
    val result = scala.collection.mutable.ListBuffer[(ClassSurface.Member, Boolean)]()

    surface.members.stdlib.foreach: member =>
      if member.visible && declarable(member) && seen.add(member.selector)
      then result += ((member, true))

    closure.stdlib.foreach: name =>
      index(name).let: parent =>
        parent.members.stdlib.foreach: member =>
          // A static member of an *interface* is not inherited by implementors (JLS 8.4.8), so it
          // presents only on the interface that declares it.
          val inherited = inheritable(member) && !(parent.interface && member.static)

          if member.visible && inherited && seen.add(member.selector)
          then result += ((member, false))

    List.from(result.toList)

  // --- atoms --------------------------------------------------------------------------------

  private def memberAtom(owner: Text, member: ClassSurface.Member): Atom =
    val flags = member.kind match
      case ClassSurface.Kind.Method => bits(member.flags, methodFlags)
      case ClassSurface.Kind.Field  => bits(member.flags, fieldFlags)

    // The presenting owner is folded into the *value*, not merely the key. The snapshot (§12.1)
    // hashes the set of *distinct* value hashes, so two membership atoms that differed only by
    // key would collapse to one, and dropping a member from one of several presenting types
    // would leave API identity unchanged — an under-report, and the one soundness trap
    // membership keying introduces.
    val encoding = hash: out =>
      tag(out, if member.kind == ClassSurface.Kind.Method then 'M' else 'F')
      utf8(out, owner)
      utf8(out, member.name)
      utf8(out, member.descriptor)
      uvarint(out, flags)
      optional(out, member.signature)
      texts(out, member.exceptions)
      optional(out, member.constant)

    // A `static final` field of constant type may already have been copied into every consumer's
    // constant pool (JLS 13.4.9), so its *value* is churn a recompile absorbs rather than a
    // linkage contract: replaceable, with no references, exactly as `resource/1` tracks content.
    val atomClass =
      if member.inlinable then AtomClass.Replaceable else AtomClass.Rigid

    Atom(t"$owner${member.selector}", atomClass, encoding)

  private def classAtom
    ( surface: ClassSurface, members: List[(ClassSurface.Member, Boolean)] )
  :   Atom =

    // Rule 5 of `tasty.md` §8, transposed: on a class open to subclassing, *adding* an abstract
    // method breaks every existing implementor, so the abstract member set folds into the class's
    // own atom and the addition grades major. On a final class nothing can implement it, the fold
    // is empty, and the same addition is pure extension.
    val abstracts =
      if surface.isFinal then Nil else List.from:
        members.stdlib.collect:
          case (member, _) if member.abstrakt => member.selector

        . sortBy(_.s)

    val encoding = hash: out =>
      tag(out, 'K')
      utf8(out, surface.name)
      uvarint(out, bits(surface.access, classFlags))
      optional(out, surface.superclass)
      // Interfaces keep no declaration order: the JVM resolves default methods by the class
      // hierarchy, not by the order of the `interfaces` array, and `ClassSurface` sorts them.
      texts(out, surface.interfaces)
      optional(out, surface.signature)
      texts(out, abstracts)

    Atom(surface.name, AtomClass.Rigid, encoding)

  // Atomizes the visible classes of one release. `classes` is the release's own content, keyed by
  // JVM internal name; `classpath` locates the dependencies whose members present through it.
  def atomize(classes: Map[Text, ClassSurface], classpath: List[Text]): Outcome =
    val index = ClasspathIndex(classes, classpath)
    val atoms = scala.collection.mutable.ListBuffer[Atom]()
    val unresolved = scala.collection.mutable.LinkedHashSet[Text]()

    index.own.stdlib.foreach: name =>
      index(name).let: surface =>
        // Synthetic classes — lambda holders, `package-info`, anonymous helpers — are not
        // resolvable by name from any consumer's source, so they carry no interface of their own.
        if surface.visible && !surface.synthetic then
          val (_, missing) = index.closure(name)
          unresolved ++= missing.stdlib

          val members = presented(surface, index)
          atoms += classAtom(surface, members)

          members.stdlib.foreach: (member, _) =>
            atoms += memberAtom(surface.name, member)

    Outcome(List.from(atoms.toList), List.from(unresolved.toList))
