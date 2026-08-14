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
package xenophile

import scala.collection.immutable.List as SList
import scala.collection.immutable.Map as SMap

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import reliquary.*
import rudiments.*
import vacuous.*

// The atomization rules of `webidl/1` (`webidl.md`).
//
// The folding decisions are the platform's own compatibility rules, and they invert `dts/1`'s
// where the IDL declares what TypeScript cannot see: `partial interface` and `includes` exist
// because the platform adds members to existing interfaces continuously, and nothing outside
// the browser implements `Element` — so interface members are standalone atoms whose addition
// is minor, and only *required* dictionary members fold (adding one breaks every caller
// constructing the dictionary), while optional members stand alone.
object WebIdlAtomizer:
  val id: Text = t"webidl/1"

  private def malformed(detail: Text): Discipline.Error =
    import errorDiagnostics.emptyDiagnostics
    Discipline.Error(id, Discipline.Error.Reason.Malformed(detail))

  // --- canonical binary encoding -------------------------------------------------------------

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

  private def flag(out: java.io.ByteArrayOutputStream, value: Boolean): Unit =
    out.write(if value then 1 else 0)

  private def hash(encode: java.io.ByteArrayOutputStream => Unit): Data =
    val out = java.io.ByteArrayOutputStream()
    encode(out)
    Lira.Hash(Lira.Hash.Domain.Atom(id), Array.unsafeFrozen(out.toByteArray.nn))

  // Union members sort by their encoded bytes (`webidl.md` §7): `(A or B)` is `(B or A)`.
  private def encode(out: java.io.ByteArrayOutputStream, typed: Foreign.Type): Unit =
    typed match
      case Foreign.Type.Named(name) =>
        tag(out, 'N')
        utf8(out, name)

      case Foreign.Type.Union(members) =>
        tag(out, '|')

        val encodings = members.stdlib.map: member =>
          val buffer = java.io.ByteArrayOutputStream()
          encode(buffer, member)
          val bytes: scala.Array[Byte] = buffer.toByteArray().nn
          scala.Vector.tabulate(bytes.length) { index => bytes(index) & 0xff }

        uvarint(out, encodings.length.toLong)

        encodings.sortWith { (left, right) => compare(left, right) < 0 }.foreach: encoding =>
          uvarint(out, encoding.length.toLong)
          encoding.foreach { byte => out.write(byte) }

      case Foreign.Type.Applied(constructor, arguments) =>
        tag(out, 'A')
        utf8(out, constructor)
        uvarint(out, arguments.stdlib.length.toLong)
        arguments.stdlib.foreach { argument => encode(out, argument) }

  private def compare(left: scala.Vector[Int], right: scala.Vector[Int]): Int =
    val shared = left.length.min(right.length)
    var index = 0
    var result = 0

    while result == 0 && index < shared do
      result = left(index) - right(index)
      index += 1

    if result != 0 then result else left.length - right.length

  // Argument *names* are not folded — WebIDL calls are positional — while optionality,
  // variadicity and a default's existence decide which calls are legal, so all three are.
  private def arguments(out: java.io.ByteArrayOutputStream, list: List[WebIdl.Argument]): Unit =
    uvarint(out, list.stdlib.length.toLong)

    list.stdlib.foreach: argument =>
      flag(out, argument.optional)
      flag(out, argument.variadic)
      flag(out, argument.default)
      encode(out, argument.typed)

  private def encodeMember
    ( out: java.io.ByteArrayOutputStream, container: Text, member: WebIdl.Member )
  :   Unit =

    // The container's key folds into the value, not merely the atom's key: the snapshot hashes
    // the set of *distinct* value hashes (LIRA §12.1), so two identically-shaped members of
    // different interfaces must not collapse into one term.
    tag(out, 'M')
    utf8(out, container)
    utf8(out, member.selector)
    uvarint(out, member.kind.ordinal.toLong)
    flag(out, member.readonly)
    flag(out, member.static)
    utf8(out, member.special.or(t""))
    encode(out, member.typed)
    arguments(out, member.arguments)

  // --- resolution -----------------------------------------------------------------------------

  // `webidl.md` §4: partials merge into their targets and mixins fold into their includers
  // before atomization, exactly as the platform presents them. The merge is a set union keyed
  // by member selector, independent of definition order across files.
  private def resolved(definitions: List[WebIdl.Definition])
  :   SList[WebIdl.Definition] raises Discipline.Error =

    import WebIdl.Definition.*

    val all = definitions.stdlib

    val mixinMembers: SMap[Text, SList[WebIdl.Member]] =
      all.collect { case interface: Interface if interface.mixin => interface }
      . groupBy(_.name)
      . map { (name, group) => name -> group.flatMap(_.members.stdlib) }
      . toMap

    val includes: SMap[Text, SList[Text]] =
      all.collect { case Includes(target, mixin) => (target, mixin) }
      . groupBy(_(0))
      . map { (target, group) => target -> group.map(_(1)) }
      . toMap

    def mixedIn(name: Text): SList[WebIdl.Member] =
      includes.getOrElse(name, SList()).flatMap: mixin =>
        mixinMembers.getOrElse(mixin,
            abort(malformed(t"the mixin $mixin is included but never defined")))

    val names = scala.collection.mutable.HashSet[Text]()

    val complete = all.flatMap:
      case interface: Interface if interface.mixin || interface.partial => SList()
      case Includes(_, _)                                              => SList()
      case Dictionary(_, _, _, true)                                   => SList()
      case Namespace(_, _, _, true)                                    => SList()

      case interface: Interface =>
        val partials = all.collect:
          case partial: Interface if partial.partial && partial.name == interface.name => partial

        val members = interface.members.stdlib
          ++ partials.flatMap(_.members.stdlib)
          ++ mixedIn(interface.name)

        val intrinsics = interface.intrinsics.stdlib ++ partials.flatMap(_.intrinsics.stdlib)

        SList(interface.copy(members = List.from(members),
            intrinsics = List.from(intrinsics), partial = false))

      case dictionary: Dictionary =>
        val partials = all.collect:
          case partial: Dictionary if partial.partial && partial.name == dictionary.name =>
            partial

        val fields = dictionary.fields.stdlib ++ partials.flatMap(_.fields.stdlib)
        SList(dictionary.copy(fields = List.from(fields), partial = false))

      case namespace: Namespace =>
        val partials = all.collect:
          case partial: Namespace if partial.partial && partial.name == namespace.name => partial

        val members = namespace.members.stdlib ++ partials.flatMap(_.members.stdlib)
        SList(namespace.copy(members = List.from(members), partial = false))

      case other => SList(other)

    complete.foreach: definition =>
      if !names.add(definition.named)
      then abort(malformed(t"the definition ${definition.named} appears twice"))

    complete

  // --- atoms ----------------------------------------------------------------------------------

  // The atom key of a definition: its name, with the sorted `[Exposed]` scopes appended where
  // any are declared (`webidl.md` §5) — `Window` and `WorkerGlobalScope` genuinely offer
  // different surfaces.
  private def keyOf(name: Text, exposed: List[Text]): Text =
    if exposed.stdlib.isEmpty then name
    else Text(s"$name[${exposed.stdlib.map(_.s).sorted.mkString(",")}]")

  def atomize(definitions: List[WebIdl.Definition]): List[Atom] raises Discipline.Error =
    import WebIdl.Definition.*

    val atoms = scala.collection.mutable.ListBuffer[Atom]()

    resolved(definitions).foreach:
      case Interface(name, parent, exposed, members, intrinsics, _, _, callback) =>
        val key = keyOf(name, exposed)

        members.stdlib.foreach: member =>
          atoms += Atom(t"$key#${member.selector}", Atom.Class.Rigid,
              hash(encodeMember(_, key, member)))

        // Member lists do *not* fold into the interface's own atom — the deliberate inversion
        // of `dts/1`, licensed by the declared usage direction (`webidl.md` §6). Intrinsics
        // (`iterable<…>` and kin) are features of the type and do fold, sorted by keyword.
        atoms += Atom(key, Atom.Class.Rigid, hash: out =>
          tag(out, 'I')
          utf8(out, key)
          utf8(out, parent.or(t""))
          flag(out, callback)

          val ordered = intrinsics.stdlib.sortBy(_(0).s)
          uvarint(out, ordered.length.toLong)

          ordered.foreach: (keyword, args) =>
            utf8(out, keyword)
            uvarint(out, args.stdlib.length.toLong)
            args.stdlib.foreach { arg => encode(out, arg) })

      case Dictionary(name, parent, fields, _) =>
        val required = fields.stdlib.filter(_.required).sortBy(_.name.s)

        fields.stdlib.filter(!_.required).foreach: field =>
          atoms += Atom(t"$name#${field.name}", Atom.Class.Rigid, hash: out =>
            tag(out, 'F')
            utf8(out, name)
            utf8(out, field.name)
            flag(out, field.default)
            encode(out, field.typed))

        atoms += Atom(name, Atom.Class.Rigid, hash: out =>
          tag(out, 'D')
          utf8(out, name)
          utf8(out, parent.or(t""))
          uvarint(out, required.length.toLong)

          required.foreach: field =>
            utf8(out, field.name)
            flag(out, field.default)
            encode(out, field.typed))

      case Namespace(name, exposed, members, _) =>
        val key = keyOf(name, exposed)

        members.stdlib.foreach: member =>
          atoms += Atom(t"$key#${member.selector}", Atom.Class.Rigid,
              hash(encodeMember(_, key, member)))

        atoms += Atom(key, Atom.Class.Rigid, hash: out =>
          tag(out, 'S')
          utf8(out, key))

      case Enumeration(name, values) =>
        values.stdlib.foreach: value =>
          atoms += Atom(t"$name#$value", Atom.Class.Rigid, hash: out =>
            tag(out, 'V')
            utf8(out, name)
            utf8(out, value))

        atoms += Atom(name, Atom.Class.Rigid, hash: out =>
          tag(out, 'E')
          utf8(out, name))

      case Alias(name, typed) =>
        atoms += Atom(name, Atom.Class.Rigid, hash: out =>
          tag(out, 'T')
          utf8(out, name)
          encode(out, typed))

      case CallbackFunction(name, result, args) =>
        atoms += Atom(name, Atom.Class.Rigid, hash: out =>
          tag(out, 'C')
          utf8(out, name)
          encode(out, result)
          arguments(out, args))

      case Includes(_, _) => ()

    List.from(atoms.toList)
