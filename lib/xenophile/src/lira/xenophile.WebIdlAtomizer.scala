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

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import reliquary.*
import rudiments.*
import symbolism.*
import vacuous.*

// The canonical encoding writes an explicit element count before every folded sequence
// (`webidl.md` §7), so `List#size` is genuinely required here; the lists are declaration-sized.
import denominative.dysasymptotics.linearSize
import denominative.{nil, size}
import rudiments.sortingAlgorithms.timsort

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

  private def uvarint(out: Scribe[Byte], value0: Long): Unit =
    var value = value0

    while value >= 0x80L do
      out.append(((value & 0x7f) | 0x80).toByte)
      value >>>= 7

    out.append(value.toByte)

  private def utf8(out: Scribe[Byte], text: Text): Unit =
    val bytes = Array.unsafeFrozen(text.s.getBytes("UTF-8").nn)
    uvarint(out, bytes.length.toLong)
    out.append(bytes)

  private def tag(out: Scribe[Byte], char: Char): Unit = out.append(char.toByte)

  private def flag(out: Scribe[Byte], value: Boolean): Unit =
    if value then out.append(1) else out.append(0)

  private def hash(encode: Scribe[Byte] => Unit): Data =
    Lira.Hash(Lira.Hash.Domain.Atom(id), Array.collect[Byte]()(encode))

  // Union members sort by their encoded bytes (`webidl.md` §7): `(A or B)` is `(B or A)`.
  private def encode(out: Scribe[Byte], typed: Foreign.Type): Unit =
    typed match
      case Foreign.Type.Named(name) =>
        tag(out, 'N')
        utf8(out, name)

      case Foreign.Type.Union(members) =>
        tag(out, '|')

        // The encoded members stay in a `stdlib` view: an opaque `List` of `scala.Vector[Int]`
        // fails capture checking here, because the vector's `prefix1` array carries a root
        // capability that cannot flow into the traversal evidence's empty capture set.
        val encodings = members.stdlib.map: member =>
          val collected = Array.collect[Byte](): buffer =>
            encode(buffer, member)

          val bytes: scala.Array[Byte] = Array.unsafeJvm(collected)
          scala.Vector.tabulate(bytes.length) { index => bytes(index) & 0xff }

        uvarint(out, encodings.length.toLong)

        encodings.sortWith { (left, right) => compare(left, right) < 0 }.foreach: encoding =>
          uvarint(out, encoding.length.toLong)
          encoding.foreach { byte => out.append(byte.toByte) }

      case Foreign.Type.Applied(constructor, arguments) =>
        tag(out, 'A')
        utf8(out, constructor)
        uvarint(out, arguments.size.toLong)
        arguments.each: argument => encode(out, argument)

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
  private def arguments(out: Scribe[Byte], list: List[WebIdl.Argument]): Unit =
    uvarint(out, list.size.toLong)

    list.each: argument =>
      flag(out, argument.optional)
      flag(out, argument.variadic)
      flag(out, argument.default)
      encode(out, argument.typed)

  private def encodeMember
    ( out: Scribe[Byte], container: Text, member: WebIdl.Member )
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
  :   List[WebIdl.Definition] raises Discipline.Error =

    import WebIdl.Definition.*

    // `group` rebuilds each group in the source's own shape, and `Map#map` maps values with the
    // keys preserved, so these are the `groupBy`/`mapValues` pair in one step each.
    val mixinMembers: Map[Text, List[WebIdl.Member]] =
      definitions.sweep { case interface: Interface if interface.mixin => interface }
      . group(_.name)
      . map { group => group.bind(_.members) }

    val includes: Map[Text, List[Text]] =
      definitions.sweep { case Includes(target, mixin) => (target, mixin) }
      . group(_(0))
      . map { group => group.map(_(1)) }

    def mixedIn(name: Text): List[WebIdl.Member] =
      val included: List[Text] = includes.at(name).or(Nil)

      included.bind: mixin0 =>
        // Both the element and the `Optional` lookup are bound to typed locals before they are
        // read: `bind` types its lambda parameter as the traversal's (still-uninstantiated)
        // `Operand`, which trips the compiler's `wildApprox` assertion inside the `t"…"` macro.
        val mixin: Text = mixin0
        val members: Optional[List[WebIdl.Member]] = mixinMembers.at(mixin)
        members.or(abort(malformed(t"the mixin $mixin is included but never defined")))

    val names = scala.collection.mutable.HashSet[Text]()

    val complete: List[WebIdl.Definition] = definitions.bind:
      case interface: Interface if interface.mixin || interface.partial => Nil
      case Includes(_, _)                                              => Nil
      case Dictionary(_, _, _, true)                                   => Nil
      case Namespace(_, _, _, true)                                    => Nil

      case interface: Interface =>
        val partials: List[Interface] = definitions.sweep:
          case partial: Interface if partial.partial && partial.name == interface.name => partial

        val members =
          interface.members + partials.bind(_.members) + mixedIn(interface.name)

        val intrinsics = interface.intrinsics + partials.bind(_.intrinsics)

        List(interface.copy(members = members, intrinsics = intrinsics, partial = false))

      case dictionary: Dictionary =>
        val partials: List[Dictionary] = definitions.sweep:
          case partial: Dictionary if partial.partial && partial.name == dictionary.name =>
            partial

        val fields = dictionary.fields + partials.bind(_.fields)
        List(dictionary.copy(fields = fields, partial = false))

      case namespace: Namespace =>
        val partials: List[Namespace] = definitions.sweep:
          case partial: Namespace if partial.partial && partial.name == namespace.name => partial

        val members = namespace.members + partials.bind(_.members)
        List(namespace.copy(members = members, partial = false))

      case other => List(other)

    complete.each: definition0 =>
      // The element is bound to a typed local before it is read (`wildApprox`; see `mixedIn`).
      val definition: WebIdl.Definition = definition0
      val named: Text = definition.named
      if !names.add(named) then abort(malformed(t"the definition $named appears twice"))

    complete

  // --- atoms ----------------------------------------------------------------------------------

  // The atom key of a definition: its name, with the sorted `[Exposed]` scopes appended where
  // any are declared (`webidl.md` §5) — `Window` and `WorkerGlobalScope` genuinely offer
  // different surfaces.
  private def keyOf(name: Text, exposed: List[Text]): Text =
    if exposed.nil then name else
      // Ordered by the underlying `String`, as before: this is a canonical key, not a display
      // order, so it must not depend on a locale's collation.
      val scopes: Text = exposed.order(_.s).join(t",")
      t"$name[$scopes]"

  def atomize(definitions: List[WebIdl.Definition]): List[Atom] raises Discipline.Error =
    import WebIdl.Definition.*

    val atoms = scala.collection.mutable.ListBuffer[Atom]()

    resolved(definitions).each:
      case Interface(name, parent, exposed, members, intrinsics, _, _, callback) =>
        val key = keyOf(name, exposed)

        members.each: member =>
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

          val ordered: List[(Text, List[Foreign.Type])] = intrinsics.order(_(0).s)
          uvarint(out, ordered.size.toLong)

          ordered.each: (keyword, args) =>
            utf8(out, keyword)
            uvarint(out, args.size.toLong)
            args.each { arg => encode(out, arg) })

      case Dictionary(name, parent, fields, _) =>
        val required: List[WebIdl.Field] = fields.filter(_.required).order(_.name.s)

        fields.filter(!_.required).each: field =>
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
          uvarint(out, required.size.toLong)

          required.each: field =>
            utf8(out, field.name)
            flag(out, field.default)
            encode(out, field.typed))

      case Namespace(name, exposed, members, _) =>
        val key = keyOf(name, exposed)

        members.each: member =>
          atoms += Atom(t"$key#${member.selector}", Atom.Class.Rigid,
              hash(encodeMember(_, key, member)))

        atoms += Atom(key, Atom.Class.Rigid, hash: out =>
          tag(out, 'S')
          utf8(out, key))

      case Enumeration(name, values) =>
        values.each: value =>
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

    atoms.toList.to(List)
