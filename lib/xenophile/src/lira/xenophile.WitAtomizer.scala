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
// (`wit.md` §6), so `List#size` is genuinely required here; the lists are declaration-sized.
import denominative.dysasymptotics.linearSize
import denominative.size
import rudiments.sortingAlgorithms.timsort

// The atomization rules of `wit/1` (`wit.md`).
//
// Functions are standalone atoms — adding one to an interface is additive, and nothing
// implements a host interface from outside — while records, variants, enums and flags fold
// their contents in declaration order, since the canonical ABI is positional and gated
// evolution is not modelled in version 1. A world's imports are standalone (a host gaining a
// capability is a minor) and its exports fold (an export is an obligation on every component
// targeting the world).
object WitAtomizer:
  val id: Text = t"wit/1"

  private def malformed(detail: Text): Discipline.Error =
    import errorDiagnostics.emptyDiagnostics
    Discipline.Error(id, Discipline.Error.Reason.Malformed(detail))

  private def unresolved(name: Text): Discipline.Error =
    import errorDiagnostics.emptyDiagnostics
    Discipline.Error(id, Discipline.Error.Reason.Unresolved(name))

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

  private val primitives: Set[Text] =
    Set(t"bool", t"u8", t"u16", t"u32", t"u64", t"s8", t"s16", t"s32", t"s64", t"f32", t"f64",
        t"char", t"string", t"_")

  private val constructors: Set[Text] =
    Set(t"list", t"option", t"result", t"tuple", t"own", t"borrow")

  // Every named type reference is encoded fully qualified (`wit.md` §7): local names under the
  // declaring interface's id, `use`-imported names under the interface they came from. A name
  // that resolves to neither is a hard error, never a guess.
  private def encode
    ( out:   Scribe[Byte],
      typed: Foreign.Type,
      scope: Map[Text, Text] )
  :   Unit raises Discipline.Error =

    typed match
      case Foreign.Type.Named(name) =>
        tag(out, 'N')

        if primitives.has(name) then utf8(out, name)
        else utf8(out, scope.at(name).or(abort(unresolved(name))))

      case Foreign.Type.Applied(constructor, arguments) =>
        if !constructors.has(constructor)
        then abort(malformed(t"$constructor is not a type constructor"))

        tag(out, 'A')
        utf8(out, constructor)
        uvarint(out, arguments.size.toLong)
        arguments.each: argument => encode(out, argument, scope)

      case Foreign.Type.Union(_) =>
        abort(malformed(t"a union type is not WIT"))

  private def function
    ( container: Text,
      resource:  Optional[Text],
      fn:        Wit.Function,
      scope:     Map[Text, Text] )
  :   Atom raises Discipline.Error =

    val key = resource.let { res => t"$container#$res.${fn.name}" }.or(t"$container#${fn.name}")

    Atom(key, Atom.Class.Rigid, hash: out =>
      tag(out, 'f')
      utf8(out, container)
      utf8(out, resource.or(t""))
      utf8(out, fn.name)
      flag(out, fn.static)
      flag(out, fn.constructor)
      uvarint(out, fn.parameters.size.toLong)

      // WIT calls are by named parameter, so names fold beside types (`wit.md` §6).
      fn.parameters.each: (name, typed) =>
        utf8(out, name)
        encode(out, typed, scope)

      // The `Optional` result is bound to a typed local before it is read (`wildApprox`).
      val result: Optional[Foreign.Type] = fn.result
      result.lay(tag(out, '0')) { typed => tag(out, '1'); encode(out, typed, scope) })

  // --- atomization ----------------------------------------------------------------------------

  def atomize(documents: List[Wit.Document]): List[Atom] raises Discipline.Error =
    val atoms = scala.collection.mutable.ListBuffer[Atom]()

    documents.each: document =>
      // Both `Optional` fields are bound to typed locals before they are read (`wildApprox`).
      val packageName: Optional[Text] = document.packageName
      val version: Optional[Text] = document.version
      val pkg = packageName.or(abort(malformed(t"a document declares no package")))
      val suffix = version.let { version => t"@$version" }.or(t"")

      def qualify(interface: Text): Text =
        if interface.s.contains(":") then interface else t"$pkg/$interface$suffix"

      document.interfaces.each: interface =>
        val ifaceId = t"$pkg/${interface.name}$suffix"

        // The qualification scope: local item names under this interface's id, `use`-imported
        // names under the id they came from. `use` clauses are otherwise transparent
        // (`wit.md` §6) — a re-export or rename carries no semantic content.
        val scope: Map[Text, Text] =
          val local: List[(Text, Text)] = interface.items.sweep:
            case item if !item.isInstanceOf[Wit.Item.Use] => item.named -> t"$ifaceId#${item.named}"

          val imported: List[(Text, Text)] = interface.items.bind:
            case Wit.Item.Use(from, names) =>
              names.map: (original, alias) => alias -> t"${qualify(from)}#$original"

            case _ =>
              Nil

          (local + imported).to[Map]

        val typeKeys = scala.collection.mutable.ListBuffer[Text]()

        interface.items.each:
          case Wit.Item.Use(_, _) => ()

          case Wit.Item.Function(fn) =>
            atoms += function(ifaceId, Unset, fn, scope)

          case Wit.Item.Alias(name, target) =>
            typeKeys += t"$ifaceId#$name"

            atoms += Atom(t"$ifaceId#$name", Atom.Class.Rigid, hash: out =>
              tag(out, 't')
              utf8(out, t"$ifaceId#$name")
              encode(out, target, scope))

          case Wit.Item.Record(name, fields) =>
            typeKeys += t"$ifaceId#$name"

            // Fields fold in declaration order: the canonical ABI is positional (`wit.md` §6).
            atoms += Atom(t"$ifaceId#$name", Atom.Class.Rigid, hash: out =>
              tag(out, 'r')
              utf8(out, t"$ifaceId#$name")
              uvarint(out, fields.size.toLong)

              fields.each: (field, typed) =>
                utf8(out, field)
                encode(out, typed, scope))

          case Wit.Item.Variant(name, cases) =>
            typeKeys += t"$ifaceId#$name"

            atoms += Atom(t"$ifaceId#$name", Atom.Class.Rigid, hash: out =>
              tag(out, 'v')
              utf8(out, t"$ifaceId#$name")
              uvarint(out, cases.size.toLong)

              cases.each: (label, payload0) =>
                // The `Optional` payload is bound to a typed local before it is read
                // (`wildApprox`).
                val payload: Optional[Foreign.Type] = payload0
                utf8(out, label)
                payload.lay(tag(out, '0')) { typed => tag(out, '1'); encode(out, typed, scope) })

          case Wit.Item.Enumeration(name, cases) =>
            typeKeys += t"$ifaceId#$name"

            atoms += Atom(t"$ifaceId#$name", Atom.Class.Rigid, hash: out =>
              tag(out, 'e')
              utf8(out, t"$ifaceId#$name")
              uvarint(out, cases.size.toLong)
              cases.each { label => utf8(out, label) })

          case Wit.Item.Flags(name, names) =>
            typeKeys += t"$ifaceId#$name"

            atoms += Atom(t"$ifaceId#$name", Atom.Class.Rigid, hash: out =>
              tag(out, 'g')
              utf8(out, t"$ifaceId#$name")
              uvarint(out, names.size.toLong)
              names.each { label => utf8(out, label) })

          case Wit.Item.Resource(name, methods) =>
            typeKeys += t"$ifaceId#$name"

            atoms += Atom(t"$ifaceId#$name", Atom.Class.Rigid, hash: out =>
              tag(out, 'R')
              utf8(out, t"$ifaceId#$name"))

            methods.each: method => atoms += function(ifaceId, name, method, scope)

        // The interface's own atom folds the sorted key list of its *type* declarations, not
        // its functions: adding a function is additive (`wit.md` §6).
        atoms += Atom(ifaceId, Atom.Class.Rigid, hash: out =>
          tag(out, 'I')
          utf8(out, ifaceId)
          val sorted: List[Text] = typeKeys.to(List).order(_.s)
          uvarint(out, sorted.size.toLong)
          sorted.each { key => utf8(out, key) })

      document.worlds.each: world =>
        val worldId = t"$pkg/${world.name}$suffix"
        val imports: List[Text] = world.imports.map(qualify(_))

        // Inline items follow the same polarity as referenced ones: an inline function import
        // is a capability the host supplies (standalone, its signature folded into the value),
        // and an inline export joins the folded obligation list by its bare name.
        val exports: List[Text] =
          (world.exports.map(qualify(_)) + world.inlineExports.map { (name, _) => name })
          . order(_.s)

        imports.each: imported =>
          atoms += Atom(t"$worldId#import $imported", Atom.Class.Rigid, hash: out =>
            tag(out, 'i')
            utf8(out, worldId)
            utf8(out, imported))

        world.inlineImports.each: (name, function0) =>
          // The `Optional` inline function is bound to a typed local before it is read
          // (`wildApprox`).
          val function: Optional[Wit.Function] = function0

          atoms += Atom(t"$worldId#import $name", Atom.Class.Rigid, hash: out =>
            tag(out, 'i')
            utf8(out, worldId)
            utf8(out, name)

            function.let: fn =>
              uvarint(out, fn.parameters.size.toLong)

              fn.parameters.each: (parameter, typed) =>
                utf8(out, parameter)
                encode(out, typed, Map())

              // Likewise for the function's `Optional` result.
              val result: Optional[Foreign.Type] = fn.result
              result.lay(tag(out, '0')) { typed => tag(out, '1'); encode(out, typed, Map()) })

        atoms += Atom(worldId, Atom.Class.Rigid, hash: out =>
          tag(out, 'W')
          utf8(out, worldId)
          uvarint(out, exports.size.toLong)
          exports.each { exported => utf8(out, exported) })

    atoms.toList.to(List)
