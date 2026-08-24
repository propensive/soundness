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

import scala.collection.immutable.Map as SMap

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import reliquary.*
import rudiments.*
import vacuous.*

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

  private val primitives: Set[Text] =
    Set(t"bool", t"u8", t"u16", t"u32", t"u64", t"s8", t"s16", t"s32", t"s64", t"f32", t"f64",
        t"char", t"string", t"_")

  private val constructors: Set[Text] =
    Set(t"list", t"option", t"result", t"tuple", t"own", t"borrow")

  // Every named type reference is encoded fully qualified (`wit.md` §7): local names under the
  // declaring interface's id, `use`-imported names under the interface they came from. A name
  // that resolves to neither is a hard error, never a guess.
  private def encode
    ( out:   java.io.ByteArrayOutputStream,
      typed: Foreign.Type,
      scope: SMap[Text, Text] )
  :   Unit raises Discipline.Error =

    typed match
      case Foreign.Type.Named(name) =>
        tag(out, 'N')

        if primitives.stdlib.contains(name) then utf8(out, name)
        else utf8(out, scope.getOrElse(name, abort(unresolved(name))))

      case Foreign.Type.Applied(constructor, arguments) =>
        if !constructors.stdlib.contains(constructor)
        then abort(malformed(t"$constructor is not a type constructor"))

        tag(out, 'A')
        utf8(out, constructor)
        uvarint(out, arguments.stdlib.length.toLong)
        arguments.stdlib.foreach { argument => encode(out, argument, scope) }

      case Foreign.Type.Union(_) =>
        abort(malformed(t"a union type is not WIT"))

  private def function
    ( container: Text,
      resource:  Optional[Text],
      fn:        Wit.Function,
      scope:     SMap[Text, Text] )
  :   Atom raises Discipline.Error =

    val key = resource.let { res => t"$container#$res.${fn.name}" }.or(t"$container#${fn.name}")

    Atom(key, Atom.Class.Rigid, hash: out =>
      tag(out, 'f')
      utf8(out, container)
      utf8(out, resource.or(t""))
      utf8(out, fn.name)
      flag(out, fn.static)
      flag(out, fn.constructor)
      uvarint(out, fn.parameters.stdlib.length.toLong)

      // WIT calls are by named parameter, so names fold beside types (`wit.md` §6).
      fn.parameters.stdlib.foreach: (name, typed) =>
        utf8(out, name)
        encode(out, typed, scope)

      fn.result.lay(tag(out, '0')) { typed => tag(out, '1'); encode(out, typed, scope) })

  // --- atomization ----------------------------------------------------------------------------

  def atomize(documents: List[Wit.Document]): List[Atom] raises Discipline.Error =
    val atoms = scala.collection.mutable.ListBuffer[Atom]()

    documents.stdlib.foreach: document =>
      val pkg = document.packageName.or(abort(malformed(t"a document declares no package")))
      val suffix = document.version.let { version => t"@$version" }.or(t"")

      def qualify(interface: Text): Text =
        if interface.s.contains(":") then interface else t"$pkg/$interface$suffix"

      document.interfaces.stdlib.foreach: interface =>
        val ifaceId = t"$pkg/${interface.name}$suffix"

        // The qualification scope: local item names under this interface's id, `use`-imported
        // names under the id they came from. `use` clauses are otherwise transparent
        // (`wit.md` §6) — a re-export or rename carries no semantic content.
        val scope: SMap[Text, Text] =
          val local = interface.items.stdlib.collect:
            case item if !item.isInstanceOf[Wit.Item.Use] => item.named -> t"$ifaceId#${item.named}"

          val imported = interface.items.stdlib.collect:
            case Wit.Item.Use(from, names) =>
              names.stdlib.map: (original, alias) =>
                alias -> t"${qualify(from)}#$original"

          . flatten

          (local ++ imported).toMap

        val typeKeys = scala.collection.mutable.ListBuffer[Text]()

        interface.items.stdlib.foreach:
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
              uvarint(out, fields.stdlib.length.toLong)

              fields.stdlib.foreach: (field, typed) =>
                utf8(out, field)
                encode(out, typed, scope))

          case Wit.Item.Variant(name, cases) =>
            typeKeys += t"$ifaceId#$name"

            atoms += Atom(t"$ifaceId#$name", Atom.Class.Rigid, hash: out =>
              tag(out, 'v')
              utf8(out, t"$ifaceId#$name")
              uvarint(out, cases.stdlib.length.toLong)

              cases.stdlib.foreach: (label, payload) =>
                utf8(out, label)
                payload.lay(tag(out, '0')) { typed => tag(out, '1'); encode(out, typed, scope) })

          case Wit.Item.Enumeration(name, cases) =>
            typeKeys += t"$ifaceId#$name"

            atoms += Atom(t"$ifaceId#$name", Atom.Class.Rigid, hash: out =>
              tag(out, 'e')
              utf8(out, t"$ifaceId#$name")
              uvarint(out, cases.stdlib.length.toLong)
              cases.stdlib.foreach { label => utf8(out, label) })

          case Wit.Item.Flags(name, names) =>
            typeKeys += t"$ifaceId#$name"

            atoms += Atom(t"$ifaceId#$name", Atom.Class.Rigid, hash: out =>
              tag(out, 'g')
              utf8(out, t"$ifaceId#$name")
              uvarint(out, names.stdlib.length.toLong)
              names.stdlib.foreach { label => utf8(out, label) })

          case Wit.Item.Resource(name, methods) =>
            typeKeys += t"$ifaceId#$name"

            atoms += Atom(t"$ifaceId#$name", Atom.Class.Rigid, hash: out =>
              tag(out, 'R')
              utf8(out, t"$ifaceId#$name"))

            methods.stdlib.foreach: method =>
              atoms += function(ifaceId, name, method, scope)

        // The interface's own atom folds the sorted key list of its *type* declarations, not
        // its functions: adding a function is additive (`wit.md` §6).
        atoms += Atom(ifaceId, Atom.Class.Rigid, hash: out =>
          tag(out, 'I')
          utf8(out, ifaceId)
          val sorted = typeKeys.toList.sortBy(_.s)
          uvarint(out, sorted.length.toLong)
          sorted.foreach { key => utf8(out, key) })

      document.worlds.stdlib.foreach: world =>
        val worldId = t"$pkg/${world.name}$suffix"
        val imports = world.imports.stdlib.map(qualify(_))

        // Inline items follow the same polarity as referenced ones: an inline function import
        // is a capability the host supplies (standalone, its signature folded into the value),
        // and an inline export joins the folded obligation list by its bare name.
        val exports =
          (world.exports.stdlib.map(qualify(_))
            ++ world.inlineExports.stdlib.map { (name, _) => name })
          . sortBy(_.s)

        imports.foreach: imported =>
          atoms += Atom(t"$worldId#import $imported", Atom.Class.Rigid, hash: out =>
            tag(out, 'i')
            utf8(out, worldId)
            utf8(out, imported))

        world.inlineImports.stdlib.foreach: (name, function) =>
          atoms += Atom(t"$worldId#import $name", Atom.Class.Rigid, hash: out =>
            tag(out, 'i')
            utf8(out, worldId)
            utf8(out, name)

            function.let: fn =>
              uvarint(out, fn.parameters.stdlib.length.toLong)

              fn.parameters.stdlib.foreach: (parameter, typed) =>
                utf8(out, parameter)
                encode(out, typed, SMap())

              fn.result.lay(tag(out, '0')) { typed => tag(out, '1'); encode(out, typed, SMap()) })

        atoms += Atom(worldId, Atom.Class.Rigid, hash: out =>
          tag(out, 'W')
          utf8(out, worldId)
          uvarint(out, exports.length.toLong)
          exports.foreach { exported => utf8(out, exported) })

    atoms.toList.to(List)
