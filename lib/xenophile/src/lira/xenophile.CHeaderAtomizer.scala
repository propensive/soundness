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
import vacuous.*

// The canonical encoding writes an explicit element count before every folded sequence
// (`cheader.md` §6), so `List#size` is genuinely required here; the lists are declaration-sized.
import denominative.dysasymptotics.linearSize
import denominative.size

// The atomization rules of `cheader/1` (`cheader.md`): one rigid atom per declaration, keyed by
// bare name — C has one flat namespace — with functions standalone and structs, unions, enums
// and typedefs folding their contents.
object CHeaderAtomizer:
  val id: Text = t"cheader/1"

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

  private def encode(out: Scribe[Byte], typed: Foreign.Type): Unit =
    typed match
      case Foreign.Type.Named(name) =>
        tag(out, 'N')
        utf8(out, name)

      case Foreign.Type.Applied(constructor, arguments) =>
        tag(out, 'A')
        utf8(out, constructor)
        uvarint(out, arguments.size.toLong)
        arguments.each: argument => encode(out, argument)

      case Foreign.Type.Union(_) => () // not producible by `CHeader.Parser`

  def atomize(declarations: List[CHeader.Declaration]): List[Atom] =

      declarations.map:
        case CHeader.Declaration.Function(name, result, parameters, variadic) =>
          Atom(name, Atom.Class.Rigid, hash: out =>
            tag(out, 'f')
            utf8(out, name)
            flag(out, variadic)
            uvarint(out, parameters.size.toLong)
            parameters.each: parameter => encode(out, parameter)
            encode(out, result))

        case CHeader.Declaration.Alias(name, target) =>
          Atom(name, Atom.Class.Rigid, hash: out =>
            tag(out, 't')
            utf8(out, name)
            encode(out, target))

        // Fields fold in declaration order — layout is positional — and an opaque tag folds an
        // opacity marker instead, so completing it later is a value change (`cheader.md` §6).
        case CHeader.Declaration.Structure(name, union, fields, opaque) =>
          Atom(name, Atom.Class.Rigid, hash: out =>
            tag(out, if union then 'u' else 's')
            utf8(out, name)
            flag(out, opaque)
            uvarint(out, fields.size.toLong)

            fields.each: (field, typed) =>
              utf8(out, field)
              encode(out, typed))

        case CHeader.Declaration.Enumeration(name, cases) =>
          Atom(name, Atom.Class.Rigid, hash: out =>
            tag(out, 'e')
            utf8(out, name)
            uvarint(out, cases.size.toLong)

            cases.each: (label, value) =>
              utf8(out, label)
              utf8(out, Text(value.toString)))
