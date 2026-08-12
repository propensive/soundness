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

// The atomization rules of `cheader/1` (`cheader.md`): one rigid atom per declaration, keyed by
// bare name — C has one flat namespace — with functions standalone and structs, unions, enums
// and typedefs folding their contents.
object CHeaderAtomizer:
  val id: Text = t"cheader/1"

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

  private def encode(out: java.io.ByteArrayOutputStream, typed: Foreign.Type): Unit =
    typed match
      case Foreign.Type.Named(name) =>
        tag(out, 'N')
        utf8(out, name)

      case Foreign.Type.Applied(constructor, arguments) =>
        tag(out, 'A')
        utf8(out, constructor)
        uvarint(out, arguments.stdlib.length.toLong)
        arguments.stdlib.foreach { argument => encode(out, argument) }

      case Foreign.Type.Union(_) => () // not producible by `CHeaderParser`

  def atomize(declarations: List[CDeclaration]): List[Atom] =
    List.from:
      declarations.stdlib.map:
        case CDeclaration.Function(name, result, parameters, variadic) =>
          Atom(name, Atom.Class.Rigid, hash: out =>
            tag(out, 'f')
            utf8(out, name)
            flag(out, variadic)
            uvarint(out, parameters.stdlib.length.toLong)
            parameters.stdlib.foreach { parameter => encode(out, parameter) }
            encode(out, result))

        case CDeclaration.Alias(name, target) =>
          Atom(name, Atom.Class.Rigid, hash: out =>
            tag(out, 't')
            utf8(out, name)
            encode(out, target))

        // Fields fold in declaration order — layout is positional — and an opaque tag folds an
        // opacity marker instead, so completing it later is a value change (`cheader.md` §6).
        case CDeclaration.Structure(name, union, fields, opaque) =>
          Atom(name, Atom.Class.Rigid, hash: out =>
            tag(out, if union then 'u' else 's')
            utf8(out, name)
            flag(out, opaque)
            uvarint(out, fields.stdlib.length.toLong)

            fields.stdlib.foreach: (field, typed) =>
              utf8(out, field)
              encode(out, typed))

        case CDeclaration.Enumeration(name, cases) =>
          Atom(name, Atom.Class.Rigid, hash: out =>
            tag(out, 'e')
            utf8(out, name)
            uvarint(out, cases.stdlib.length.toLong)

            cases.stdlib.foreach: (label, value) =>
              utf8(out, label)
              utf8(out, Text(value.toString)))
