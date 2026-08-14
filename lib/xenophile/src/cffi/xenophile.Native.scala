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
import prepositional.*

// The C / native ecosystem: `Interoperable` markers associating Scala types with the C types
// `CHeader.Dialect` reads from header files. No runtime representation is involved.
object Native:
  given int: (Int is Interoperable in Native of "int") =
    Interoperable[Int, Native, "int"]()

  given long: (Long is Interoperable in Native of "long") =
    Interoperable[Long, Native, "long"]()

  given short: (Short is Interoperable in Native of "short") =
    Interoperable[Short, Native, "short"]()

  // A C `char` (as a scalar, not `char*`) is a byte.
  given char: (Byte is Interoperable in Native of "char") =
    Interoperable[Byte, Native, "char"]()

  given double: (Double is Interoperable in Native of "double") =
    Interoperable[Double, Native, "double"]()

  given float: (Float is Interoperable in Native of "float") =
    Interoperable[Float, Native, "float"]()

  given boolean: (Boolean is Interoperable in Native of "bool") =
    Interoperable[Boolean, Native, "bool"]()

  // A C string (`char*` / `const char*`) corresponds to a Scala `Text`.
  given string: (Text is Interoperable in Native of "string") =
    Interoperable[Text, Native, "string"]()

  // Any other C pointer (`T*`, `void*`, an opaque handle, an out-param) corresponds to the raw
  // `Address`; the navigation macro subsumes the `pointer` topic under every `ptr<T>` parameter.
  given pointer: (Address is Interoperable in Native of "pointer") =
    Interoperable[Address, Native, "pointer"]()

trait Native extends Ecosystem:
  type Grammar = CHeader.Dialect.type

  // Two backends, one per target platform: `xenophile.native` lowers to a Panama downcall on the
  // JVM, `xenophile.scalanative` to a `dlsym`/`CFuncPtr` call on Scala Native. A build depends on
  // one or the other — never both — so naming both here lets the same source compile either way,
  // which is what `enigmatic.openssl` relies on.
  type Emission = "xenophile.PanamaInvoke" | "xenophile.NativeInvoke"
