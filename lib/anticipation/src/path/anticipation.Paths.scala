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
┃    Soundness, version 0.63.0.                                                                    ┃
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
package anticipation

import scala.caps

import prepositional.*

object Paths:
  case class Trusted(text: Text)

  // The `java.io.File` / `java.nio.file.Path` "Representative of Paths" bridges live in `diuretic`
  // (with the rest of the `java.*` interop) so that `anticipation.path` stays Scala.js-portable.

  // Resolve a directory `Text` into a `path`, preferring the trusted instantiation — temporary and
  // working directories come from the OS, so they are trusted — and falling back to plain-text
  // instantiation. This is resolved by ordinary implicit search (the `trusted` given in the
  // companion takes priority over `fromText` in the parent), which works inside a staged quote where
  // an inline `summonFrom` over the two `Instantiable` givens cannot be reduced.
  trait Resolver[path]:
    def apply(text: Text): path

  object Resolver extends Resolver2:
    given trusted: [path] => (instantiable: (path is Instantiable across Paths from Trusted)^)
    =>  Resolver[path] =
      // The instance retains its `Instantiable` evidence, which shares the instance's
      // given-resolution lifetime; laundered pure per the codec-thunk seal pattern (see
      // rep/DECISIONS.md). A capturing result here would flow into the inline
      // `workingDirectory`/`temporaryDirectory` resolver parameters, which must stay pure:
      // they are expanded inside staged quotes (ethereal daemon `cli` blocks).
      caps.unsafe.unsafeAssumePure: value =>
        Trusted(value).instantiate

  trait Resolver2:
    given fromText: [path] => (instantiable: (path is Instantiable across Paths from Text)^)
    =>  Resolver[path] =
      // Laundered pure as for `trusted` above.
      caps.unsafe.unsafeAssumePure: value =>
        value.instantiate

sealed trait Paths
