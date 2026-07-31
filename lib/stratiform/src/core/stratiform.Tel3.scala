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
package stratiform

import scala.caps

import scala.compiletime.*

import anticipation.*
import distillate.*
import prepositional.*
import wisteria.*

// Lowest-priority layer (extended by `Tel2`), holding the universal fallback
// for `Tel.Field` — the typeclass the product derivation resolves per field —
// mirroring `Tel2.decodable`'s dispatch order so a field's wire format is
// identical on both the direct and the AST paths: an explicit (or derived)
// direct parser; a text codec; structural derivation; or the AST bridge over
// any remaining `Tel.Decodable` (opaque leaf types). A case class with both a
// `Reflection` and a custom hand-written `Tel.Decodable` derives here,
// diverging from its custom decoder — the documented remedy is one line:
// `given MyType is Tel.Parsable = Tel.Parsable.fromDecodable(...)`.
// Lowest priority so `Tel2`'s element-wise field givens match collection and
// `Optional` types first (a `List`'s own `Mirror` would otherwise derive it
// as a sum).
trait Tel3:
  inline given field: [value] => value is Tel.Field = summonFrom:
    case parsable: (`value` is Tel.Parsable) =>
      Tel.Field(parsable)

    case given (`value` is distillate.Decodable in Text) =>
      // Laundered pure per the codec-thunk seal pattern (see rep/DECISIONS.md):
      // the parser closes over the resolution-scoped text codec.
      caps.unsafe.unsafeAssumePure:
        Tel.Field(Tel.textCodecParsable[value])

    case given Reflection[`value`] =>
      Tel.ParsableDerivation.derived

    case given (`value` is Tel.Decodable) =>
      caps.unsafe.unsafeAssumePure:
        Tel.Field(Tel.Parsable.fromDecodable(infer[`value` is Tel.Decodable]))
