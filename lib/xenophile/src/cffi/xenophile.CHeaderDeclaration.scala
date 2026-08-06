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
import fulminate.*
import gossamer.*

// The declaration model of a C header, as `cheader/1` atomizes it (`cheader.md`). Unlike
// `CHeaderDialect`, which canonicalizes for FFI marshalling — collapsing signedness, pointer
// depth and enumerators, none of which a downcall needs — this model retains the declared
// surface: exact arithmetic spellings, pointer structure, struct and union fields, and
// enumerator names with their values.
enum CDeclaration:
  case Function
    ( name:       Text,
      result:     Foreign.Type,
      parameters: List[Foreign.Type],
      variadic:   Boolean = false )

  case Alias(name: Text, target: Foreign.Type)

  case Structure
    ( name:   Text,
      union:  Boolean,
      fields: List[(Text, Foreign.Type)],
      opaque: Boolean = false )

  case Enumeration(name: Text, cases: List[(Text, Long)])

  def named: Text = this match
    case Function(name, _, _, _)  => name
    case Alias(name, _)           => name
    case Structure(name, _, _, _) => name
    case Enumeration(name, _)     => name

object CHeaderError:
  enum Reason(val number: Int) extends Clarification:
    case Syntax(detail: Text, near: Text)  extends Reason(1)
    case Unsupported(construct: Text)      extends Reason(2)

  given communicable: Reason is Communicable =
    case Reason.Syntax(detail, near) => m"$detail, near $near"

    case Reason.Unsupported(construct) =>
      m"the construct $construct is outside the grammar this parser accepts"

// A C header could not be read as declarations. `Unsupported` is deliberately an error and not
// a silent skip: a partially-read header understates the contract, and every claim computed
// from it would be unsound.
case class CHeaderError(reason: CHeaderError.Reason)(using Diagnostics)
extends Error(646, reason.number)(m"the C header could not be read because $reason")
