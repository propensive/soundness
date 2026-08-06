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
import vacuous.*

// The declaration model of a WIT document, as `wit/1` atomizes it (`wit.md`). Unlike
// `WitDialect`, which flattens what foreign navigation does not need — enum and variant case
// names, package structure, worlds' contents — this model retains the declared surface whole.
case class WitFunction
  ( name:        Text,
    parameters:  List[(Text, Foreign.Type)],
    result:      Optional[Foreign.Type] = Unset,
    static:      Boolean               = false,
    constructor: Boolean               = false )

enum WitItem:
  case Alias(name: Text, target: Foreign.Type)
  case Record(name: Text, fields: List[(Text, Foreign.Type)])
  case Variant(name: Text, cases: List[(Text, Optional[Foreign.Type])])
  case Enumeration(name: Text, cases: List[Text])
  case Flags(name: Text, names: List[Text])
  case Resource(name: Text, methods: List[WitFunction])
  case Function(function: WitFunction)

  // One `use` clause: the interface it draws from (package-qualified or same-package) and the
  // (original, local) name pairs it introduces. Transparent to atomization — references are
  // encoded fully qualified — but load-bearing for the qualification itself.
  case Use(from: Text, names: List[(Text, Text)])

  def named: Text = this match
    case Alias(name, _)        => name
    case Record(name, _)       => name
    case Variant(name, _)      => name
    case Enumeration(name, _)  => name
    case Flags(name, _)        => name
    case Resource(name, _)     => name
    case Function(function)    => function.name
    case Use(from, _)          => from

case class WitInterface(name: Text, items: List[WitItem])

// A world's referenced imports and exports are interface ids; its *inline* items — `import
// name: func(…)`, `export name: interface { … }` — define rather than reference, and are
// carried as (name, function) pairs, the function absent for an inline interface.
case class WitWorldModel
  ( name:          Text,
    imports:       List[Text],
    exports:       List[Text],
    inlineImports: List[(Text, Optional[WitFunction])] = List(),
    inlineExports: List[(Text, Optional[WitFunction])] = List() )

case class WitDocument
  ( packageName: Optional[Text],
    version:     Optional[Text],
    interfaces:  List[WitInterface],
    worlds:      List[WitWorldModel] )

object WitParseError:
  enum Reason(val number: Int) extends Clarification:
    case Syntax(detail: Text, near: Text)  extends Reason(1)
    case Unsupported(construct: Text)      extends Reason(2)
    case Duplicate(name: Text)             extends Reason(3)
    case Unresolved(name: Text)            extends Reason(4)

  given communicable: Reason is Communicable =
    case Reason.Syntax(detail, near) => m"$detail, near $near"

    case Reason.Unsupported(construct) =>
      m"the construct $construct is outside the grammar this parser accepts"

    case Reason.Duplicate(name)  => m"the definition $name appears twice"
    case Reason.Unresolved(name) => m"the type $name resolves to no declaration"

// A WIT document could not be read. `Unsupported` is deliberately an error and not a silent
// skip: a contract read partially is a smaller contract than the file declares, and every
// claim computed from it would be unsound. `@unstable` items are `Unsupported` by design
// (`wit.md` §4): an unstable item in a published contract would be a stable claim about an
// unstable surface.
case class WitParseError(reason: WitParseError.Reason)(using Diagnostics)
extends Error(645, reason.number)(m"the WIT could not be read because $reason")
