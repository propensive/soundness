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
import prepositional.*
import vacuous.*

// The declaration model of a WebIDL fragment, as `webidl/1` atomizes it (`webidl.md`). Unlike
// `WebIdlDialect`, which flattens inheritance and erases exactly the distinctions foreign
// navigation does not need, this model retains what the compatibility algebra depends on:
// partiality and mixin identity (resolved by the atomizer, not the parser), required-versus-
// optional dictionary members, `[Exposed]` scopes, and enumeration values.
case class WebIdlArgument
  ( name:     Text,
    typed:    Foreign.Type,
    optional: Boolean = false,
    variadic: Boolean = false,
    default:  Boolean = false )

object WebIdlMember:
  enum Kind:
    case Attribute, Operation, Constant, Constructor

case class WebIdlMember
  ( kind:      WebIdlMember.Kind,
    name:      Text,
    typed:     Foreign.Type,
    arguments: List[WebIdlArgument] = List(),
    readonly:  Boolean              = false,
    static:    Boolean              = false,
    special:   Optional[Text]       = Unset ):

  // The member's selector within its container (`webidl.md` §5): the bare name for attributes
  // and constants, the name with argument types for operations (overloads have distinct
  // selectors), `new(…)` for constructors, and the special-operation keyword in brackets for
  // anonymous getters, setters and deleters.
  def selector: Text =
    def signature: Text = Text(arguments.stdlib.map(_.typed.text.s).mkString(","))

    kind match
      case WebIdlMember.Kind.Attribute | WebIdlMember.Kind.Constant => name
      case WebIdlMember.Kind.Constructor                            => Text(s"new($signature)")

      case WebIdlMember.Kind.Operation =>
        if name.s.isEmpty then Text(s"${special.or(t"?")}[]($signature)")
        else Text(s"$name($signature)")

// One field of a dictionary. A required member folds into the dictionary's own atom — adding
// one breaks every caller constructing the dictionary — while an optional member stands alone
// (`webidl.md` §6).
case class WebIdlField(name: Text, typed: Foreign.Type, required: Boolean, default: Boolean)

enum WebIdlDefinition:
  // `intrinsics` carries the type-level declarations — `iterable<…>`, `maplike<…>`,
  // `setlike<…>`, `async iterable<…>`, bare `stringifier` — as (keyword, type arguments)
  // pairs: they are features of the type rather than members, and fold into the interface's
  // own atom.
  case Interface
    ( name:       Text,
      parent:     Optional[Text] = Unset,
      exposed:    List[Text]     = List(),
      members:    List[WebIdlMember],
      intrinsics: List[(Text, List[Foreign.Type])] = List(),
      partial:    Boolean        = false,
      mixin:      Boolean        = false,
      callback:   Boolean        = false )

  case Dictionary
    ( name:    Text,
      parent:  Optional[Text] = Unset,
      fields:  List[WebIdlField],
      partial: Boolean        = false )

  case Namespace(name: Text, exposed: List[Text], members: List[WebIdlMember],
      partial: Boolean = false)

  case Enumeration(name: Text, values: List[Text])
  case Alias(name: Text, typed: Foreign.Type)
  case CallbackFunction(name: Text, result: Foreign.Type, arguments: List[WebIdlArgument])
  case Includes(target: Text, mixin: Text)

  // The definition's own name — for `includes` statements, the including target.
  def named: Text = this match
    case Interface(name, _, _, _, _, _, _, _) => name
    case Dictionary(name, _, _, _)            => name
    case Namespace(name, _, _, _)             => name
    case Enumeration(name, _)                 => name
    case Alias(name, _)                       => name
    case CallbackFunction(name, _, _)         => name
    case Includes(target, _)                  => target

object WebIdlError:
  enum Reason(val number: Int) extends Clarification:
    case Syntax(detail: Text, near: Text)  extends Reason(1)
    case Unsupported(construct: Text)      extends Reason(2)
    case Duplicate(name: Text)             extends Reason(3)

  given communicable: Reason is Communicable =
    case Reason.Syntax(detail, near) => m"$detail, near $near"

    case Reason.Unsupported(construct) =>
      m"the construct $construct is outside the grammar this parser accepts"

    case Reason.Duplicate(name) => m"the definition $name appears twice"

// A WebIDL fragment could not be read. `Unsupported` is deliberately an error and not a silent
// skip, for the reason `TypescriptError` records: a capability contract read partially is a
// smaller contract than the file declares, and every claim computed from it would be unsound.
case class WebIdlError(reason: WebIdlError.Reason)(using Diagnostics)
extends Error(644, reason.number)(m"the WebIDL could not be read because $reason")
