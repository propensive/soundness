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
import gossamer.*
import vacuous.*

object TypescriptMember:
  // What the member *is*, which decides how a consumer may address it. A property and a method of
  // the same name are different contracts even when their types agree, and a call signature is
  // addressable only by invoking the enclosing type.
  enum Kind:
    case Property, Method, Getter, Setter, Call, Construct, Index

  enum Visibility:
    case Public, Protected, Private

// One member of an interface, class or inline object type.
//
// Overloads are *not* merged: TypeScript resolves a call against the declared signatures in
// order, so a member holds the list it was declared with and the order is semantic.
case class TypescriptMember
  ( name:       Text,
    kind:       TypescriptMember.Kind,
    signatures: List[TypescriptType],
    visibility: TypescriptMember.Visibility = TypescriptMember.Visibility.Public,
    static:     Boolean = false,
    readonly:   Boolean = false,
    optional:   Boolean = false,
    isAbstract: Boolean = false ):

  // The key a member is addressed by within its owning declaration. Getters and setters share a
  // name with a property but are distinct contracts, and index and call signatures have no name
  // of their own, so each kind contributes its own selector shape.
  def selector: Text = kind match
    case TypescriptMember.Kind.Property  => name
    case TypescriptMember.Kind.Method    => name
    case TypescriptMember.Kind.Getter    => t"get $name"
    case TypescriptMember.Kind.Setter    => t"set $name"
    case TypescriptMember.Kind.Call      => t"()"
    case TypescriptMember.Kind.Construct => t"new()"
    case TypescriptMember.Kind.Index     => t"[]"

  // A `private` member is not part of any consumer's contract, and a TypeScript consumer cannot
  // name it. `protected` is, since a subclass may.
  def visible: Boolean = visibility != TypescriptMember.Visibility.Private
