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
import rudiments.*
import vacuous.*

object TypescriptDeclaration:
  // A declaration's namespace path, outermost first: `declare namespace a { namespace b { … } }`
  // gives an inner declaration the scope `a.b`. Two declarations of the same name in different
  // namespaces are different contracts, so the scope is part of every key.
  type Scope = List[Text]

  extension (scope: Scope)
    def qualify(name: Text): Text =
      if scope.stdlib.isEmpty then name else t"${scope.join(t".")}.$name"

// What every declaration has in common. Declared abstractly here rather than as methods on the
// enum, so that each case's own parameters implement them: an enum whose cases redeclare a
// member of the enum body would need an `override` on every parameter.
sealed trait Declared:
  def name: Text
  def scope: TypescriptDeclaration.Scope
  def exported: Boolean

// A top-level declaration of a `.d.ts` file.
//
// `exported` records whether the declaration is reachable by a consumer of the module: in a
// module (a file with any top-level `import`/`export`), only exported declarations are; in a
// legacy global script every top-level declaration is. Resolving that distinction is the
// parser's job, so by the time a declaration reaches a discipline the flag means what it says.
enum TypescriptDeclaration extends Declared:
  case Interface
    ( name:     Text,
      scope:    TypescriptDeclaration.Scope,
      typed:    List[TypescriptType.Parameter],
      extending: List[TypescriptType],
      members:  List[TypescriptMember],
      exported: Boolean )

  case Class
    ( name:       Text,
      scope:      TypescriptDeclaration.Scope,
      typed:      List[TypescriptType.Parameter],
      extending:  Optional[TypescriptType],
      implements: List[TypescriptType],
      members:    List[TypescriptMember],
      isAbstract: Boolean,
      exported:   Boolean )

  case Alias
    ( name:     Text,
      scope:    TypescriptDeclaration.Scope,
      typed:    List[TypescriptType.Parameter],
      target:   TypescriptType,
      exported: Boolean )

  // An enum's members are its contract, and a `const enum` is inlined into consumers at *their*
  // compile time, which is a materially different guarantee — so the flag is carried, not
  // discarded.
  case Enumeration
    ( name:     Text,
      scope:    TypescriptDeclaration.Scope,
      members:  List[(Text, Optional[Text])],
      constant: Boolean,
      exported: Boolean )

  case Function
    ( name:       Text,
      scope:      TypescriptDeclaration.Scope,
      signatures: List[TypescriptType],
      exported:   Boolean )

  case Variable
    ( name:     Text,
      scope:    TypescriptDeclaration.Scope,
      typed:    Optional[TypescriptType],
      constant: Boolean,
      exported: Boolean )

  // The members a declaration presents. Named apart from the `members` parameter that two of the
  // cases carry, which an enum-level method of the same name would collide with.
  def declaredMembers: List[TypescriptMember] = this match
    case Interface(_, _, _, _, members, _)   => members
    case Class(_, _, _, _, _, members, _, _) => members
    case _                                   => Nil

  import TypescriptDeclaration.qualify
  def key: Text = scope.qualify(name)
