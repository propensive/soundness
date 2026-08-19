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

// The dialect works with ordinary Scala collections internally; the single `parse` boundary
// re-wraps as the opaque `Map` (erasure-identical cast).
import scala.collection.immutable.List as SList
import scala.collection.immutable.Map

import proscenium.compat.*

import anticipation.*
import contingency.*
import gossamer.*
import rudiments.*
import vacuous.*

// The WebIDL grammar for foreign navigation: a *projection* of `WebIdl.Parser`'s declaration
// model onto the flat form the JS backend marshals against — one reader per language, two
// views, exactly as `TypescriptDialect` projects from `Typescript.Parser`.
//
// The parser retains the declared surface faithfully — partiality, mixin identity, `[Exposed]`
// scopes, required-versus-optional dictionary members, enumeration values — and this
// projection deliberately erases what a navigation cannot use. An `interface` becomes a
// navigable foreign type whose members are its attributes, constants and named operations; a
// `dictionary` a record; an `enum` a `string`. Inheritance is flattened, so an inherited
// member (`Node`'s `nodeName` on an `HTMLElement`) resolves: each type's members include its
// base chain's and those of every mixin applied by `includes`, with its own overriding.
// `partial` bodies merge into the type they extend, and `typedef`/`enum` aliases resolve
// transitively. Constructors, special operations and intrinsic declarations are not navigable.
object WebIdlDialect extends Dialect:

  def parse(source: Text): proscenium.Map[Text, proscenium.Map[Text, Prototype]] =
    parse0(source).asInstanceOf[proscenium.Map[Text, proscenium.Map[Text, Prototype]]]

  private def parse0(source: Text): Map[Text, Map[Text, Prototype]] =
    import strategies.throwUnsafely
    val definitions = WebIdl.Parser.parse(source).stdlib

    var types = Map[Text, Map[Text, Prototype]]()
    var parents = Map[Text, Text]()
    var includes = Map[Text, SList[Text]]()
    var typedefs = Map[Text, Foreign.Type]()

    def record(name: Text, parent: Optional[Text], members: Map[Text, Prototype]): Unit =
      val merged = types.get(name).optional.lay(members)(_ ++ members)
      types = types.updated(name, merged)
      parent.let { base => parents = parents.updated(name, base) }

    def navigable(members: List[WebIdl.Member]): Map[Text, Prototype] =
      members.stdlib.flatMap: member =>
        member.kind match
          case WebIdl.Member.Kind.Attribute | WebIdl.Member.Kind.Constant =>
            SList(member.name -> Prototype(Unset, member.typed))

          case WebIdl.Member.Kind.Operation =>
            if member.special.present || member.name.s.isEmpty then SList()
            else
              val parameters = List.from(member.arguments.stdlib.map(_.typed))
              SList(member.name -> Prototype(parameters, member.typed))

          case WebIdl.Member.Kind.Constructor => SList()

      . toMap

    definitions.foreach:
      case WebIdl.Definition.Interface(name, parent, _, members, _, _, _, _) =>
        record(name, parent, navigable(members))

      case WebIdl.Definition.Dictionary(name, parent, fields, _) =>
        val members = fields.stdlib.map: field =>
          field.name -> Prototype(Unset, field.typed)

        record(name, parent, members.toMap)

      case WebIdl.Definition.Namespace(name, _, members, _) =>
        record(name, Unset, navigable(members))

      case WebIdl.Definition.Enumeration(name, _) =>
        typedefs = typedefs.updated(name, Foreign.Type.Named(t"string"))

      case WebIdl.Definition.Alias(name, typed) =>
        typedefs = typedefs.updated(name, typed)

      case WebIdl.Definition.Includes(target, mixin) =>
        includes = includes.updated(target, includes.getOrElse(target, SList()) :+ mixin)

      case WebIdl.Definition.CallbackFunction(_, _, _) => ()

    resolve(flatten(types, parents, includes), typedefs)

  // Flattens inheritance: each type's members are those of its base chain, then of every
  // applied mixin, then its own (so a type's own members override inherited ones of the same
  // name). A visited set guards against cycles.
  private def flatten
    ( types:    Map[Text, Map[Text, Prototype]],
      parents:  Map[Text, Text],
      includes: Map[Text, SList[Text]] )
  :   Map[Text, Map[Text, Prototype]] =

    val empty = Map[Text, Prototype]()

    def collect(name: Text, visiting: Set[Text]): Map[Text, Prototype] =
      if visiting.has(name) then types.getOrElse(name, empty)
      else
        val visiting2 = visiting :+ name
        val own = types.getOrElse(name, empty)

        val inherited = parents.get(name).optional.lay(empty): base =>
          collect(base, visiting2)

        val mixedIn = includes.getOrElse(name, SList()).foldLeft(inherited): (acc, mixin) =>
          acc ++ collect(mixin, visiting2)

        mixedIn ++ own

    types.map { (name, _) => (name, collect(name, Set())) }

  // Resolves every `typedef`/`enum` alias appearing in a type, transitively.
  private def resolve
    ( definitions: Map[Text, Map[Text, Prototype]], typedefs: Map[Text, Foreign.Type] )
  :   Map[Text, Map[Text, Prototype]] =

    def expand(foreign: Foreign.Type): Foreign.Type = foreign match
      case Foreign.Type.Named(name) =>
        typedefs.get(name).optional.lay(foreign)(expand)

      case Foreign.Type.Union(members) =>
        Foreign.Type.Union(members.map(expand))

      case Foreign.Type.Applied(constructor, arguments) =>
        Foreign.Type.Applied(constructor, arguments.map(expand))

    def signature(sig: Prototype): Prototype =
      Prototype(sig.parameters.let(_.map(expand)), expand(sig.result))

    definitions.map: (name, members) =>
      (name, members.map { (member, sig) => (member, signature(sig)) })
