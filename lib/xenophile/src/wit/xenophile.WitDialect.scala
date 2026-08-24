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
import scala.collection.immutable.Map


import anticipation.*
import contingency.*
import gossamer.*
import rudiments.*
import vacuous.*

// The WIT grammar for foreign navigation: a *projection* of `Wit.Parser`'s declaration model
// onto the flat form the wasm backend marshals against — one reader per language, two views,
// exactly as `TypescriptDialect` projects from `Typescript.Parser`.
//
// The parser retains the declared surface faithfully — enum and variant cases, package
// structure, `use` clauses — and this projection deliberately erases what an invocation cannot
// use: `record`s become navigable foreign types whose members are their fields, an interface's
// functions become members of a type named after the interface, `enum`s collapse to the
// unsigned discriminant that holds their cases and `flags` to a bit-vector, `option<T>`
// becomes the union `T | none`, a `result` always carries exactly two arms (missing ones
// padded with `_`), and `type` aliases are resolved transitively. The `package` declaration
// qualifies each interface's functions with their Component Model module id
// (`wasi:random/random@0.2.0`).
object WitDialect extends Dialect:
  // The imports and exports of a `world` declaration, as Component Model interface ids. A world
  // states which host capabilities a component needs and which interfaces it offers — which is
  // precisely what a Wasm OCI Artifact's config records, so a packager can describe a component
  // from the world it was linked against without disassembling the component itself.
  case class World(name: Text, imports: List[Text], exports: List[Text])

  def parse(source: Text): proscenium.Map[Text, proscenium.Map[Text, Prototype]] =
    parse0(source).asInstanceOf[proscenium.Map[Text, proscenium.Map[Text, Prototype]]]

  // Every `world` declared in a source, by name. A bare interface name (`import
  // monotonic-clock;`, naming an interface in the same package) is qualified with the package
  // id, so every id in the result is a full Component Model id.
  def worlds(source: Text): proscenium.Map[Text, World] =
    worlds0(source).asInstanceOf[proscenium.Map[Text, World]]

  private def packageOf(document: Wit.Document): Optional[Text] =
    document.packageName.let: name =>
      document.version.let { version => t"$name@$version" }.or(name)

  private def worlds0(source: Text): Map[Text, World] =
    import strategies.throwUnsafely

    Wit.Parser.parse(source).stdlib.flatMap: document =>
      val pkg = packageOf(document)

      def qualify(id: Text): Text =
        if id.s.contains(":") then id else moduleId(pkg, id).or(id)

      document.worlds.stdlib.map: world =>
        world.name ->
          World(world.name, world.imports.map(qualify(_)), world.exports.map(qualify(_)))

    . toMap

  private def parse0(source: Text): Map[Text, Map[Text, Prototype]] =
    import strategies.throwUnsafely
    val documents = Wit.Parser.parse(source).stdlib

    var types = Map[Text, Map[Text, Prototype]]()
    var typedefs = Map[Text, Foreign.Type]()

    for
      document  <- documents
      interface <- document.interfaces.stdlib
    do
      val pkg = packageOf(document)
      val module = moduleId(pkg, interface.name)
      val functions = scala.collection.mutable.LinkedHashMap[Text, Prototype]()

      def signature(fn: Wit.Function, resource: Optional[Text]): Prototype =
        Prototype
          ( (fn.parameters.stdlib.map { (_, typed) => project(typed) }).to(List),
            if fn.constructor then Foreign.Type.Named(resource.or(t""))
            else fn.result.let(project(_)).or(Foreign.Type.Named(t"unit")),
            module,
            resource,
            fn.static || fn.constructor )

      interface.items.stdlib.foreach:
        case Wit.Item.Function(fn) =>
          functions(fn.name) = signature(fn, Unset)

        case Wit.Item.Record(name, fields) =>
          val members = fields.stdlib.map: (field, typed) =>
            field -> Prototype(Unset, project(typed))

          types = types.updated(name, members.toMap)

        // An `enum` collapses to the unsigned discriminant that holds its cases, and `flags`
        // to the bit-vector that holds its members, so the FFM layouts stay correct.
        case Wit.Item.Enumeration(name, cases) =>
          val count = cases.stdlib.length
          val topic = if count <= 256 then t"u8" else if count <= 65536 then t"u16" else t"u32"
          typedefs = typedefs.updated(name, Foreign.Type.Named(topic))

        case Wit.Item.Flags(name, names) =>
          val count = names.stdlib.length

          val topic =
            if count <= 8 then t"b8" else if count <= 16 then t"b16"
            else if count <= 32 then t"b32" else t"b64"

          typedefs = typedefs.updated(name, Foreign.Type.Named(topic))

        case Wit.Item.Alias(name, target) =>
          typedefs = typedefs.updated(name, project(target))

        // A variant (or a bodyless resource) has no navigable members, but must still record
        // which module defines it, so functions in *other* interfaces that mention it (e.g. in
        // a `result` error arm) resolve its facade class: a single unnameable member carries
        // the module.
        case Wit.Item.Variant(name, _) =>
          types = types.updated(name, declaration(name, module))

        case Wit.Item.Resource(name, methods) =>
          if methods.stdlib.isEmpty then types = types.updated(name, declaration(name, module))
          else
            val members = methods.stdlib.map: method =>
              method.name -> signature(method, name)

            types = types.updated(name, members.toMap)

        case Wit.Item.Use(_, _) => ()

      // Merge, rather than overwrite: a resource or variant sharing the interface's own name
      // (e.g. the `network` resource in `interface network`) has already recorded its module
      // under this key, which a plain overwrite with the interface's (possibly empty)
      // functions would discard.
      val merged = types.get(interface.name).optional.lay(functions.toMap)(_ ++ functions.toMap)
      types = types.updated(interface.name, merged)

    resolve(types, typedefs)

  // Collapses the parser's faithful types to the marshalling vocabulary: `option<T>` is the
  // union `T | none` (an `Optional`), and a `result` always carries exactly two arms.
  private def project(typed: Foreign.Type): Foreign.Type = typed match
    case Foreign.Type.Named(name) =>
      if name == t"result" then padded(scala.Nil) else typed

    case applied: Foreign.Type.Applied =>
      val arguments = applied.arguments.stdlib.map(project(_))

      if applied.constructor == t"option" && arguments.length == 1
      then Foreign.Type.Union(List(arguments.head, Foreign.Type.Named(t"none")))
      else if applied.constructor == t"result" then padded(arguments)
      else Foreign.Type.Applied(applied.constructor, arguments.to(List))

    case Foreign.Type.Union(members) =>
      Foreign.Type.Union(members.map(project(_)))

  private def padded(args: scala.List[Foreign.Type]): Foreign.Type =
    val unit = Foreign.Type.Named(t"_")
    Foreign.Type.Applied(t"result", ((args ++ scala.List(unit, unit)).take(2)).to(List))

  // The pseudo-member recording, for a memberless type declaration, the module that defines it.
  private def declaration(name: Text, module: Optional[Text]): Map[Text, Prototype] =
    Map(t"" -> Prototype(Unset, Foreign.Type.Named(name), module))

  // Resolves every `type` alias appearing in a type, transitively.
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
      Prototype
        ( sig.parameters.let(_.map(expand)),
          expand(sig.result),
          sig.module,
          sig.resource,
          sig.static )

    definitions.map: (name, members) =>
      (name, members.map { (member, sig) => (member, signature(sig)) })

  // Builds the Component Model module id for an interface from the enclosing package id:
  // package `wasi:random@0.2.0` and interface `random` give `wasi:random/random@0.2.0` — the
  // interface name is spliced in before the `@version`. `Unset` when there is no `package`
  // declaration.
  private def moduleId(pkg: Optional[Text], iface: Text): Optional[Text] = pkg.let: id =>
    id.cut(t"@") match
      case base :: version :: _ => t"$base/$iface@$version"
      case _                    => t"$id/$iface"
