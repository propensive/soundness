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

import proscenium.compat.*

import anticipation.*
import contingency.*
import gossamer.*
import rudiments.*
import vacuous.*

// The C grammar for foreign navigation: a *projection* of `CHeaderParser`'s declaration model
// onto the flat form the FFI backends marshal against — one reader per language, two views,
// exactly as `TypescriptDialect` projects from `Typescript.Parser`.
//
// The parser retains the declared surface faithfully — exact arithmetic spellings, pointer
// structure, enumerators — and this projection deliberately erases what a downcall cannot use:
// `struct`/`union` definitions become navigable foreign types whose members are their fields,
// top-level prototypes become members of a synthetic `"library"` type, `enum`s are treated as
// `int`, `typedef` aliases are resolved transitively, signedness and pointer depth collapse to
// keep the FFM layouts correct, and a plain `char*` is the C-string type. Array fields,
// function-pointer typedefs and opaque tags are outside the marshalling vocabulary and are not
// navigable.
object CHeaderDialect extends Dialect:
  val library: Text = t"library"

  def parse(source: Text): proscenium.Map[Text, proscenium.Map[Text, Prototype]] =
    parse0(source).asInstanceOf[proscenium.Map[Text, proscenium.Map[Text, Prototype]]]

  private def parse0(source: Text): Map[Text, Map[Text, Prototype]] =
    import strategies.throwUnsafely
    val declarations = CHeaderParser.parse(source).stdlib

    val typedefs: Map[Text, Foreign.Type] =
      declarations.collect:
        case CDeclaration.Enumeration(name, _) => name -> Foreign.Type.Named(t"int")

        case CDeclaration.Alias(name, target) if !functionPointer(target) =>
          name -> project(target)

      . toMap

    val structs: Map[Text, Map[Text, Prototype]] =
      declarations.collect:
        case CDeclaration.Structure(name, _, fields, false) =>
          val members = fields.stdlib.filter { (_, typed) => !array(typed) }.map:
            (field, typed) => field -> Prototype(Unset, project(typed))

          name -> members.toMap

      . toMap

    val functions: Map[Text, Prototype] =
      declarations.collect:
        case CDeclaration.Function(name, result, parameters, _) =>
          name -> Prototype(List.from(parameters.stdlib.map(project(_))), project(result))

      . toMap

    val all = if functions.isEmpty then structs else structs.updated(library, functions)
    resolve(all, typedefs)

  private def functionPointer(typed: Foreign.Type): Boolean = typed match
    case applied: Foreign.Type.Applied =>
      applied.constructor == t"fn" || applied.constructor == t"variadic"

    case _ => false

  private def array(typed: Foreign.Type): Boolean = typed match
    case applied: Foreign.Type.Applied => applied.constructor == t"array"
    case _                             => false

  // Collapses the parser's faithful types to the marshalling vocabulary. Only a *plain* `char*`
  // is the C-string type: `unsigned char*`/`signed char*` conventionally mean a byte buffer,
  // not text, so they stay pointers.
  private def project(typed: Foreign.Type): Foreign.Type = typed match
    case Foreign.Type.Named(name) => Foreign.Type.Named(canonical(name))

    case applied: Foreign.Type.Applied =>
      if applied.constructor == t"ptr" then
        val (base, count) = unwrap(typed)

        if base == t"char" && count == 1 then Foreign.Type.Named(t"string")
        else Foreign.Type.Applied(t"ptr", List(Foreign.Type.Named(canonical(base))))
      else if applied.constructor == t"const" then project(applied.arguments.stdlib.head)
      else Foreign.Type.Applied(applied.constructor, applied.arguments.map(project(_)))

    case other => other

  // The innermost named base beneath `ptr` and `const` wrappers, with the pointer depth.
  private def unwrap(typed: Foreign.Type): (Text, Int) = typed match
    case Foreign.Type.Named(name) => (name, 0)

    case applied: Foreign.Type.Applied =>
      if applied.constructor == t"const" then unwrap(applied.arguments.stdlib.head)
      else if applied.constructor == t"ptr" then
        val (base, inner) = unwrap(applied.arguments.stdlib.head)
        (base, inner + 1)
      else (t"*", 0)

    case _ => (t"*", 0)

  // The width-exact and sign-qualified names map to the primitive of the same size, so the FFM
  // layout stays correct; widths without a matching primitive are left as-is.
  private def canonical(name: Text): Text = name.s match
    case "unsigned-int" | "int32_t" | "uint32_t"              => t"int"
    case "unsigned-char" | "signed-char"                      => t"char"
    case "unsigned-short"                                     => t"short"
    case "long-long" | "unsigned-long" | "unsigned-long-long" => t"long"
    case "int64_t" | "uint64_t" | "intptr_t" | "uintptr_t"    => t"long"
    case "size_t" | "ssize_t"                                 => t"long"
    case "long-double"                                        => t"long double"
    case _                                                    => name

  // Resolves every `typedef` alias appearing in a type, transitively.
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
