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
package adversaria

import scala.compiletime.summonInline

import anticipation.*
import prepositional.*

extension [entity](entity: entity)
  def membersOfType[value](using deref: entity is Dereferenceable to value): Iterable[value] =
    deref.values(entity)

// Read the `ann`-typed annotations on each field of `self`, keyed by field name,
// keeping only fields that actually carry one (an `Annotated.Fields` lists every
// field bearing *any* annotation, with an empty set for the others). Replaces the
// `match { case _: Annotated.Fields => … case _ => Map() }` boilerplate callers
// would otherwise repeat.
inline def fieldAnnotations[self, annotation <: StaticAnnotation]
:   Map[Text, Set[annotation]] =

  summonInline[self is Annotated by annotation] match
    case annotated: Annotated.Fields => annotated.fields.filter(_(1).nonEmpty)
    case _                           => Map()

// The serialization renames for `format`: a map from each `@name`-annotated
// field's name to its serialized name, with a `@name[format]` overriding a bare
// `@name` (i.e. `@name[Any]`) default on the same field. Fields without a `@name`
// are absent (callers fall back to the field's own name). Used by each format's
// derivation to honour `@name[Xml](t"…")` / `@name(t"…")` in encode and decode.
inline def relabelling[self, format]: Map[Text, Text] =
  val general:  Map[Text, Text] =
    fieldAnnotations[self, name[Any]].map: (field, set) => field -> set.head.name

  val specific: Map[Text, Text] =
    fieldAnnotations[self, name[format]].map: (field, set) => field -> set.head.name

  general ++ specific

// Like `fieldAnnotations`, but reads the `annotation`-typed annotations on the
// subtypes (enum cases / sealed variants) of `self`, keyed by variant name,
// keeping only variants that carry one. Used for renaming sum-type variants.
inline def subtypeAnnotations[self, annotation <: StaticAnnotation]
:   Map[Text, Set[annotation]] =

  summonInline[Annotated by annotation under self] match
    case annotated: Annotated.Subtypes => annotated.subtypes.filter(_(1).nonEmpty)
    case _                             => Map()

// The serialization renames for the variants of a sum type `self`: exactly like
// `relabelling` but for `@name`-annotated enum cases / sealed variants. Maps each
// renamed variant's name to its serialized discriminator.
inline def variantRelabelling[self, format]: Map[Text, Text] =
  val general:  Map[Text, Text] =
    subtypeAnnotations[self, name[Any]].map: (variant, set) => variant -> set.head.name

  val specific: Map[Text, Text] =
    subtypeAnnotations[self, name[format]].map: (variant, set) => variant -> set.head.name

  general ++ specific
