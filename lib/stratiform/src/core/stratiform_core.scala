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
package stratiform

import anticipation.*
import contextual.*
import prepositional.*
import rudiments.*
import vacuous.*

// Encodes any value with an `Encodable in Tel` instance to its `Tel` form.
// Mirrors jacinta's `.json`, xylophone's `.xml`, ypsiloid's `.yaml`, etc.
extension [entity: Encodable in Tel](value: entity) def tel: Tel = value.encode

// `tel"…"` extension on StringContext routes through contextual's
// interpolation framework; the actual macro lives in `stratiform.internal`.
// Mirrors `extension (inline context: StringContext) def j` from
// `lib/jacinta/src/core/jacinta_core.scala:230`.
extension (inline context: StringContext)
  transparent inline def tel: Interpolation = interpolation[Tel](context)

// Collection/optic helpers used by the `Tel2` codec and optic givens. They are pure functions of
// their arguments, so they live at package level rather than as members of the `Tel2` trait —
// referencing a trait member would make the codec/optic lambdas capture `Tel2.this`, which the
// pure `Encodable`/`Optic` SAMs reject under capture checking.

// Encodes a collection by flattening each element's compound(s) into one document's children.
private[stratiform] def collectionDocument[value]
    (values: Iterable[value])(using encodable: value is Encodable in Tel)
:   Tel =

  val compounds: IArray[Tel.Compound] = IArray.from:
    values.flatMap: element =>
      encodable.encoded(element).subtree match
        case compound: Tel.Compound => List(compound)
        case document: Tel.Document => document.children.flatMap(_.compounds).to(List)

  Tel(Tel.Document(Unset, Unset, Tel.LineEndings.Lf,
      IArray(Tel.Block(IArray.empty, Unset, compounds, 0))))

// Re-keys a replacement compound to the original child's keyword (so a positional optic update
// preserves field identity).
private[stratiform] def rewrap(original: Tel.Compound, replacement: Tel): Tel.Compound =
  replacement.subtree match
    case compound: Tel.Compound =>
      compound.copy(keyword = original.keyword)

    case document: Tel.Document =>
      original.copy(atoms = IArray.empty[Tel.Atom], remark = Unset, children = document.children)

// Rebuilds a node with replaced children, preserving its document/compound shape.
private[stratiform] def rebuild(origin: Tel, children: IArray[Tel.Block]): Tel = origin.subtree match
  case document: Tel.Document => Tel.make(document.copy(children = children))
  case compound: Tel.Compound => Tel.make(compound.copy(children = children))

// Wraps a value as a compound under the given keyword (used to key map entries' key/value children).
private[stratiform] def reKey(tel: Tel, keyword: Text): Tel.Compound = tel.subtree match
  case c: Tel.Compound => c.copy(keyword = keyword)
  case d: Tel.Document => Tel.Compound(keyword, IArray.empty, Unset, d.children)

