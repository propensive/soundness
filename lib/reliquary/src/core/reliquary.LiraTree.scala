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
package reliquary

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import hieroglyph.*
import stratiform.*
import turbulence.*
import vacuous.*

import LiraError.Reason

// One row of a Tree metadata blob: a path mapped to the blob holding its content.
case class TreeEntry(path: TreePath, blob: Data)

object LiraTree:
  val empty: LiraTree = LiraTree(List())

  // Establishes the §9.2 invariants: rows sorted in ascending bytewise UTF-8 path order, paths
  // unique. Accepts entries in any order; sorting here is what makes tree serialization a pure
  // function of the mapping.
  def of(entries: List[TreeEntry]): LiraTree raises LiraError =
    val sorted = entries.stdlib.sortWith: (a, b) => TreePath.compare(a.path, b.path) < 0

    sorted.zip(sorted.drop(1)).foreach: (a, b) =>
      if a.path == b.path
      then abort(LiraError(Reason.InvalidTree(t"the path ${a.path.text} appears twice")))

    LiraTree(List.from(sorted))

  // Parses and checks a Tree metadata blob: a TEL document under the `lira-tree` schema, whose
  // pragma carries that schema's signature.
  def decode(data: Data): LiraTree raises LiraError =
    given Tel.Validator.Registry = LiraValidators.registry

    import Tels.Decoder.validate

    val document =
      import errorDiagnostics.emptyDiagnostics

      mitigate:
        case TelError(reason, _) =>
          LiraError(Reason.InvalidTree(t"the document is invalid: $reason"))

      . protect:
          val tel = data.read[Tel]
          tel.validate(using LiraSchemas.tree, LiraValidators.registry)
          tel

    val compounds = document.childCompounds.readable.filter(_.keyword == t"entry").toVector

    val entries = compounds.map: compound =>
      val atoms = compound.atoms.readable.collect:
        case Tel.Atom.Inline(text, _)  => text
        case Tel.Atom.Source(text)     => text
        case Tel.Atom.Literal(_, text) => text

      if atoms.length != 2
      then abort(LiraError(Reason.InvalidTree(t"an entry does not have exactly two atoms")))

      val path = TreePath(atoms(0))

      val hash =
        import errorDiagnostics.emptyDiagnostics

        mitigate:
          case Base256Error(_) => LiraError(Reason.InvalidTree(t"a blob hash is malformed"))

        . protect(Base256.decodeStrict(atoms(1)))

      TreeEntry(path, hash)

    var index = 1

    while index < entries.length do
      val order = TreePath.compare(entries(index - 1).path, entries(index).path)

      val detail =
        if order == 0 then t"the path ${entries(index).path.text} appears twice"
        else t"rows are not in ascending path order"

      if order >= 0 then abort(LiraError(Reason.InvalidTree(detail)))
      index += 1

    LiraTree(List.from(entries))

// A section's mapping from paths to blobs (§9.2), with rows in ascending bytewise path order.
case class LiraTree private(entries: List[TreeEntry]):
  lazy val index: scala.collection.immutable.Map[Text, TreeEntry] =
    scala.collection.immutable.Map.from:
      entries.stdlib.map: entry => (entry.path.text, entry)

  def get(path: TreePath): Optional[TreeEntry] = index.get(path.text).getOrElse(Unset)

  // The canonical serialization: the pragma line carrying the `lira-tree` schema signature, one
  // `entry` row per mapping in tree order, hard-space separated, LF line endings. Deterministic
  // by construction, so a tree blob's hash is a pure function of the mapping.
  def encode: Data =
    val rows = entries.stdlib.map: entry =>
      s"entry ${entry.path.text}  ${LiraHash.text(entry.blob)}"

    val body = rows.mkString("\n")
    val text = Text(s"tel 1.0 ${LiraSchemas.treeSignature}\n\n$body\n")
    charEncoders.utf8Encoder.encoded(text)
