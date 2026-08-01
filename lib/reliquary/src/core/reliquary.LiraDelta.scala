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

import LiraError.Reason

// One replaced replaceable atom in a lineage step: the same key under an old and a new value.
// (The TEL keywords are `old` and `new`; `new` is not a legal Scala field name.)
case class Replacement(old: Data, next: Data)

object LiraDelta:

  // The atom-level change record of one lineage step (§12.3): the atoms added, and the
  // replaceable atoms replaced. Deltas make staleness computable (§13.4) and allow a verifier
  // holding consecutive releases to check a lineage step exactly.
  def compute(previous: List[Atomization], next: List[Atomization]): LiraDelta =
    def flat(atomizations: List[Atomization]): scala.List[Atom] =
      atomizations.stdlib.flatMap(_.atoms.stdlib)

    val before = flat(previous)
    val after = flat(next)
    val beforeHashes = before.map { atom => LiraHash.text(atom.valueHash) }.toSet

    val added = after
      . filter: atom => !beforeHashes.contains(LiraHash.text(atom.valueHash))
      . map(_.valueHash)
      . sortWith: (a, b) => Blob.compare(a, b) < 0

    val beforeReplaceable = before.filter(_.atomClass == AtomClass.Replaceable)

    val afterReplaceable =
      scala.collection.immutable.Map.from:
        after.filter(_.atomClass == AtomClass.Replaceable).map: atom => (atom.key, atom)

    val replaced = beforeReplaceable
      . flatMap: atom =>
          afterReplaceable.get(atom.key) match
            case scala.Some(successor)
              if Blob.compare(successor.valueHash, atom.valueHash) != 0 =>
              scala.List(Replacement(atom.valueHash, successor.valueHash))

            case _ => scala.Nil

      . sortWith: (a, b) => Blob.compare(a.old, b.old) < 0

    LiraDelta(List.from(added), List.from(replaced))

  def decode(data: Data): LiraDelta raises LiraError =
    import Tels.Decoder.validate

    val document =
      import errorDiagnostics.emptyDiagnostics

      mitigate:
        case TelError(reason, _) =>
          LiraError(Reason.InvalidManifest(t"the delta blob is invalid: $reason"))

      . protect:
          val tel = data.read[Tel]
          tel.validate(using LiraSchemas.delta, LiraValidators.registry)
          tel

    def bad(detail: Text): LiraError =
      import errorDiagnostics.emptyDiagnostics
      LiraError(Reason.InvalidManifest(t"the delta blob is invalid: $detail"))

    def hash(text: Text): Data =
      import errorDiagnostics.emptyDiagnostics

      mitigate:
        case Base256Error(_) => bad(t"a hash is malformed")

      . protect(Base256.decodeStrict(text))

    def texts(compound: Tel.Compound): scala.collection.immutable.Vector[Text] =
      compound.atoms.readable.collect:
        case Tel.Atom.Inline(text, _)  => text
        case Tel.Atom.Source(text)     => text
        case Tel.Atom.Literal(_, text) => text

      . toVector

    val compounds = document.childCompounds.readable

    val added = compounds.filter(_.keyword == t"add").toVector.map: compound =>
      val atoms = texts(compound)
      if atoms.length != 1 then abort(bad(t"an add row does not have exactly one atom"))
      hash(atoms(0))

    val replaced = compounds.filter(_.keyword == t"replace").toVector.map: compound =>
      val atoms = texts(compound)
      if atoms.length != 2 then abort(bad(t"a replace row does not have exactly two atoms"))
      Replacement(hash(atoms(0)), hash(atoms(1)))

    LiraDelta(List.from(added), List.from(replaced))

case class LiraDelta(add: List[Data], replace: List[Replacement]):

  // Canonical serialization: `add` rows in ascending hash order, then `replace` rows in
  // ascending old-hash order, under the pinned `lira-delta` schema signature.
  def encode: Data =
    val addRows = add.stdlib.map: hash => s"add ${LiraHash.text(hash)}"

    val replaceRows = replace.stdlib.map: replacement =>
      s"replace ${LiraHash.text(replacement.old)}  ${LiraHash.text(replacement.next)}"

    val rows = addRows ++ replaceRows
    val header = s"tel 1.0 ${LiraSchemas.deltaSignature}"
    val body = rows.mkString("\n")
    val text = Text(if rows.isEmpty then s"$header\n" else s"$header\n\n$body\n")
    charEncoders.utf8Encoder.encoded(text)
