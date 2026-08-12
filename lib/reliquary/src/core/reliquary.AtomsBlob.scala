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

import Lira.Error.Reason

// The Atoms metadata blob (§10.4): one discipline's atom listing, one row per atom in ascending
// value-hash order, with the key in human-readable form for diagnostics (the key text
// participates in no hash, but it is what assembly-time reference resolution matches against).
object AtomsBlob:

  def encode(atomization: Atomization): Data =
    val rows = atomization.atoms.stdlib.map: atom =>
      s"atom ${atom.atomClass.keyword}  ${Lira.Hash.text(atom.valueHash)}  ${atom.key}"

    val body = rows.mkString("\n")
    val header = s"tel 1.0 ${Lira.Schemas.atomsSignature}\n\ndiscipline ${atomization.discipline}"
    val text = Text(if rows.isEmpty then s"$header\n" else s"$header\n\n$body\n")
    charEncoders.utf8Encoder.encoded(text)

  def decode(data: Data): Atomization raises Lira.Error =
    import Tels.Decoder.validate

    val document =
      import errorDiagnostics.emptyDiagnostics

      mitigate:
        case Tel.Error(reason, _) =>
          Lira.Error(Reason.InvalidManifest(t"the atoms blob is invalid: $reason"))

      . protect:
          val tel = data.read[Tel]
          tel.validate(using Lira.Schemas.atoms, Lira.Validators.registry)
          tel

    val discipline =
      document.childCompounds.readable.find(_.keyword == t"discipline")
      . map: compound => atomTexts(compound)
      . flatMap(_.headOption)
      . getOrElse(abort(badBlob(t"the discipline identifier is missing")))

    val rows = document.childCompounds.readable.filter(_.keyword == t"atom").toVector

    val atoms = rows.map: compound =>
      val atoms0 = atomTexts(compound)

      if atoms0.length != 3 then abort(badBlob(t"an atom row does not have exactly three atoms"))

      val atomClass = Atom.Class.parse(atoms0(0)) match
        case atomClass: Atom.Class => atomClass
        case _                    => abort(badBlob(t"an atom class is malformed"))

      val hash =
        import errorDiagnostics.emptyDiagnostics

        mitigate:
          case Base256.Error(_) => badBlob(t"an atom hash is malformed")

        . protect(Base256.decodeStrict(atoms0(1)))

      Atom(atoms0(2), atomClass, hash)

    var index = 1

    while index < atoms.length do
      if Blob.compare(atoms(index - 1).valueHash, atoms(index).valueHash) > 0
      then abort(badBlob(t"rows are not in ascending value-hash order"))

      index += 1

    import errorDiagnostics.emptyDiagnostics

    mitigate:
      case DisciplineError(_, reason) => badBlob(t"the listing is inconsistent: $reason")

    . protect(Atomization.of(discipline, List.from(atoms)))

  private def badBlob(detail: Text): Lira.Error =
    import errorDiagnostics.emptyDiagnostics
    Lira.Error(Reason.InvalidManifest(t"the atoms blob is invalid: $detail"))

  private def atomTexts(compound: Tel.Compound): scala.collection.immutable.Vector[Text] =
    compound.atoms.readable.collect:
      case Tel.Atom.Inline(text, _)  => text
      case Tel.Atom.Source(text)     => text
      case Tel.Atom.Literal(_, text) => text

    . toVector
