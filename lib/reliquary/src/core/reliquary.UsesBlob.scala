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
import denominative.nil
import hieroglyph.*
import rudiments.*
import stratiform.*
import turbulence.*

import Lira.Error.Reason
import rudiments.sortingAlgorithms.timsort
import symbolism.*

// The Uses metadata blob (§13.4): the set of one dependency's atom value hashes that a module
// actually depends on, computed as the module's own direct references transitively closed over
// the reference lists of its dependencies' replaceable atoms — capturing content copied through
// any depth of inline expansion, with no compiler cooperation beyond the archives themselves.
object UsesBlob:

  def encode(module: Text, atoms: List[Data]): Data =
    // A named `Comparable` replaces the stdlib's comparator-taking `sortWith`.
    given (Text, Data) is Comparable = (left, right) =>
      Comparison(Blob.compare(left(1), right(1)))

    // The row text is built by a named `def` rather than inline in the `map` lambda: a `t"…"`
    // interpolation as a combinator lambda's direct body trips the `wildApprox` assertion.
    def row(pair: (Text, Data)): Text = t"atom ${pair(0)}"

    val hashes: List[Text] = atoms.map { hash => Lira.Hash.text(hash) }.distinct
    val decoded: List[(Text, Data)] = hashes.map: text => (text, Base256.decode(text))
    val sorted: List[(Text, Data)] = decoded.sort

    val rows: List[Text] = sorted.map(row)
    val header = s"tel 1.0 ${Lira.Schemas.usesSignature}\n\nmodule $module"
    val body = rows.join(t"\n")
    val text = Text(if rows.nil then s"$header\n" else s"$header\n\n$body\n")
    charEncoders.utf8Encoder.encoded(text)

  def decode(data: Data): (Text, List[Data]) raises Lira.Error =
    import Tels.Decoder.validate

    def bad(detail: Text): Lira.Error =
      import errorDiagnostics.emptyDiagnostics
      Lira.Error(Reason.InvalidManifest(t"the uses blob is invalid: $detail"))

    val document =
      import errorDiagnostics.emptyDiagnostics

      mitigate:
        case Tel.Error(reason, _) =>
          Lira.Error(Reason.InvalidManifest(t"the uses blob is invalid: $reason"))

      . protect:
          val tel = data.read[Tel]
          tel.validate(using Lira.Schemas.uses, Lira.Validators.registry)
          tel

    def texts(compound: Tel.Compound): scala.collection.immutable.Vector[Text] =
      compound.atoms.readable.collect:
        case Tel.Atom.Inline(text, _)  => text
        case Tel.Atom.Source(text)     => text
        case Tel.Atom.Literal(_, text) => text

      . toVector

    val module =
      document.childCompounds.readable.find(_.keyword == t"module")
      . map(texts(_)).flatMap(_.headOption)
      . getOrElse(abort(bad(t"the module name is missing")))

    val atoms = document.childCompounds.readable.filter(_.keyword == t"atom").toVector.map:
      compound =>
        val row = texts(compound)
        if row.length != 1 then abort(bad(t"an atom row does not have exactly one atom"))

        import errorDiagnostics.emptyDiagnostics

        mitigate:
          case _: Base256.Error => bad(t"an atom hash is malformed")

        . protect(Base256.decodeStrict(row(0)))

    (module, atoms.to(List))

  // Transitive closure of a direct-use set over the reference lists of replaceable atoms.
  // `Own` references resolve within the atom's own module; `Foreign` references resolve into
  // any other dependency by exact key match — sound over-approximation where keys are shared.
  def closure(direct: List[Data], dependencies: List[(Text, Atomization)]): List[Data] =
    val byHash = scala.collection.mutable.HashMap[Text, (Text, Atom)]()
    val byKey = scala.collection.mutable.HashMap[(Text, Text), Atom]()

    dependencies.each: pair =>
      pair(1).atoms.each: atom =>
        byHash(Lira.Hash.text(atom.valueHash)) = (pair(0), atom)
        byKey((pair(0), atom.key)) = atom

    val used = scala.collection.mutable.LinkedHashMap[Text, Data]()
    val queue = scala.collection.mutable.ArrayDeque[Data]()
    direct.each: hash => queue.append(hash)

    while queue.nonEmpty do
      val hash = queue.removeHead()
      val text = Lira.Hash.text(hash)

      if !used.contains(text) then
        used(text) = hash

        byHash.get(text) match
          case scala.Some((module, atom)) =>
            atom.references.each: reference =>
              reference match
                case Atom.Reference.Own(key) =>
                  byKey.get((module, key)).foreach: target => queue.append(target.valueHash)

                case Atom.Reference.Foreign(key) =>
                  dependencies.each: other =>
                    byKey.get((other(0), key)).foreach: t => queue.append(t.valueHash)

          case scala.None => ()

    used.values.to(List)

  // §13.4 spanning: a module compiled against one release is also valid against any release
  // whose atom set includes everything the module uses.
  def spanning(used: List[Data], candidate: List[Atom]): Boolean =
    val available = candidate.map { atom => Lira.Hash.text(atom.valueHash) }.to[Set]
    used.all: hash => available.has(Lira.Hash.text(hash))

  // §13.4 staleness: after a minor upgrade, the modules that should be recompiled are exactly
  // those whose used-set intersects the replaced atoms of the traversed deltas. Advisory only —
  // linkage is guaranteed by the algebra.
  def staleness(used: List[Data], replaced: List[Replacement]): Boolean =
    val old = replaced.map { replacement => Lira.Hash.text(replacement.old) }.to[Set]
    used.exists: hash => old.has(Lira.Hash.text(hash))
