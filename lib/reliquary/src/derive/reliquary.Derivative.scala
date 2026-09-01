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
import denominative.*
import distillate.*
import fulminate.*
import gossamer.*
import prepositional.*
import rudiments.*
import serpentine.*
import turbulence.*
import vacuous.*
import zephyrine.*
import zeppelin.*

import Lira.Error.Reason

// The canonical derivative artifact (lira#1): a byte-deterministic JAR derived from one
// section's materialized tree, whose hash the manifest declares so that a release can be found
// from an ordinary classpath entry alone. The profile is pinned forever:
//
//   - entries in tree order (ascending bytewise path order), no directory entries;
//   - names in UTF-8; no extra fields, no comments, no prefix;
//   - DOS-epoch timestamps;
//   - every entry Stored.
//
// Entries are Stored rather than compressed deliberately: a compression method would pin one
// encoder implementation's exact output into every manifest forever, whereas the Stored profile
// depends only on the content itself.
object Derivative:

  def jar(tree: Lira.Tree, store: Blobstore): Data raises Lira.Error =
    given Zip.Compression = Zip.Compression.Stored

    // Hoisted out of the `map` lambda below: a `t"…"` interpolation evaluated inside a
    // combinator lambda trips the compiler's `wildApprox` assertion.
    val notZipPath = t"the path is not a zip path"

    val entries: List[Zip.Entry] = tree.entries.map: entry =>
      val ref =
        import errorDiagnostics.emptyDiagnostics

        mitigate:
          case _: Path.Error => Lira.Error(Reason.InvalidTree(notZipPath))

        . protect(entry.path.text.as[Path on Zip])

      Zip.Entry(ref, store.resolve(entry.blob))

    val out = java.io.ByteArrayOutputStream()

    Zipfile(entries, Unset, Unset).serialize.drain: region =>
      range =>
        val interval: Interval = range
        out.write(unsafely(region.raw.asInstanceOf[scala.Array[Byte]]), interval.start.n0,
            interval.size)

    Array.unsafeFrozen(out.toByteArray.nn)

  def hash(tree: Lira.Tree, store: Blobstore): Data raises Lira.Error =
    Lira.Hash(Lira.Hash.Domain.Derivative, jar(tree, store))

  // §16 step 3 (L138): every declared derivative hash must recompute from the section's
  // materialized tree. Sections of unknown universes are never materialized (§9.4), so a
  // declared derivative there stays unchecked here, exactly as the rest of the section does.
  def verify(manifest: Lira.Manifest, report: Verification.Report): Unit raises Lira.Error =
    manifest.section.each: section =>
      // The `Optional` field is bound to a typed local as the lambda's first statement: reading
      // it directly inside a combinator lambda trips the `wildApprox` assertion.
      val derivative: Optional[Data] = section.derivative

      derivative.let: declared =>
        report.tree(section.realm, section.integration).let: tree =>
          if Blob.compare(hash(tree, report.blobstore), declared) != 0
          then abort(Lira.Error(Reason.BadDerivative(section.realm)))
