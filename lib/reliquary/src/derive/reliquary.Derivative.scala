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
import distillate.*
import fulminate.*
import gossamer.*
import prepositional.*
import serpentine.*
import turbulence.*
import vacuous.*
import zephyrine.*
import zeppelin.*

import LiraError.Reason

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

  def jar(tree: LiraTree, store: Blobstore): Data raises LiraError =
    given Zip.Compression = Zip.Compression.Stored

    val entries = tree.entries.stdlib.map: entry =>
      val ref =
        import errorDiagnostics.emptyDiagnostics

        mitigate:
          case _: PathError => LiraError(Reason.InvalidTree(t"the path is not a zip path"))

        . protect(entry.path.text.as[Path on Zip])

      Zip.Entry(ref, store.resolve(entry.blob))

    val out = java.io.ByteArrayOutputStream()

    Zipfile(List.from(entries), Unset, Unset).serialize.sweep: (window, start, count) =>
      out.write(window.asInstanceOf[scala.Array[Byte]], start, count)

    Array.unsafeFrozen(out.toByteArray.nn)

  def hash(tree: LiraTree, store: Blobstore): Data raises LiraError =
    LiraHash(LiraHash.Domain.Derivative, jar(tree, store))
