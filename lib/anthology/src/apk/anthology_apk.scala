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
package anthology

import proscenium.compat.*

import java.nio.file as jnf

import scala.util.control as suc

import ambience.*
import anticipation.*
import contingency.*
import digression.*
import galilei.*
import gossamer.*
import parasite.*
import prepositional.*
import serpentine.*
import turbulence.*
import zeppelin.*

import dexLinkages.given

object apkOptions:
  private def apk(edit: ApkConfiguration => ApkConfiguration): Linker.Option[Artifact.Apk] =
    Linker.Option(edit)

  def minApi(level: Int): Linker.Option[Artifact.Apk] = apk(_.copy(minApi = level))
  def targetApi(level: Int): Linker.Option[Artifact.Apk] = apk(_.copy(targetApi = level))
  def packageName(name: Text): Linker.Option[Artifact.Apk] = apk(_.copy(packageName = name))
  def label(text: Text): Linker.Option[Artifact.Apk] = apk(_.copy(label = text))

  def version(code: Int, name: Text): Linker.Option[Artifact.Apk] =
    apk(_.copy(versionCode = code, versionName = name))

  // Adds a requested runtime permission (e.g. `android.permission.VIBRATE`).
  def permission(name: Text): Linker.Option[Artifact.Apk] =
    apk: config => config.copy(permissions = config.permissions :+ name)

  def keystore(path: Text, storePass: Text, alias: Text, keyPass: Text)
  :   Linker.Option[Artifact.Apk] =

    apk(_.copy(keystore = path, storePass = storePass, alias = alias, keyPass = keyPass))

// The classfile-to-APK link family: dexes the compilation (reusing the `Dex` linkage), encodes a
// binary manifest (`Axml`), assembles a zip-aligned package (`zeppelin`), and signs it (APK v2,
// `ApkSigner`) — a complete, installable Android application, produced with no Android SDK build
// tool. Import it where APK artifacts are linked: `import apkLinkages.given`.
object apkLinkages:
  // The packaging step itself, shared between the one-hop `Linkage` and the toolchain's
  // `apkEdges` tool: takes an already-dexed archive and yields the signed package.
  private[anthology] def package0
    ( form:       ApkConfiguration,
      dexArchive: Path on Linux,
      activity:   Fqcn,
      out:        Path on Linux )
  :   Path on Linux logs LinkEvent raises LinkError =

    try
      jnf.Files.createDirectories(jnf.Paths.get(out.encode.s))

      val dexEntries = unsafely(Zipfile.read(dexArchive).entries).stdlib.filter: entry =>
        entry.ref.encode.ends(t".dex")

      // The binary manifest, built from the configuration and the launcher activity.
      val manifest =
        Axml.encode:
          ApkManifest
            ( packageName = form.packageName,
              versionCode = form.versionCode,
              versionName = form.versionName,
              minSdk      = form.minApi,
              targetSdk   = form.targetApi,
              label       = form.label,
              activity    = activity.text,
              permissions = form.permissions )

      // The package: the manifest first, then each dex stored uncompressed and 4-byte aligned
      // so the runtime can memory-map it. `Zip.Compression.Stored` keeps every entry
      // uncompressed, and `.aligned(4)` requests the boundary.
      given Zip.Compression = Zip.Compression.Stored

      val manifestEntry = Zip.Entry(%.on[Zip] / "AndroidManifest.xml", manifest)

      // Reuse each dex entry's own path (`classes.dex`, `classes2.dex`, …), re-storing its
      // decompressed bytes uncompressed and aligned.
      val dexZipEntries = dexEntries.map: entry =>
        Zip.Entry(entry.ref, unsafely(entry.read[Data])).aligned(4)

      val unsignedPath = out / "unsigned.apk"
      unsafely(Zipfile.write(unsignedPath)(manifestEntry :: dexZipEntries))
      val unsigned = jnf.Files.readAllBytes(jnf.Paths.get(unsignedPath.encode.s)).nn

      val signed =
        ApkSigner.sign(Array.unsafeFrozen(unsigned), form.keystore, form.storePass,
            form.alias, form.keyPass)

      val apkPath = out / "app.apk"
      jnf.Files.write(jnf.Paths.get(apkPath.encode.s), Array.unsafeJvm(signed))
      apkPath

    catch case suc.NonFatal(error) =>
      abort(LinkError(LinkError.Reason.Failed(error.stackTrace)))

  given apk: (Linkage[Artifact.Apk] from Universe.Classfile):
    type Origin = Universe.Classfile
    private[anthology] type Form = ApkConfiguration
    private[anthology] def initial: ApkConfiguration = ApkConfiguration.default

    private[anthology] def link
      ( form:        ApkConfiguration,
        compilation: Compilation[Universe.Classfile],
        entryPoints: List[Linker.EntryPoint],
        out:         Path on Linux )
    :   Path on Linux logs LinkEvent raises LinkError =

      val activity: Fqcn = entryPoints match
        case List(entry) => entry.mainClass
        case Nil         => abort(LinkError(LinkError.Reason.NoEntryPoint))
        case _           => abort(LinkError(LinkError.Reason.ManyEntryPoints))

      jnf.Files.createDirectories(jnf.Paths.get(out.encode.s))
      val dexDir = out / "dex"
      jnf.Files.createDirectories(jnf.Paths.get(dexDir.encode.s))

      // Dexing is the `Dex` linkage, reused verbatim: it yields an archive of `classes*.dex`.
      val dexOptionList = List(dexOptions.minApi(form.minApi), dexOptions.mode.release)
      val dexArchive = Linker[Artifact.Dex](dexOptionList).link(compilation, dexDir)

      package0(form, dexArchive, activity, out)

// The packaging edge of a toolchain: `Dex` to `Apk`. Dexing is the preceding edge on the path,
// so this tool consumes the dex archive it produced; the dex edge's own settings (API level,
// release mode) are addressed to the `Dex` node.
object apkEdges:
  def apply(): List[Edge] = List(Edge(Dex, Apk, ApkTool))

  private object ApkTool extends Tool:
    type Settings = ApkConfiguration

    def name: Text = t"apk"
    def initial: ApkConfiguration = ApkConfiguration.default

    def run
      ( settings:    ApkConfiguration,
        input:       Deliverable,
        entryPoints: List[EntryPoint],
        out:         Path on Linux )
      ( using Monitor, System, WorkingDirectory )
      ( using Tactic[LinkError], LinkEvent is Loggable )
    :   Deliverable =

      val activity: Fqcn = entryPoints match
        case List(entry) => entry.mainClass
        case Nil         => abort(LinkError(LinkError.Reason.NoEntryPoint))
        case _           => abort(LinkError(LinkError.Reason.ManyEntryPoints))

      val dexArchive = input.product(Apk)
      Deliverable.Product(apkLinkages.package0(settings, dexArchive, activity, out))
