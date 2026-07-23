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

import java.nio.file as jnf

import scala.util.control as suc

import anticipation.*
import contingency.*
import digression.*
import galilei.*
import gossamer.*
import prepositional.*
import rudiments.*
import serpentine.*
import turbulence.*
import vacuous.*
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
    apk: config =>
      config.copy(permissions = config.permissions :+ name)

  def keystore(path: Text, storePass: Text, alias: Text, keyPass: Text)
  :   Linker.Option[Artifact.Apk] =

    apk(_.copy(keystore = path, storePass = storePass, alias = alias, keyPass = keyPass))

// The classfile-to-APK link family: dexes the compilation (reusing the `Dex` linkage), encodes a
// binary manifest (`Axml`), assembles a zip-aligned package (`zeppelin`), and signs it (APK v2,
// `ApkSigner`) — a complete, installable Android application, produced with no Android SDK build
// tool. Import it where APK artifacts are linked: `import apkLinkages.given`.
object apkLinkages:
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

      try
        jnf.Files.createDirectories(jnf.Paths.get(out.encode.s))
        val dexDir = out / "dex"
        jnf.Files.createDirectories(jnf.Paths.get(dexDir.encode.s))

        // Dexing is the `Dex` linkage, reused verbatim: it yields an archive of `classes*.dex`.
        val dexOptionList = List(dexOptions.minApi(form.minApi), dexOptions.mode.release)
        val dexArchive = Linker[Artifact.Dex](dexOptionList).link(compilation, dexDir)

        val dexEntries = unsafely(Zipfile.read(dexArchive).entries.to(List)).filter: entry =>
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
          ApkSigner.sign(unsigned.immutable(using Unsafe), form.keystore, form.storePass,
              form.alias, form.keyPass)

        val apkPath = out / "app.apk"
        jnf.Files.write(jnf.Paths.get(apkPath.encode.s), signed.mutable(using Unsafe))
        apkPath

      catch case suc.NonFatal(error) =>
        abort(LinkError(LinkError.Reason.Failed(error.stackTrace)))
