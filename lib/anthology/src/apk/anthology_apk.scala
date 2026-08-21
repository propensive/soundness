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
import rudiments.`:+`
import denominative.asymptotics.linearSizeComplexity

object apkOptions:
  private def apk(edit: Apk.Configuration => Apk.Configuration): Setting =
    Setting[Apk.Configuration](_ == Apk)(edit)

  // The lowest Android API level the application must run on. The level configures both the
  // manifest (`Apk`) and the dexing that precedes it on the path (`Dex`), so this one setting
  // addresses both nodes, dispatching to each node's own configuration type.
  def minApi(level: Int): Setting = new Setting:
    def appliesTo(format: Format): Boolean = format == Apk || format == Dex

    def edit(format: Format, settings: Any): Any = format match
      case Dex => settings.asInstanceOf[DexConfiguration].copy(minApi = level)
      case _   => settings.asInstanceOf[Apk.Configuration].copy(minApi = level)

  def targetApi(level: Int): Setting = apk(_.copy(targetApi = level))
  def packageName(name: Text): Setting = apk(_.copy(packageName = name))
  def label(text: Text): Setting = apk(_.copy(label = text))

  def version(code: Int, name: Text): Setting =
    apk(_.copy(versionCode = code, versionName = name))

  // Adds a requested runtime permission (e.g. `android.permission.VIBRATE`).
  def permission(name: Text): Setting =
    apk: config => config.copy(permissions = config.permissions :+ name)

  def keystore(path: Text, storePass: Text, alias: Text, keyPass: Text): Setting =
    apk(_.copy(keystore = path, storePass = storePass, alias = alias, keyPass = keyPass))

// The packaging edge of a toolchain: `Dex` to `Apk`. It encodes a binary manifest (`Axml`),
// assembles a zip-aligned package (`zeppelin`), and signs it (APK v2, `Apk.Signer`)—a complete,
// installable Android application, produced with no Android SDK build tool. Dexing is the
// preceding edge on the path, so this tool consumes the dex archive it produced; dex settings
// (compilation mode, platform stubs) are addressed to the `Dex` node.
object apkEdges:
  def apply(): List[Edge] = List(Edge(Dex, Apk, ApkTool))

  private object ApkTool extends Tool:
    type Settings = Apk.Configuration

    def name: Text = t"apk"
    def initial: Apk.Configuration = Apk.Configuration.default

    def run
      ( settings:    Apk.Configuration,
        input:       Deliverable,
        entryPoints: List[EntryPoint],
        out:         Path on Linux )
      ( using Monitor, System, WorkingDirectory )
      ( using Tactic[Link.Error], LinkEvent is Loggable )
    :   Deliverable =

      val activity: Fqcn = entryPoints match
        case List(entry) => entry.mainClass
        case Nil         => abort(Link.Error(Link.Error.Reason.NoEntryPoint))
        case _           => abort(Link.Error(Link.Error.Reason.ManyEntryPoints))

      val dexArchive = input.product(Apk)
      Deliverable.Product(package0(settings, dexArchive, activity, out))

  // The packaging step itself: takes an already-dexed archive and yields the signed package.
  private def package0
    ( form:       Apk.Configuration,
      dexArchive: Path on Linux,
      activity:   Fqcn,
      out:        Path on Linux )
  :   Path on Linux logs LinkEvent raises Link.Error =

    try
      jnf.Files.createDirectories(jnf.Paths.get(out.encode.s))

      val dexEntries = unsafely(Zipfile.read(dexArchive).entries).stdlib.filter: entry =>
        entry.ref.encode.ends(t".dex")

      // The binary manifest, built from the configuration and the launcher activity.
      val manifest =
        Axml.encode:
          Apk.Manifest
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
        Apk.Signer.sign(Array.unsafeFrozen(unsigned), form.keystore, form.storePass,
            form.alias, form.keyPass)

      val apkPath = out / "app.apk"
      jnf.Files.write(jnf.Paths.get(apkPath.encode.s), Array.unsafeJvm(signed))
      apkPath

    catch case suc.NonFatal(error) =>
      abort(Link.Error(Link.Error.Reason.Failed(error.stackTrace)))
