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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package burdock

import anticipation.*
import contingency.*
import digression.*
import distillate.*
import fulminate.*
import galilei.*
import gossamer.*
import prepositional.*
import revolution.*
import rudiments.*
import serpentine.*
import spectacular.*
import symbolism.*
import turbulence.*
import urticose.*
import vacuous.*
import zeppelin.*

import errorDiagnostics.empty

// The repackager. Reads the dependency hashes embedded by the compile-time macro
// and partitions them: a hash that resolves to a public URL is externalized (a
// `Burdock-Require` reference, fetched at runtime); anything else is inlined by
// copying the cached JAR's classes into the artifact. The completeness invariant:
// every dependency is either externalized or inlined — a hash that is neither is
// an error.
object Repackager:
  object BurdockMain extends ManifestAttribute["Burdock-Main"]
  object BurdockVerbosity extends ManifestAttribute["Burdock-Verbosity"]
  object BurdockRequire extends ManifestAttribute["Burdock-Require"]

  given burdockMain: ("Burdock-Main" is EncodableManifest of Fqcn) = _.text

  given burdockRequire: ("Burdock-Require" is EncodableManifest of List[Requirement]) =
    _.map(_.text).join(t" ")

  given burdockVerbosity: ("Burdock-Verbosity" is EncodableManifest of Text) = identity(_)

  case class Requirement(url: into[HttpUrl], digest: Text):
    def text = t"$digest:$url"

  case class Entry(name: Text, data: Data)
  case class UserError(detail: Message)(using Diagnostics) extends Error(detail)
  case class RepackageError(detail: Message)(using Diagnostics) extends Error(detail)

  // Resolves a dependency hash to its public URL (deps.dev), or `Unset`.
  type Resolver = Text => Optional[HttpUrl]

  // Looks up a dependency hash's entries (directories and the manifest excluded) in the
  // local cache, or `Unset`. Payloads stay lazy, so identifying an externalized
  // dependency's entry names (to strip them) does not read the cached JAR's contents.
  type CacheReader = Text => Optional[List[Zip.Entry]]

  // Pure partition; `resolve` (deps.dev) and `cached` (cache lookup) are injected
  // so the logic is testable without the network or the filesystem.
  def partition(hashes: List[Text], resolve: Resolver, cached: CacheReader)
  :   (List[Requirement], List[Entry]) raises RepackageError =

    val requirements = List.newBuilder[Requirement]
    val inlined = List.newBuilder[Entry]

    hashes.each: hash =>
      resolve(hash) match
        case url: HttpUrl =>
          requirements += Requirement(url, hash)

        case Unset =>
          val entries: List[Zip.Entry] = cached(hash).lest:
            RepackageError(m"dependency $hash is neither published nor cached")

          entries.each: entry =>
            inlined += Entry(entry.ref.show, entry.read[Data])

    (requirements.result(), inlined.result())


  // Rewrites `inputJar` into `outputJar`: keeps the application's own classes,
  // externalizes published dependencies as `Burdock-Require` references (stripping their
  // bundled entries, since they are downloaded at runtime), inlines the rest from the
  // cache, and sets `Main-Class` to the runtime bootstrap. `resolve`/`cached` are injected;
  // `bootstrapClass` is the `burdock.Bootstrap` class bytes (force-included since it can't
  // itself be downloaded).
  def repackage
    ( inputJar: Path on Linux, outputJar: Path on Linux, resolve: Resolver, cached: CacheReader,
      bootstrapClass: Data )
  :   Unit raises RepackageError =

    whereas:
      case IoError(_, _, _, _)  => RepackageError(m"a filesystem error occurred while repackaging")
      case StreamError(_)       => RepackageError(m"a stream error occurred while repackaging")
      case ZipError(reason)     => RepackageError(m"the JAR could not be read or written ($reason)")
      case PathError(_, _)      => RepackageError(m"a path could not be resolved while repackaging")
      case NumberError(_, _, _) => RepackageError(m"the manifest contained malformed data")
      case FqcnError(_, _)      => RepackageError(m"the Main-Class is not a valid class name")

    . mitigate:
        val resource: Text = burdock.internal.ResourcePath.tt
        val bootstrapName: Text = t"burdock/Bootstrap.class"
        var manifestData: Optional[Data] = Unset
        var depsData: Optional[Data] = Unset
        val ownBuilder = List.newBuilder[Entry]

        Zipfile.read(inputJar).entries.each: entry =>
          val name: Text = entry.ref.show

          // The bootstrap class is force-included below, so drop any copy already bundled
          // (an app whose `Main-Class` is `burdock.Bootstrap` bundles burdock) to avoid a
          // duplicate entry.
          if name == t"META-INF/MANIFEST.MF" then manifestData = entry.read[Data]
          else if name == resource then depsData = entry.read[Data]
          else if name == bootstrapName then ()
          else ownBuilder += Entry(name, entry.read[Data])

        val manifest: Manifest =
          manifestData.lest(RepackageError(m"the JAR has no manifest")).read[Manifest]

        val hashes: List[Text] =
          depsData.lest(RepackageError(m"the JAR has no $resource resource")).utf8
          . cut(t"\n").filter(_ != t"")

        val ownEntries: List[Entry] = ownBuilder.result()
        val (requirements, inlined) = partition(hashes, resolve, cached)

        // Published dependencies are downloaded and added to the classpath at runtime, so
        // their bundled entries can be stripped from the repackaged JAR to slim it. Identify
        // them by name from each published dependency's cached JAR (payloads are not read); a
        // dependency absent from the cache can't be identified, so its entries stay bundled —
        // still correct, just not slimmed.
        val publishedEntries: List[Zip.Entry] = requirements.flatMap: requirement =>
          cached(requirement.digest).or(Nil)

        val stripped: Set[Text] = publishedEntries.map(_.ref.show).to(Set)

        val keptEntries: List[Entry] = ownEntries.filter: entry =>
          !stripped.contains(entry.name)

        import manifestAttributes.*

        val originalMain =
          manifest(MainClass).lest(RepackageError(m"the JAR manifest has no Main-Class"))

        val manifest2: Manifest =
          manifest - MainClass + BurdockRequire(requirements) + BurdockMain(originalMain)
          + BurdockVerbosity(t"silent") + MainClass(fqcn"burdock.Bootstrap")

        val bootstrap: Entry = Entry(bootstrapName, bootstrapClass)

        // Keep the first occurrence of each entry name: the force-included bootstrap over any
        // bundled or inlined copy (an unpublished `burdock` dependency's cached JAR also
        // carries `burdock/Bootstrap.class`), and a bundled class over an inlined cache copy.
        // Without this, `Zipfile.write` rejects the duplicate entry.
        val entries: List[Entry] = (bootstrap :: keptEntries ++ inlined).distinctBy(_.name)

        Zipfile.write(outputJar):
          Zip.Entry(t"META-INF/MANIFEST.MF".decode[Path on Zip], manifest2.serialize)
          #:: entries.to(Stream).map: entry =>
            Zip.Entry(entry.name.decode[Path on Zip], () => Stream(entry.data))
