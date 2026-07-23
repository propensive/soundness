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
┃    Soundness, version 0.63.0.                                                                    ┃
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

import proscenium.compat.*

import anticipation.*
import contingency.*
import digression.*
import distillate.*
import fulminate.*
import galilei.*
import gossamer.*
import parasite.{async, supervise, AsyncError}
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

import errorDiagnostics.emptyDiagnostics
import parasite.probates.cancelProbate
import parasite.threading.virtualThreading

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

  case class UserError(detail: Message)(using Diagnostics) extends Error(detail)
  case class RepackageError(detail: Message)(using Diagnostics) extends Error(detail)

  // A record of what `repackage` did, returned so the caller can report it. The core
  // logic stays free of I/O; the command-line entry point prints this.
  case class Summary
    ( inputEntries:       Int,
      directoriesSkipped: Int,
      ownKept:            Int,
      externalized:       List[Requirement],
      inlined:            Int,
      stripped:           Int,
      outputEntries:      Int )

  // Resolves a dependency hash to its public URL (deps.dev), or `Unset`.
  type Resolver = Text => Optional[HttpUrl]

  // Reports per-dependency progress as `(completed, total)`, called once after each dependency is
  // processed. Injected (rather than printed here) so the core stays free of I/O; the command-line
  // entry point renders a progress bar from it.
  type Progress = (Int, Int) => Unit

  // Looks up a dependency hash's entries (directories and the manifest excluded) in the
  // local cache, or `Unset`. Payloads stay lazy, so identifying an externalized
  // dependency's entry names (to strip them) does not read the cached JAR's contents.
  type CacheReader = Text => Optional[List[Zip.Entry]]

  // How many deps.dev lookups to run concurrently (see `partition`). Each is an independent HTTP
  // round-trip, so serial resolution costs `dependencies × latency`; a bounded fan-out cuts that
  // to roughly `ceil(dependencies / parallelism) × latency` while staying polite to the API.
  private val parallelism: Int = 16

  // Partitions the dependency hashes; `resolve` (deps.dev) and `cached` (cache lookup) are injected
  // so the logic is testable without the network or the filesystem. A published dependency becomes
  // a `Requirement` (externalized, fetched at runtime); otherwise it contributes its cached entries
  // to inline. The cached `Zip.Entry` values are carried through verbatim (still compressed) rather
  // than decompressed, so the write path can copy their bytes without re-deflating.
  def partition
    ( hashes: List[Text], resolve: Resolver, cached: CacheReader,
      progress: Progress = (_, _) => () )
  :   (List[Requirement], List[Zip.Entry]) raises RepackageError =

    val requirements = scala.collection.immutable.List.newBuilder[Requirement]
    val inlined = scala.collection.immutable.List.newBuilder[Zip.Entry]
    val total: Int = hashes.length
    var completed: Int = 0

    // Classify a single hash. Pure (no shared mutable state), so many hashes can be classified
    // concurrently. It never raises — a hash that is neither published nor cached is returned as
    // `Unset` and raised (with the offending hash) on the main thread after the join — so the async
    // task's only failure mode is `AsyncError`, which keeps `await`'s error type concrete.
    def classify(hash: Text): Optional[(List[Requirement], List[Zip.Entry])] =
      (resolve(hash): @unchecked) match
        case url: HttpUrl => (List(Requirement(url, hash)), Nil)
        case Unset        => cached(hash).let: entries => (Nil, entries)

    // Resolve deps.dev in parallel: each lookup is an independent HTTP round-trip, so the serial
    // cost is `total × latency`. Fan out over virtual threads, bounded to `parallelism` in flight;
    // await each group in order on this thread (keeping the builders single-threaded) and advance
    // the progress bar once per group, so terminal writes never race across worker threads.
    mitigate:
      case AsyncError(_) =>
        RepackageError(m"a concurrency error occurred while resolving dependencies")

    . protect:
        supervise:
          hashes.batched(parallelism).each: group =>
            val tasks = group.map: hash =>
              async((hash, classify(hash)))

            tasks.map(_.await()).each: (hash, result) =>
              val (reqs, entries) =
                result.lest(RepackageError(m"dependency $hash is neither published nor cached"))

              requirements ++= reqs.stdlib
              inlined ++= entries.stdlib

            completed += group.length
            progress(completed, total)

    (List.of(requirements.result()), List.of(inlined.result()))


  // Rewrites `inputJar` into `outputJar`: keeps the application's own classes,
  // externalizes published dependencies as `Burdock-Require` references (stripping their
  // bundled entries, since they are downloaded at runtime), inlines the rest from the
  // cache, and sets `Main-Class` to the runtime bootstrap. `resolve`/`cached` are injected;
  // `bootstrapClass` is the `burdock.Bootstrap` class bytes (force-included since it can't
  // itself be downloaded).
  def repackage
    ( inputJar: Path on Linux, outputJar: Path on Linux, resolve: Resolver, cached: CacheReader,
      bootstrapClass: Data, progress: Progress = (_, _) => () )
  :   Summary raises RepackageError =

    mitigate:
      case IoError(_, _, _, _)  => RepackageError(m"a filesystem error occurred while repackaging")
      case StreamError(_)       => RepackageError(m"a stream error occurred while repackaging")
      case ZipError(reason)     => RepackageError(m"the JAR could not be read or written ($reason)")
      case PathError(_, _)      => RepackageError(m"a path could not be resolved while repackaging")
      case NumberError(_, _, _) => RepackageError(m"the manifest contained malformed data")
      case FqcnError(_, _)      => RepackageError(m"the Main-Class is not a valid class name")

    . protect:
        val resource: Text = burdock.internal.ResourcePath.tt
        val bootstrapName: Text = t"burdock/Bootstrap.class"
        val manifestName: Text = t"META-INF/MANIFEST.MF"
        var manifestData: Optional[Data] = Unset
        var depsData: Optional[Data] = Unset
        var inputCount: Int = 0
        var directoriesSkipped: Int = 0
        val ownBuilder = scala.collection.immutable.List.newBuilder[Zip.Entry]

        Zipfile.read(inputJar).entries.each: entry =>
          inputCount += 1

          // Directory entries (the zeppelin reader strips their trailing slash and flags them as
          // `directory`) are dropped: a JAR needs no explicit directory entries — extractors
          // synthesise parents on demand — and `cached` drops them too, so keeping them would only
          // reintroduce the "exists but is not directory" extraction clash. Every other entry is
          // kept as its original `Zip.Entry`, so its already-compressed payload is copied verbatim
          // on write rather than inflated here and re-deflated there.
          if entry.directory then directoriesSkipped += 1 else
            val name: Text = entry.ref.show

            // The manifest and the deps resource are the only entries whose *decompressed* bytes
            // are needed (to rewrite the manifest and read the hash list); read those two. Drop any
            // bundled `burdock/Bootstrap.class` (an app whose `Main-Class` is `burdock.Bootstrap`
            // bundles burdock) since the bootstrap is force-included below, avoiding a duplicate.
            if name == manifestName then manifestData = entry.read[Data]
            else if name == resource then depsData = entry.read[Data]
            else if name == bootstrapName then ()
            else ownBuilder += entry

        val manifest: Manifest =
          manifestData.lest(RepackageError(m"the JAR has no manifest")).read[Manifest]

        val hashes: List[Text] =
          depsData.lest(RepackageError(m"the JAR has no $resource resource")).utf8
          . cut(t"\n").filter(_ != t"")

        val ownEntries: List[Zip.Entry] = List.of(ownBuilder.result())
        val (requirements, inlined) = partition(hashes, resolve, cached, progress)

        // Published dependencies are downloaded and added to the classpath at runtime, so
        // their bundled entries can be stripped from the repackaged JAR to slim it. Identify
        // them by name from each published dependency's cached JAR (payloads are not read); a
        // dependency absent from the cache can't be identified, so its entries stay bundled —
        // still correct, just not slimmed.
        val publishedEntries: List[Zip.Entry] = requirements.bind: requirement =>
          cached(requirement.digest).or(Nil)

        val stripped: scala.collection.immutable.Set[Text] =
          publishedEntries.stdlib.map(_.ref.show).toSet

        val keptEntries: List[Zip.Entry] = ownEntries.filter: entry =>
          !stripped.contains(entry.ref.show)

        import manifestAttributes.*

        val originalMain =
          manifest(MainClass).lest(RepackageError(m"the JAR manifest has no Main-Class"))

        val manifest2: Manifest =
          manifest - MainClass + BurdockRequire(requirements) + BurdockMain(originalMain) +
            BurdockVerbosity(t"silent") + MainClass(fqcn"burdock.Bootstrap")

        // The freshly-built entries — the rewritten manifest and the force-included bootstrap class
        // — are the only two that are compressed here; everything else is a verbatim copy.
        val manifestEntry: Zip.Entry =
          Zip.Entry(manifestName.as[Path on Zip], manifest2.serialize)

        val bootstrap: Zip.Entry =
          Zip.Entry(bootstrapName.as[Path on Zip], bootstrapClass)

        // Keep the first occurrence of each entry name: the force-included bootstrap over any
        // bundled or inlined copy (an unpublished `burdock` dependency's cached JAR also
        // carries `burdock/Bootstrap.class`), and a bundled class over an inlined cache copy.
        // Without this, `Zipfile.write` rejects the duplicate entry.
        val entries: List[Zip.Entry] =
          List.of((bootstrap :: keptEntries.stdlib ++ inlined.stdlib).distinctBy(_.ref.show))

        Zipfile.write(outputJar)(manifestEntry #:: Progression.from(entries.stdlib))

        // The output also carries the manifest entry, hence `entries.length + 1`.
        Summary
          ( inputEntries       = inputCount,
            directoriesSkipped = directoriesSkipped,
            ownKept            = keptEntries.length,
            externalized       = requirements,
            inlined            = inlined.length,
            stripped           = ownEntries.length - keptEntries.length,
            outputEntries      = entries.length + 1 )
