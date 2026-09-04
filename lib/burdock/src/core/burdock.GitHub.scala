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
package burdock

import adversaria.*
import anticipation.*
import contingency.*
import distillate.*
import eucalyptus.*
import fulminate.*
import gesticulate.*
import gossamer.*
import jacinta.*
import rudiments.*
import spectacular.*
import symbolism.*
import telekinesis.*
import urticose.*
import vacuous.*
import zephyrine.*

import Repackager.UserError
import denominative.nil
import denominative.size
import errorDiagnostics.emptyDiagnostics
import httpBackends.javaNetHttp
import internetAccess.online

// Resolves dependency content hashes against the release assets of GitHub repositories the
// user has named as hints (`--github owner/repo`). GitHub has no global hash index, but the
// REST API lists a single repository's releases, and each asset carries a `digest` field
// (`sha256:<hex>`), so a repository's assets can be indexed by SHA-256 up front and consulted
// for every dependency without further requests.
object GitHub:
  object Repository:
    given showable: Repository is Showable = _.text

    // A hint is `owner/repo`: exactly one slash, both parts non-empty.
    def parse(text: Text): Repository raises UserError = text.cut(t"/") match
      case List(owner, name) if owner != t"" && name != t"" => Repository(owner, name)

      case _ =>
        abort(UserError(m"$text is not a GitHub repository of the form owner/repo"))

  case class Repository(owner: Text, name: Text):
    def text: Text = t"$owner/$name"

  // The subset of the releases-API payload that matters here. `digest` is absent (or `null`) on
  // assets uploaded before GitHub started computing them; those cannot be matched.
  case class Asset
    ( name: Text, @name[Json](t"browser_download_url") url: Text, digest: Optional[Text] )

  case class Release(assets: List[Asset])

  private val pageSize: Int = 100
  private val digestPrefix: Text = t"sha256:"
  private val jsonMedia: MediaType = media"application/vnd.github+json"

  // Indexes every `.jar` asset carrying a SHA-256 digest by its lowercase hex hash. The API
  // lists newest releases first, so where the same bytes were attached to several releases
  // the newest release's URL wins. Pure, so the mapping is testable without the network.
  def indexReleases(releases: List[Release]): Map[Text, HttpUrl] =
    def entry(asset: Asset): Optional[(Text, HttpUrl)] = asset.digest.let: digest =>
      if asset.name.ends(t".jar") && digest.starts(digestPrefix)
      then safely(asset.url.as[HttpUrl]).let: url => (digest.skip(digestPrefix.length).lower, url)
      else Unset

    // Intermediate results are bound with explicit types so that no implicit search runs
    // against an uninstantiated result variable (the `wildApprox` hazard).
    val assets: List[Asset] = releases.bind(_.assets)
    val pairs: List[(Text, HttpUrl)] = assets.bind: asset => entry(asset).let(List(_)).or(Nil)
    val unique: List[(Text, HttpUrl)] = pairs.deduplicate(_(0))
    unique.to[Map]

  // Lists every release of `repository`, paging until the API returns an empty page (which
  // avoids parsing `Link` headers). A token lifts the unauthenticated rate limit.
  def releases(repository: Repository, token: Optional[Text])
  :   List[Release] logs DepsEvent raises UserError =

    mitigate:
      case Http.Error(status, _) =>
        UserError(m"could not list the releases of $repository (HTTP $status)")

      case Connect.Error(_) =>
        UserError(m"could not connect to api.github.com to list the releases of $repository")

      case Url.Error(_, _, _) =>
        UserError(m"the releases URL for $repository is not valid")

      case Json.Error(_) =>
        UserError(m"the releases listing for $repository was not in the expected format")

      case Parse.Error(_, _, _) =>
        UserError(m"the releases listing for $repository was not valid JSON")

    . protect:
        def page(number: Int): List[Release] =
          val path: Text = t"repos/${repository.owner}/${repository.name}/releases"
          val query: Text = t"per_page=$pageSize&page=$number"
          val url: HttpUrl = t"https://api.github.com/$path?$query".as[HttpUrl]

          val response: Http.Response =
            token.lay(url.fetch(accept = jsonMedia)): token =>
              url.fetch(accept = jsonMedia, authorization = Auth.Bearer(token))

          val batch: List[Release] = mute[Http.Event](response.receive[Json]).as[List[Release]]
          if batch.nil then Nil else batch + page(number + 1)

        Log.fine(DepsEvent.Indexing(repository.text))
        page(1)

  // Builds one index across all the hinted repositories.
  def index(repositories: List[Repository], token: Optional[Text])
  :   Map[Text, HttpUrl] logs DepsEvent raises UserError =

    val pairs: List[(Text, HttpUrl)] = repositories.bind: repository =>
      val index: Map[Text, HttpUrl] = indexReleases(releases(repository, token))
      Log.info(DepsEvent.Indexed(repository.text, index.size))
      val entries: List[(Text, HttpUrl)] = index.to[List]
      entries

    val unique: List[(Text, HttpUrl)] = pairs.deduplicate(_(0))
    unique.to[Map]
