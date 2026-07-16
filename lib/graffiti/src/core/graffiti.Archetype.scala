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
package graffiti

import anticipation.*
import cataclysm.*
import cataclysm.formatting.standardCssFormatting
import gesticulate.*
import gossamer.*
import honeycomb.*
import honeycomb.doms.html.whatwg.*
import parasite.*
import prepositional.*
import spectacular.show
import turbulence.*
import zephyrine.Credit

object Archetype:
  // Serve an archetype as an HTTP `text/html` response — exactly as `Document[Html]` is served — by
  // pairing the media type with a text stream of the rendered page. `Servable` is then synthesised
  // (in telekinesis) wherever a handler returns one, so graffiti needs no scintillate dependency.
  // The instances range over every `page <: Archetype`, since a page is always a concrete subtype.
  given media: [page <: Archetype] => page is Media:
    extension (value: page)
      def mediaType: MediaType = media"text/html"(charset = "UTF-8")

  // `^{monitor}` only: `Probate` is not capture-tracked.
  given streamable: [page <: Archetype] => (monitor: Monitor, probate: Probate)
  =>  ((page is Streamable by Text over Credit)^{monitor}) =
    archetype => zephyrine.Stream(archetype.document.lazyList[Text].iterator)

// The base of every page archetype. Concrete pages are built by mixing in feature traits (each a
// subtype of `Archetype`); their only obligation is to provide `content`, the central matter.
//
// Three members are the seams that feature traits compose through:
//   - `frame`  — the wrapping pipeline. Layout traits override it as `decorate(super.frame)`, so a
//                set of them simply nests in linearization order; independent panels never collide.
//   - `styles` — the stylesheet accumulator. Traits override it as `super.styles + ownRules`.
//   - `head`   — the `<head>` accumulator; traits override it as `Fragment(…, super.head)`.
//   - named slots (introduced by feature traits, e.g. `verso`) — filled by the user or a trait.
//
// A page author rarely touches `frame`/`styles` (or `super`): each feature exposes its own hooks —
// content (`verso`, `menu`, …), configuration (`versoWidth`, `menuGap`, …) and a self-contained
// `…Styles` rule set — that are overridden directly, with no `super` call. The seams stay
// `protected` (feature traits chain through them); the page assembly (`html`/`css`) is `final`, so
// a page can't override it and silently lose the inline stylesheet or the direction.
trait Archetype:
  // The central content of the page; supplied by the concrete template.
  def content: Html of (? <: Flow)

  // Document-level metadata.
  def pageTitle: Text = t""

  // Writing direction. `Ltr` by default; flipping it to `Rtl` swaps every logical-property layout
  // (verso ⇄ recto) with no change to the emitted CSS — the browser resolves `inline-start`/`-end`.
  def direction: HDir = HDir.Ltr

  // The composition seams. Feature traits override these, chaining with `super`; a page author
  // customises through the per-feature hooks instead and need not touch them.
  protected def frame: Html of (? <: Flow) = content
  protected def styles: Css = Css(Nil)
  protected def head: Html of (? <: Metadata) = Fragment[Metadata]()

  // The stylesheet rendered to text for inline embedding in a `<style>` element.
  private def stylesheet: Text = styles.show

  // The page stylesheet, as structured CSS (for serving separately, later).
  final def css: Css = styles

  // The complete single-document page: inline `<style>`, accumulated `<head>` metadata, and `dir`
  // set on `<body>` from `direction` (the `<html>` element admits no Whatwg global attributes).
  final def html: Html of "html" =
    Html(Head(Title(pageTitle), Style(stylesheet), head), Body(dir = direction)(frame))

  // The page as a `Document[Html]` with a leading doctype — the form that is served over HTTP.
  final def document: Document[Html] =
    Document[Html](Fragment(Html.doctype, html), doms.html.whatwg)
