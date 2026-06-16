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
package graffiti

import anticipation.*
import cataclysm.*
import cataclysm.cssFormatters.standardCssFormatter
import gossamer.*
import honeycomb.*
import honeycomb.doms.html.whatwg.*
import prepositional.*

// The base of every page archetype. Concrete pages are built by mixing in feature traits (each a
// subtype of `Archetype`); their only obligation is to provide `content`, the page's central matter.
//
// Three members are the seams that feature traits compose through:
//   - `frame`  — the wrapping pipeline. Layout traits override it as `decorate(super.frame)`, so a
//                set of them simply nests in linearization order; independent panels never collide.
//   - `styles` — the stylesheet accumulator. Traits override it as `super.styles + ownRules`.
//   - named slots (introduced by feature traits, e.g. `verso`) — filled by the user or a trait.
//
// A page author rarely touches `frame`/`styles` (or `super`): each feature exposes its own hooks —
// content (`verso`, `menu`, …), configuration (`versoWidth`, `menuGap`, …) and a self-contained
// `…Styles` rule set — that are overridden directly, with no `super` call. The two seams stay
// `protected` (feature traits chain through them); the page assembly (`html`/`css`) is `final`, so a
// page can't override it and silently lose the inline stylesheet or the direction.
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

  // The stylesheet rendered to text for inline embedding in a `<style>` element.
  private def stylesheet: Text = CssSerializer.render(styles)

  // The page stylesheet, as structured CSS (for serving separately, later).
  final def css: Css = styles

  // The complete single-document page: inline `<style>`, `dir` set on `<body>` from `direction`
  // (the document `<html>` element does not admit Whatwg global attributes).
  final def html: Html of "html" =
    Html(Head(Title(pageTitle), Style(stylesheet)), Body(dir = direction)(frame))
