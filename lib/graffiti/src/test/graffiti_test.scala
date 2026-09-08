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
package graffiti

import soundness.*

import htmlDoms.whatwg.*
import formatting.indentedCssFormatting
import strategies.throwUnsafely
import parasite.probates.awaitProbate
import parasite.threading.virtualThreading

// A page assembled from a modest set of independent template traits, mirroring the README example.
// `content` and `verso` are the only slots the page fills; everything else is contributed by traits.
class Page(title: Text, path: List[Text], dir: HDir = HDir.Ltr)
extends Archetype, VersoPanel, FoldableRectoPanel, TopMenu, Headline(title), Breadcrumbs(path*), Logo:
  override def direction: HDir = dir
  override def menuItems: List[Html of (? <: Phrasing)] = List(Strong("Home"), Strong("About"))
  def content: Html of (? <: Flow) = P("Hello, world!")
  override def verso: Html of (? <: Flow) = menu

// Two pages mixing the panels in opposite orders, to check feature-independence.
class VersoFirst extends Archetype, VersoPanel, RectoPanel:
  def content: Html of (? <: Flow) = P("body")

class RectoFirst extends Archetype, RectoPanel, VersoPanel:
  def content: Html of (? <: Flow) = P("body")

// A page that customises a feature through a single config hook — no `super`, no `frame`/`styles`.
class WideVerso extends Archetype, VersoPanel:
  def content: Html of (? <: Flow) = P("body")
  override def versoWidth = 20.0*Rem

// A fuller page: the regions (hero, masthead, mainstay, colophon) plus a head full of metadata.
class FullPage(title: Text)
extends Archetype, Mainstay, Masthead, Colophon, Hero("Big Headline"), Headline(title),
    StandardMetadata("A demo page"), Author("Jon Pretty"), Keywords("scala", "html"),
    Favicon("/icon.png"), Canonical("https://example.com/"):
  def content: Html of (? <: Flow) = P("Main body")
  override def masthead: Html of (? <: Flow) = P("Site name")
  override def colophon: Html of (? <: Flow) = P("Copyright")

// A page carrying only the aggregated standard metadata.
class MetaPage extends Archetype, StandardMetadata("Just a description"):
  def content: Html of (? <: Flow) = P("x")

// Two features that each introduce a `sidebar` slot of their own — a genuinely incompatible pair:
// the two independent definitions collide, and a page must say which it means to keep.
trait LeftRail extends Archetype:
  def sidebar: Html of (? <: Flow) = Fragment[Flow]()

trait RightRail extends Archetype:
  def sidebar: Html of (? <: Flow) = Fragment[Flow]()

// A concrete dashboard, supplying only the details `Dashboard` leaves abstract.
class AdminDashboard extends Dashboard:
  def brand: Text = "Admin"

  def cards: List[Dashboard.Card] =
    List(Dashboard.Card(t"Users", P("128 active")), Dashboard.Card(t"Revenue", P("4200")))

object Tests extends Suite(m"Graffiti tests"):
  def run(): Unit =
    val page = Page("Welcome", List(t"home", t"docs"))
    val html = page.html.show
    val css = page.css.show

    suite(m"Page assembly"):
      test(m"the document title is set from the Title trait"):
        html.contains("<title>Welcome</title>")
      . assert(_ == true)

      test(m"the page's own content is present"):
        html.contains("Hello, world!")
      . assert(_ == true)

      test(m"the Title trait contributes a heading"):
        html.contains("<h1")
      . assert(_ == true)

      test(m"the top menu is present, with its items"):
        html.contains("graffiti-top-menu") && html.contains("Home")
      . assert(_ == true)

      test(m"the verso slot is filled by the menu helper"):
        html.contains("graffiti-verso")
      . assert(_ == true)

      test(m"the recto panel is present"):
        html.contains("graffiti-recto")
      . assert(_ == true)

      test(m"the breadcrumb trail is present"):
        html.contains("graffiti-breadcrumbs")
      . assert(_ == true)

    suite(m"Contributed CSS"):
      test(m"each trait's rules reach the embedded stylesheet"):
        css.contains(".graffiti-top-menu") && css.contains(".graffiti-verso-layout")
        && css.contains(".graffiti-recto-layout")
      . assert(_ == true)

      test(m"the layout grids are real grids"):
        css.contains("display: grid")
      . assert(_ == true)

    suite(m"Direction is logical, never left/right"):
      test(m"the default page is left-to-right"):
        html.contains("""dir="ltr"""")
      . assert(_ == true)

      test(m"an RTL page only changes the dir attribute"):
        Page("Welcome", List(t"home"), HDir.Rtl).html.show.contains("""dir="rtl"""")
      . assert(_ == true)

      test(m"the CSS uses logical edges and mentions no physical left"):
        css.contains("inline-size") && !css.contains("left")
      . assert(_ == true)

      test(m"the CSS mentions no physical right"):
        !css.contains("right")
      . assert(_ == true)

      test(m"flipping direction leaves the stylesheet byte-for-byte identical"):
        Page("Welcome", List(t"home"), HDir.Rtl).css.show == Page("Welcome", List(t"home")).css.show
      . assert(_ == true)

    suite(m"Feature independence"):
      test(m"verso-then-recto carries both panels"):
        val text = VersoFirst().css.show
        text.contains(".graffiti-verso-layout") && text.contains(".graffiti-recto-layout")
      . assert(_ == true)

      test(m"recto-then-verso, the opposite mix-in order, carries both panels too"):
        val text = RectoFirst().css.show
        text.contains(".graffiti-verso-layout") && text.contains(".graffiti-recto-layout")
      . assert(_ == true)

    suite(m"Customisation through hooks"):
      test(m"a config hook changes the CSS without super or a frame/styles override"):
        WideVerso().css.show.contains("20rem")
      . assert(_ == true)

      test(m"the final page assembly cannot be overridden"):
        demilitarize:
          new Archetype:
            def content: Html of (? <: Flow) = P("x")
            override def css: Css = Css(Nil)
        . exists(_.message.contains("final"))
      . assert(_ == true)

    suite(m"Page regions"):
      val html = FullPage("Demo").html.show

      test(m"the content is wrapped in a <main> landmark"):
        html.contains("graffiti-mainstay")
      . assert(_ == true)

      test(m"the masthead region and its content are present"):
        html.contains("graffiti-masthead") && html.contains("Site name")
      . assert(_ == true)

      test(m"the colophon region and its content are present"):
        html.contains("graffiti-colophon") && html.contains("Copyright")
      . assert(_ == true)

      test(m"the hero banner and its headline are present"):
        html.contains("graffiti-hero") && html.contains("Big Headline")
      . assert(_ == true)

    suite(m"Head metadata"):
      val html = FullPage("Demo").html.show

      test(m"StandardMetadata contributes a description and a viewport"):
        html.contains("A demo page") && html.contains("width=device-width")
      . assert(_ == true)

      test(m"author and keywords reach the head"):
        html.contains("Jon Pretty") && html.contains("scala, html")
      . assert(_ == true)

      test(m"favicon and canonical links reach the head"):
        html.contains("/icon.png") && html.contains("https://example.com/")
      . assert(_ == true)

      test(m"StandardMetadata alone yields a description and a viewport"):
        val head = MetaPage().html.show
        head.contains("Just a description") && head.contains("viewport")
      . assert(_ == true)

    suite(m"Incompatible combinations"):
      test(m"two features that each introduce a sidebar cannot be combined"):
        demilitarize:
          class Clash extends Archetype, LeftRail, RightRail:
            def content: Html of (? <: Flow) = P("x")
        . exists(_.message.contains("sidebar"))
      . assert(_ == true)

    suite(m"Dashboard view"):
      val html = AdminDashboard().html.show

      test(m"the brand titles the document and fills the masthead"):
        html.contains("<title>Admin</title>") && html.contains("graffiti-masthead")
      . assert(_ == true)

      test(m"the cards are laid out in the dashboard grid"):
        html.contains("graffiti-dashboard") && html.contains("graffiti-card")
      . assert(_ == true)

      test(m"each card shows its heading and its own content"):
        html.contains("Users") && html.contains("128 active") && html.contains("Revenue")
      . assert(_ == true)

      test(m"the cards sit inside the <main> landmark"):
        html.contains("graffiti-mainstay")
      . assert(_ == true)

    suite(m"Serving over HTTP"):
      test(m"an archetype declares the text/html media type"):
        Page("Demo", Nil).mediaType.show
      . assert(_.starts("text/html"))

      test(m"serving an archetype streams the full HTML document, with a doctype"):
        supervise(Page("Demo", Nil).read[Text])
      . assert: served =>
          served.contains("<!DOCTYPE html>") && served.contains("Hello, world!")
