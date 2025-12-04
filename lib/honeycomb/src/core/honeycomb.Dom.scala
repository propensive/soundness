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
┃    Soundness, version 0.46.0.                                                                    ┃
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
package honeycomb

import language.dynamics

import java.lang as jl

import scala.collection.mutable as scm

import adversaria.*
import anticipation.*
import contingency.*
import denominative.*
import fulminate.*
import gossamer.*
import hellenism.*
import hieroglyph.*
import prepositional.*
import proscenium.*
import rudiments.*
import symbolism.*
import turbulence.*
import typonym.*
import vacuous.*
import zephyrine.*

import classloaders.threadContext
import charDecoders.utf8
import textSanitizers.skip

trait Dom:
  val elements: Dictionary[Tag]
  val entities: Dictionary[Text]

  def infer(parent: Tag, child: Tag): Optional[Tag]
  def generic: Tag = Tag.root(elements.iterator.map(_.label).to(Set))

object Html5 extends Dom:
  import Html.Issue.*

  private def recur(tagname: Text, target: Text): Boolean =
    elements(tagname).lay(false): tag =>
      tag.admissible(target) || tag.insertable

  private type InteractivePhrasing =
    "a" | "audio" | "button" | "embed" | "iframe" | "img" | "input" | "label" | "select"
    | "textarea" | "video"

  type Interactive = InteractivePhrasing | "details"

  type Flow =
    Heading | Phrasing | Sectioning | "address" | "blockquote" | "details" | "dialog" | "div" | "dl"
    | "fieldset" | "figure" | "footer" | "form" | "header" | "hr" | "main" | "menu" | "ol" | "p"
    | "pre" | "table" | "ul" | "search"

  type Phrasing =
    Embedded | InteractivePhrasing | "abbr" | "area" | "b" | "bdi" | "bdo" | "br" | "cite" | "code"
    | "data" | "datalist" | "del" | "dfn" | "em" | "i" | "ins" | "kbd" | "link" | "map" | "mark"
    | "meta" | "meter" | "noscript" | "output" | "progress" | "q" | "ruby" | "s" | "samp" | "script"
    | "slot" | "small" | "span" | "strong" | "sub" | "sup" | "template" | "time" | "u" | "var"
    | "wbr" | "selectedcontent" | "#text"

  type Embedded =
    "audio" | "canvas" | "embed" | "iframe" | "img" | "object" | "picture" | "video" | "math"
    | "svg"

  type Sectioning = "article" | "aside" | "nav" | "section"
  type ScriptSupporting = "script" | "template"
  type Metadata = "base" | "link" | "meta" | "noscript" | "script" | "style" | "template" | "title"
  type Heading = "h1" | "h2" | "h3" | "h4" | "h5" | "h6" | "hgroup"

  def insertable(tag: Tag): Set[Tag] =
    tag.admissible.map(elements(_)).compact.filter(_.insertable)

  def infer(parent: Tag, child: Tag): Optional[Tag] =
    def recur(parent: Tag): Boolean =
      parent.admissible.contains(child.label) || insertable(parent).exists(recur(_))

    insertable(parent).find(recur(_)).optional



  // Elements

  val A = Tag.transparent["a"]()
  val Abbr = Tag.container["abbr", Phrasing]()

  val Address =
    Tag.container
     ["address",
      "a" | "abbr" | "area" | "audio" | "b" | "bdi" | "bdo" | "blockquote" | "br" | "button"
      | "canvas" | "cite" | "code" | "data" | "datalist" | "del" | "details" | "dfn" | "dialog"
      | "div" | "dl" | "em" | "embed" | "fieldset" | "figure" | "form" | "hr" | "i" | "iframe"
      | "img" | "input" | "ins" | "kbd" | "label" | "link" | "main" | "map" | "mark" | "menu"
      | "meta" | "meter" | "noscript" | "object" | "ol" | "output" | "p" | "picture" | "pre"
      | "progress" | "q" | "ruby" | "s" | "samp" | "script" | "select" | "slot" | "small" | "span"
      | "strong" | "sub" | "sup" | "table" | "template" | "textarea" | "time" | "u" | "ul" | "var"
      | "video" | "wbr"]
     ()

  val Area = Tag.void["area"]()
  val Article = Tag.container["article", Flow]()
  val Aside = Tag.container["aside", Flow]()

  // - transparent content
  // - audio and video are prohibited in transparent content
  // - conditions based on presence or absence of `src` attribute
  val Audio = Tag.container["audio", "source" | "track" | "#transparent"]()

  val B = Tag.container["b", Phrasing]()

  // - `href` or `target` attributes are required
  val Base = Tag.void["base"]()

  val Bdi = Tag.container["bdi", Phrasing]()
  val Bdo = Tag.container["bdo", Phrasing]()
  val Blockquote = Tag.container["blockquote", Flow]()
  val Body = Tag.container["body", Flow](autoclose = true, insertable = true)
  val Br = Tag.void["br"]()

  // - constraints on content
  val Button = Tag.container["button", Phrasing]()

  // - transparent, but non-interactive
  val Canvas = Tag.transparent["canvas"]()

  val Caption = Tag.container["caption", Flow]()
  val Cite = Tag.container["cite", Phrasing]()
  val Code = Tag.container["code", Phrasing]()
  val Col = Tag.void["col"]()

  val Colgroup = Tag.container["colgroup", "col"]
                  (content = Html.TextContent.Whitespace, insertable = true)

  val Data = Tag.container["data", Phrasing]()
  val Datalist = Tag.container["datalist", Phrasing | "option"]()
  val Dd = Tag.container["dd", Flow](autoclose = true)
  val Del = Tag.transparent["del"]()
  val Details = Tag.container["details", "summary" | Flow]()
  val Dfn = Tag.container["dfn", Phrasing]()
  val Dialog = Tag.container["dialog", Flow]()
  val Div = Tag.container["div", Flow]()

  val Dl = Tag.container["dl", "div" | "dt" | ScriptSupporting]
            (autoclose = true, content = Html.TextContent.Whitespace)

  val Dt = Tag.container["dl", Flow](autoclose = true)
  val Em = Tag.container["em", Phrasing]()
  val Embed = Tag.void["embed"]()
  val Fieldset = Tag.container["fieldset", "legend" | Flow]()
  val Figcaption = Tag.container["figcaption", Flow]()
  val Figure = Tag.container["figure", "figcaption" | Flow]()
  val Footer = Tag.container["footer", Flow]()
  val Form = Tag.container["form", Flow]()
  val H1 = Tag.container["h1", Phrasing]()
  val H2 = Tag.container["h2", Phrasing]()
  val H3 = Tag.container["h3", Phrasing]()
  val H4 = Tag.container["h4", Phrasing]()
  val H5 = Tag.container["h5", Phrasing]()
  val H6 = Tag.container["h6", Phrasing]()

  val Head = Tag.container["head", Metadata]
              (autoclose = true, content = Html.TextContent.Whitespace, insertable = true)

  val Header = Tag.container["header", Flow](autoclose = true)
  val Hgroup = Tag.container["hgroup", "p" | "h1" | "h2" | "h3" | "h4" | "h5" | "h6"]()
  val Hr = Tag.void["hr"]()
  val Html = honeycomb.Html
  val I = Tag.container["i", Phrasing]()
  val Iframe = Tag.void["iframe"]()
  val Img = Tag.void["img"]()

  object Input extends Tag.Void("input", Nil):
    type Topic = "input"
    type Transport = ""

    val Button = Tag.void["input"](presets = List((t"type", t"button")))
    // FIXME: More Input types

  val Ins = Tag.transparent["ins"]()
  val Kbd = Tag.container["kbd", Phrasing]()
  val Label = Tag.container["label", Phrasing]()
  val Legend = Tag.container["label", Phrasing | "h1" | "h2" | "h3" | "h4" | "h5" | "h6"]()
  val Li = Tag.container["li", Flow](autoclose = true)
  val Link = Tag.void["link"]()
  val Main = Tag.container["main", Flow]()
  val Map = Tag.transparent["map"]()
  val Mark = Tag.container["mark", Phrasing]()
  val Math = Tag.foreign["math"]()
  val Menu = Tag.container["menu", "li" | ScriptSupporting](content = Html.TextContent.Whitespace)
  val Meta = Tag.void["meta"]()
  val Meter = Tag.container["meter", Phrasing]()
  val Nav = Tag.container["nav", Flow]()
  val Noscript = Tag.container["noscript", "link" | "style" | "meta"]()
  val Object = Tag.transparent["object"]()
  val Ol = Tag.container["ol", "li" | ScriptSupporting](content = Html.TextContent.Whitespace)

  val Optgroup = Tag.container["optgroup", "option" | "legend"]
                  (autoclose = true, content = Html.TextContent.Whitespace)

  val Option = Tag.container["option", "#text"](autoclose = true)
  val Output = Tag.container["output", Phrasing]()
  val P = Tag.container["p", Phrasing](autoclose = true)

  val Picture = Tag.container["picture", "source" | "img" | ScriptSupporting]
                 (content = Html.TextContent.Whitespace)

  val Pre = Tag.container["pre", Phrasing]()
  val Progress = Tag.container["progress", Phrasing]()
  val Q = Tag.container["q", Phrasing]()
  val Rp = Tag.container["rp", "#text"](autoclose = true)
  val Rt = Tag.container["rt", Phrasing](autoclose = true)
  val Ruby = Tag.container["ruby", Phrasing | "rt" | "rp"]()
  val S = Tag.container["s", Phrasing]()
  val Samp = Tag.container["samp", Phrasing]()
  val Script = Tag.container["script", "#text"](content = Html.TextContent.Raw)
  val Search = Tag.container["search", Flow]()
  val Section = Tag.container["section", Flow]()

  val Select =
    Tag.container
     ["select", "option" | "optgroup" | "hr" | "button" | "noscript" | ScriptSupporting]
     (content = Html.TextContent.Whitespace)

  val Selectedcontent = Tag.void["selectedcontent"]()
  val Slot = Tag.transparent["slot"]()
  val Small = Tag.container["small", Phrasing]()
  val Source = Tag.void["source"]()
  val Span = Tag.container["span", Phrasing]()
  val Strong = Tag.container["strong", Phrasing]()
  val Style = Tag.container["style", "#text"](content = Html.TextContent.Raw)
  val Sub = Tag.container["sub", Phrasing]()
  val Summary = Tag.container["summary", Phrasing | Heading]()
  val Sup = Tag.container["sup", Phrasing]()
  val Svg = Tag.foreign["svg"]()

  val Table =
    Tag.container["table", "caption" | "colgroup" | "thead" | "tbody" | "tfoot"]
     (content = Html.TextContent.Whitespace)

  val Tbody = Tag.container["tbody", "tr"]
               (autoclose = true, content = Html.TextContent.Whitespace, insertable = true)

  val Td = Tag.container["td", Flow](autoclose = true)
  val Template = Tag.void["template"]()
  val Textarea = Tag.container["textarea", "#text"](content = Html.TextContent.Rcdata)

  val Tfoot = Tag.container["tfoot", "tr"]
               (autoclose = true, content = Html.TextContent.Whitespace)

  val Th = Tag.container["th", Flow](autoclose = true)

  val Thead = Tag.container["thead", "tr" | ScriptSupporting]
               (autoclose = true, content = Html.TextContent.Whitespace)

  val Time = Tag.container["time", Phrasing]()
  val Title = Tag.container["title", "#text"](content = Html.TextContent.Rcdata)

  val Tr = Tag.container["tr", "td" | "th" | ScriptSupporting]
            (autoclose = true, content = Html.TextContent.Whitespace, insertable = true)

  val Track = Tag.void["track"]()
  val U = Tag.container["u", Phrasing]()
  val Ul = Tag.container["ul", "li" | ScriptSupporting](content = Html.TextContent.Whitespace)
  val Var = Tag.container["var", Phrasing]()

  // FIXME: Transparent + source + track
  val Video = Tag.container["video", "track" | "#transparent" | "source"]()
  val Wbr = Tag.void["wbr"]()


  // Attributes
  type Attribute = honeycomb.Attribute in this.type

  private type HeightTags =
    "canvas" | "embed" | "iframe" | "img" | "input" | "object" | "video" | "svg"

  private type CrossoriginTags = "audio" | "img" | "link" | "script" | "video"

  private type DisabledTags =
    "button" | "fieldset" | "input" | "optgroup" | "option" | "select" | "textarea"

  private type FormTags =
    "button" | "fieldset" | "input" | "object" | "output" | "select" | "textarea"

  private type MediaTags = "a" | "area" | "link" | "source" | "style"

  private type NameTags =
    "button" | "form" | "fieldset" | "iframe" | "input" | "object" | "output" | "select"
    | "textarea" | "map" | "meta" | "param"

  private type ReferrerpolicyTags = "a" | "area" | "iframe" | "img" | "link" | "script"

  private type RequiredTags = "input" | "select" | "textarea"

  private type SrcTags =
    "audio" | "embed" | "iframe" | "img" | "input" | "script" | "source" | "track" | "video"

  private type TypeTags =
    "button" | "input" | "embed" | "object" | "ol" | "script" | "source" | "style" | "menu" | "link"

  private type ValueTags =
    "button" | "data" | "input" | "li" | "meter" | "option" | "progress" | "param"

  private type WidthTags =
    "canvas" | "embed" | "iframe" | "img" | "input" | "object" | "video" | "svg"

  // abbr
  erased given accept: ("accept" is Attribute on "form" | "input" of Text) = !!
  erased given acceptCharset: ("acceptCharset" is Attribute on "form" of Text) = !!
  erased given accesskey: ("accesskey" is Attribute of Text) = !!
  erased given action: ("action" is Attribute on "form" of Text) = !!
  erased given allow: ("allow" is Attribute on "iframe" of Text) = !!
  // allowfullscreen
  erased given alpha: ("alpha" is Attribute on "input" of Text) = !!
  erased given alt: ("alt" is Attribute on "area" | "img" | "input" of Text) = !!
  // erased given anchor: ("anchor" is Attribute of Text) = !!
  erased given as: ("as" is Attribute on "link" of Text) = !!
  erased given async: ("async" is Attribute on "script" of Text) = !!
  erased given autocapitalize: ("autocapitalize" is Attribute of Text) = !!
  // autocomplete
  // autocomplete
  erased given autocorrect: ("autocorrect" is Attribute of Text) = !!
  erased given autofocus: ("autofocus" is Attribute of Text) = !!
  erased given autoplay: ("autoplay" is Attribute on "audio" | "video" of Text) = !!
  // blocking
  //erased given capture: ("capture" is Attribute on "input" of Text) = !!
  erased given charset: ("charset" is Attribute on "meta" of Text) = !!
  erased given checked: ("checked" is Attribute on "input" of Text) = !!
  erased given cite: ("cite" is Attribute on "blockquote" | "del" | "ins" | "q" of Text) = !!
  erased given `class`: ("class" is Attribute of Text) = !!
  // closedby
  // color
  erased given colorspace: ("colorspace" is Attribute on "input" of Text) = !!
  erased given cols: ("cols" is Attribute on "textarea" of Text) = !!
  erased given colspan: ("colspan" is Attribute on "td" | "th" of Text) = !!
  // command
  // commandfor
  // content
  erased given contenteditable: ("contenteditable" is Attribute of Text) = !!
  erased given controls: ("controls" is Attribute on "audio" | "video" of Text) = !!
  erased given coords: ("coords" is Attribute on "area" of Text) = !!
  erased given crossorigin: ("crossorigin" is Attribute on CrossoriginTags of Text) = !!
  //erased given csp: ("csp" is Attribute on "iframe" of Text) = !!
  erased given data: ("data" is Attribute on "object" of Text) = !!
  erased given datetime: ("datetime" is Attribute on "del" | "ins" | "time" of Text) = !!
  // datetime
  erased given decoding: ("decoding" is Attribute on "img" of Text) = !!
  erased given default: ("default" is Attribute on "track" of Text) = !!
  erased given defer: ("defer" is Attribute on "script" of Text) = !!
  erased given dir: ("dir" is Attribute of Text) = !!
  // dir
  erased given dirname: ("dirname" is Attribute on "input" | "textarea" of Text) = !!
  erased given disabled: ("disabled" is Attribute on DisabledTags of Boolean) = !!
  // disabled
  // disabled
  //erased given display: ("display" is Attribute on "math" of Text) = !!
  erased given download: ("download" is Attribute on "a" | "area" of Text) = !!
  erased given draggable: ("draggable" is Attribute of Text) = !!
  //erased given elementtiming: ("elementtiming" is Attribute on "img" | "video" of Text) = !!
  erased given enctype: ("enctype" is Attribute on "form" of Text) = !!
  erased given enterkeyhint: ("enterkeyhint" is Attribute of Text) = !!
  //erased given exportparts: ("exportparts" is Attribute of Text) = !!
  erased given fetchpriority: ("fetchpriority" is Attribute on "img" | "link" | "script" of Text) = !!
  erased given `for`: ("for" is Attribute on "label" | "output" of Text) = !!
  // for
  erased given form: ("form" is Attribute on FormTags of Text) = !!
  erased given formaction: ("formaction" is Attribute on "input" | "button" of Text) = !!
  erased given formenctype: ("formenctype" is Attribute on "input" | "button" of Text) = !!
  erased given formmethod: ("formmethod" is Attribute on "input" | "button" of Text) = !!
  erased given formnovalidate: ("formnovalidate" is Attribute on "input" | "button" of Text) = !!
  erased given formtarget: ("formtarget" is Attribute on "input" | "button" of Text) = !!
  erased given headers: ("headers" is Attribute on "td" | "th" of Text) = !!
  // headingoffset
  // headingreset
  erased given height: ("height" is Attribute on HeightTags of Text) = !!
  erased given hidden: ("hidden" is Attribute of Text) = !!
  erased given high: ("high" is Attribute on "meter" of Text) = !!
  erased given href: ("href" is Attribute on "a" | "area" | "base" | "link" of Text) = !!
  // href
  // href
  erased given hreflang: ("hreflang" is Attribute on "a" | "link" of Text) = !!
  erased given httpEquiv: ("http-equiv" is Attribute on "meta" of Text) = !!
  erased given id: ("id" is Attribute of Text) = !!
  // imagesizes
  // imagesrcset
  erased given inert: ("inert" is Attribute of Text) = !!
  erased given inputmode: ("inputmode" is Attribute of Text) = !!
  erased given integrity: ("integrity" is Attribute on "link" | "script" of Text) = !!
  erased given is: ("is" is Attribute of Text) = !!
  erased given ismap: ("ismap" is Attribute on "img" of Text) = !!
  erased given itemid: ("itemid" is Attribute of Text) = !!
  erased given itemprop: ("itemprop" is Attribute of Text) = !!
  erased given itemref: ("itemref" is Attribute of Text) = !!
  erased given itemscope: ("itemscope" is Attribute of Text) = !!
  erased given itemtype: ("itemtype" is Attribute of Text) = !!
  erased given kind: ("kind" is Attribute on "track" of Text) = !!
  erased given label: ("label" is Attribute on "optgroup" | "option" | "track" of Text) = !!
  erased given lang: ("lang" is Attribute of Text) = !!
  erased given list: ("list" is Attribute on "input" of Text) = !!
  erased given loading: ("loading" is Attribute on "img" | "iframe" of Text) = !!
  erased given loop: ("loop" is Attribute on "audio" | "video" of Text) = !!
  erased given low: ("low" is Attribute on "meter" of Text) = !!
  erased given max: ("max" is Attribute on "input" | "meter" | "progress" of Text) = !!
  // max
  erased given maxlength: ("maxlength" is Attribute on "input" | "textarea" of Text) = !!
  erased given media: ("media" is Attribute on MediaTags of Text) = !!
  erased given method: ("method" is Attribute on "form" of Text) = !!
  erased given min: ("min" is Attribute on "input" | "meter" of Text) = !!
  // min
  erased given minlength: ("minlength" is Attribute on "input" | "textarea" of Text) = !!
  erased given multiple: ("multiple" is Attribute on "input" | "select" of Text) = !!
  erased given muted: ("muted" is Attribute on "audio" | "video" of Text) = !!
  erased given name: ("name" is Attribute on NameTags of Text) = !!
  // name
  // name
  // name
  // name
  // name
  // name
  // nomodule
  erased given nonce: ("nonce" is Attribute of Text) = !!
  erased given novalidate: ("novalidate" is Attribute on "form" of Text) = !!
  erased given open: ("open" is Attribute on "details" | "dialog" of Text) = !!
  erased given optimum: ("optimum" is Attribute on "meter" of Text) = !!
  //erased given part: ("part" is Attribute of Text) = !!
  erased given pattern: ("pattern" is Attribute on "input" of Text) = !!
  erased given ping: ("ping" is Attribute on "a" | "area" of Text) = !!
  erased given placeholder: ("placeholder" is Attribute on "input" | "textarea" of Text) = !!
  erased given playsinline: ("playsinline" is Attribute on "video" of Text) = !!
  erased given popover: ("popover" is Attribute of Text) = !!
  // popovertarget
  // popovertargetaction
  erased given poster: ("poster" is Attribute on "video" of Text) = !!
  erased given preload: ("preload" is Attribute on "audio" | "video" of Text) = !!
  erased given readonly: ("readonly" is Attribute on "input" | "textarea" of Text) = !!
  // readonly
  erased given referrerpolicy: ("referrerpolicy" is Attribute on ReferrerpolicyTags of Text) = !!
  erased given rel: ("rel" is Attribute on "a" | "area" | "link" of Text) = !!
  // rel
  erased given required: ("required" is Attribute on RequiredTags of Text) = !!
  erased given reversed: ("reversed" is Attribute on "ol" of Text) = !!
  //erased given role: ("role" is Attribute of Text) = !!
  erased given rows: ("rows" is Attribute on "textarea" of Text) = !!
  erased given rowspan: ("rowspan" is Attribute on "td" | "th" of Text) = !!
  erased given sandbox: ("sandbox" is Attribute on "iframe" of Text) = !!
  erased given scope: ("scope" is Attribute on "th" of Text) = !!
  erased given selected: ("selected" is Attribute on "option" of Text) = !!
  // shadowrootclonable
  // shadowrootcustomelementregistry
  // shadowrootdelegatesfocus
  // shadowrootmode
  // shadowrootserializable
  erased given shape: ("shape" is Attribute on "a" | "area" of Text) = !!
  erased given size: ("size" is Attribute on "input" | "select" of Text) = !!
  erased given sizes: ("sizes" is Attribute on "link" | "img" | "source" of Text) = !!
  // sizes
  erased given slot: ("slot" is Attribute of Text) = !!
  erased given span: ("span" is Attribute on "col" | "colgroup" of Text) = !!
  erased given spellcheck: ("spellcheck" is Attribute of Text) = !!
  erased given src: ("src" is Attribute on SrcTags of Text) = !!
  erased given srcdoc: ("srcdoc" is Attribute on "iframe" of Text) = !!
  erased given srclang: ("srclang" is Attribute on "track" of Text) = !!
  erased given srcset: ("srcset" is Attribute on "img" | "source" of Text) = !!
  erased given start: ("start" is Attribute on "ol" of Text) = !!
  erased given step: ("step" is Attribute on "input" of Text) = !!
  erased given style: ("style" is Attribute of Text) = !!
  //erased given summary: ("summary" is Attribute on "table" of Text) = !!
  erased given tabindex: ("tabindex" is Attribute of Text) = !!
  erased given target: ("target" is Attribute on "a" | "area" | "base" | "form" of Text) = !!
  // target
  // target
  erased given title: ("title" is Attribute of Text) = !!
  // title
  // title
  // title
  // title
  erased given translate: ("translate" is Attribute of Text) = !!
  erased given `type`: ("type" is Attribute on TypeTags of Text) = !!
  // type
  // type
  // type
  // type
  // type
  erased given usemap: ("usemaptype" is Attribute on "img" | "input" | "object" of Text) = !!
  erased given value: ("value" is Attribute on ValueTags of Text) = !!
  // value
  // value
  // value
  // value
  //erased given vkp: ("virtualkeyboardpolicy" is Attribute of Text) = !!
  erased given width: ("width" is Attribute on WidthTags of Text) = !!
  erased given wrap: ("wrap" is Attribute on "textarea" of Text) = !!
  erased given writingsuggestions: ("writingsuggestions" is Attribute of Text) = !!


  val elements: Dictionary[Tag] =
    Dictionary(this.membersOfType[Tag].to(Seq).bi.map(_.label -> _)*)

  val entities: Dictionary[Text] =
    val list = cp"/honeycomb/entities.tsv".read[Text].cut(t"\n").map(_.cut(t"\t")).collect:
      case List(key, value) => (key, value)

    Dictionary(list*)
