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

import scala.collection.immutable as sci
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

object Whatwg:
  // Attributes
  type Attribute = honeycomb.Attribute in Whatwg

  private type HeightTags =
    "canvas" | "embed" | "iframe" | "img" | "input" | "object" | "video" | "svg"

  private type CrossoriginTags = "audio" | "img" | "link" | "script" | "video"
  private type MediaTags = "a" | "area" | "link" | "source" | "style"
  private type ReferrerpolicyTags = "a" | "area" | "iframe" | "img" | "link" | "script"
  private type RequiredTags = "input" | "select" | "textarea"

  private type FormTags =
    "button" | "fieldset" | "input" | "object" | "output" | "select" | "textarea"

  private type NameTags =
    "button" | "form" | "fieldset" | "iframe" | "input" | "object" | "output" | "select"
    | "textarea" | "map" | "meta" | "param"

  private type SrcTags =
    "audio" | "embed" | "iframe" | "img" | "input" | "script" | "source" | "track" | "video"

  private type TypeTags =
    "button" | "input" | "embed" | "object" | "ol" | "script" | "source" | "style" | "menu" | "link"

  private type ValueTags =
    "button" | "data" | "input" | "li" | "meter" | "option" | "progress" | "param"

  private type WidthTags =
    "canvas" | "embed" | "iframe" | "img" | "input" | "object" | "video" | "svg"


  def attribute[self  <: Label: ValueOf, plane <: Label: Reifiable to List[String], topic]()
  : self is Attribute on plane of topic in Whatwg =

      new Attribute(valueOf[self].tt, plane.reification().map(_.tt).to(Set), false):
        type Plane = plane
        type Topic = topic
        type Self = self
        type Form = Whatwg

  def globalAttribute[self  <: Label: ValueOf, topic](): self is Attribute of topic in Whatwg =
    new Attribute(valueOf[self].tt, Set(), true):
      type Topic = topic
      type Self = self
      type Form = Whatwg

  given abbr: ("abbr" is Attribute on "th" of Attributive.Textual) = attribute()
  given accept: ("accept" is Attribute on "input" of Attributive.MimeList) = attribute()
  given acceptCharset: ("acceptCharset" is Attribute on "form" of Attributive.Utf8) = attribute()
  given accesskey: ("accesskey" is Attribute of Attributive.AccessKeys) = globalAttribute()
  given action: ("action" is Attribute on "form" of Attributive.Url) = attribute()
  given allow: ("allow" is Attribute on "iframe" of Attributive.PermissionsPolicy) = attribute()
  given allowfullscreen: ("allowfullscreen" is Attribute on "iframe" of Attributive.Presence) = attribute()
  given alpha: ("alpha" is Attribute on "input" of Attributive.Presence) = attribute()
  given alt: ("alt" is Attribute on "area" | "img" | "input" of Attributive.Textual) = attribute()
  given as: ("as" is Attribute on "link" of Attributive.Textual) = attribute()
  given async: ("async" is Attribute on "script" of Attributive.Presence) = attribute()
  given autocapitalize: ("autocapitalize" is Attribute of Attributive.Autocapitalization) = globalAttribute()
  given autocomplete: ("autocomplete" is Attribute on "form" of Attributive.Switch) = attribute()
  given autocomplete2: ("autocomplete" is Attribute on "input" | "select" | "textarea" of Attributive.Switch) = attribute()
  given autocorrect: ("autocorrect" is Attribute of Attributive.Textual) = globalAttribute()
  given autofocus: ("autofocus" is Attribute of Attributive.Presence) = globalAttribute()
  given autoplay: ("autoplay" is Attribute on "audio" | "video" of Attributive.Presence) = attribute()
  given blocking: ("blocking" is Attribute on "link" | "script" | "style" of Attributive.Tokens) = attribute()
  given charset: ("charset" is Attribute on "meta" of Attributive.Utf8) = attribute()
  given checked: ("checked" is Attribute on "input" of Attributive.Presence) = attribute()
  given cite: ("cite" is Attribute on "blockquote" | "del" | "ins" | "q" of Attributive.Url) = attribute()
  given `class`: ("class" is Attribute of Attributive.CssClassList) = globalAttribute()
  given closedby: ("closedby" is Attribute on "dialog" of Attributive.Closedby) = attribute()
  given color: ("color" is Attribute on "link" of Attributive.Color) = attribute()
  given colorspace: ("colorspace" is Attribute on "input" of Attributive.Colorspace) = attribute()
  given cols: ("cols" is Attribute on "textarea" of Attributive.PositiveInt) = attribute()
  given colspan: ("colspan" is Attribute on "td" | "th" of Attributive.PositiveInt) = attribute()
  given command: ("command" is Attribute on "button" of Attributive.Command) = attribute()
  given commandfor: ("commandfor" is Attribute on "button" of Attributive.Id) = attribute()
  given content: ("content" is Attribute on "meta" of Attributive.Textual) = attribute()
  given contenteditable: ("contenteditable" is Attribute of Attributive.ContentEditable) = globalAttribute()
  given controls: ("controls" is Attribute on "audio" | "video" of Attributive.Presence) = attribute()
  given coords: ("coords" is Attribute on "area" of Attributive.Coords) = attribute()
  given crossorigin: ("crossorigin" is Attribute on CrossoriginTags of Attributive.Crossorigin) = attribute()
  given data: ("data" is Attribute on "object" of Attributive.Url) = attribute()
  given datetime: ("datetime" is Attribute on "del" | "ins" of Attributive.Datetime) = attribute()
  given datetime2: ("datetime" is Attribute on "time" of Attributive.Temporal) = attribute()
  given decoding: ("decoding" is Attribute on "img" of Attributive.Decoding) = attribute()
  given default: ("default" is Attribute on "track" of Attributive.Presence) = attribute()
  given defer: ("defer" is Attribute on "script" of Attributive.Presence) = attribute()
  given dir: ("dir" is Attribute of Attributive.Dir) = globalAttribute()
  given dirname: ("dirname" is Attribute on "input" | "textarea" of Attributive.Textual) = attribute()
  given disabled: ("disabled" is Attribute on "button" | "input" | "optgroup" | "option" | "select" | "textarea" of Attributive.Presence) = attribute()
  given disabled2: ("disabled" is Attribute on "fieldset" of Attributive.Presence) = attribute()
  given disabled3: ("disabled" is Attribute on "link" of Attributive.Presence) = attribute()
  given download4: ("download" is Attribute on "a" | "area" of Attributive.Textual) = attribute()
  given draggable: ("draggable" is Attribute of Attributive.Truth) = globalAttribute()
  given enctype: ("enctype" is Attribute on "form" of Attributive.Enctype) = attribute()
  given enterkeyhint: ("enterkeyhint" is Attribute of Attributive.EnterKeyHint) = globalAttribute()
  given fetchpriority: ("fetchpriority" is Attribute on "img" | "link" | "script" of Attributive.FetchPriority) = attribute()
  given `for`: ("for" is Attribute on "label" of Attributive.Id) = attribute()
  given for2: ("for" is Attribute on "output" of Attributive.Tokens) = attribute()
  given form: ("form" is Attribute on FormTags of Attributive.Id) = attribute()
  given formaction: ("formaction" is Attribute on "input" | "button" of Attributive.Url) = attribute()
  given formenctype: ("formenctype" is Attribute on "input" | "button" of Attributive.Enctype) = attribute()
  given formmethod: ("formmethod" is Attribute on "input" | "button" of Attributive.Method) = attribute()
  given formnovalidate: ("formnovalidate" is Attribute on "input" | "button" of Attributive.Presence) = attribute()
  given formtarget: ("formtarget" is Attribute on "input" | "button" of Attributive.Target) = attribute()
  given headers: ("headers" is Attribute on "td" | "th" of Attributive.Tokens) = attribute()
  given headingoffset: ("headingoffset" is Attribute of Attributive.Upto8) = globalAttribute()
  given headingreset: ("headingoffset" is Attribute of Attributive.Presence) = globalAttribute()
  given height: ("height" is Attribute on HeightTags of Attributive.PositiveInt) = attribute()
  given hidden: ("hidden" is Attribute of Attributive.Hidden) = globalAttribute()
  given high: ("high" is Attribute on "meter" of Attributive.Decimal) = attribute()
  given href: ("href" is Attribute on "a" | "area" of Attributive.Url) = attribute()
  given href2: ("href" is Attribute on "base" of Attributive.Url) = attribute()
  given href3: ("href" is Attribute on "link" of Attributive.Url) = attribute()
  given hreflang: ("hreflang" is Attribute on "a" | "link" of Attributive.Language) = attribute()
  given httpEquiv: ("http-equiv" is Attribute on "meta" of Attributive.HttpEquiv) = attribute()
  given id: ("id" is Attribute of Attributive.Id) = globalAttribute()
  given imagesizes: ("imagesizes" is Attribute on "link" of Attributive.ImageSizes) = attribute()
  given imagesrcset: ("imagesrcset" is Attribute on "link" of Attributive.ImageSrcSet) = attribute()
  given inert: ("inert" is Attribute of Attributive.Presence) = globalAttribute()
  given inputmode: ("inputmode" is Attribute of Attributive.InputMode) = globalAttribute()
  given integrity: ("integrity" is Attribute on "link" | "script" of Attributive.Textual) = attribute()
  given is: ("is" is Attribute of Attributive.CustomElementName) = globalAttribute()
  given ismap: ("ismap" is Attribute on "img" of Attributive.Presence) = attribute()
  given itemid: ("itemid" is Attribute of Attributive.Url) = globalAttribute()
  given itemprop: ("itemprop" is Attribute of Attributive.ItemProp) = globalAttribute()
  given itemref: ("itemref" is Attribute of Attributive.Ids) = globalAttribute()
  given itemscope: ("itemscope" is Attribute of Attributive.Presence) = globalAttribute()
  given itemtype: ("itemtype" is Attribute of Attributive.Urls) = globalAttribute()
  given kind: ("kind" is Attribute on "track" of Attributive.Kind) = attribute()
  given label: ("label" is Attribute on "optgroup" | "option" | "track" of Attributive.Textual) = attribute()
  given lang: ("lang" is Attribute of Attributive.Language) = globalAttribute()
  given list: ("list" is Attribute on "input" of Attributive.Id) = attribute()
  given loading: ("loading" is Attribute on "img" | "iframe" of Attributive.Laziness) = attribute()
  given loop: ("loop" is Attribute on "audio" | "video" of Attributive.Presence) = attribute()
  given low: ("low" is Attribute on "meter" of Attributive.Decimal) = attribute()
  given max: ("max" is Attribute on "input" of Attributive.Minmax) = attribute()
  given max2: ("max" is Attribute on "meter" | "progress" of Attributive.Decimal) = attribute()
  given maxlength: ("maxlength" is Attribute on "input" | "textarea" of Attributive.PositiveInt) = attribute()
  given media: ("media" is Attribute on MediaTags of Attributive.MediaQueryList) = attribute()
  given method: ("method" is Attribute on "form" of Attributive.Method) = attribute()
  given min: ("min" is Attribute on "input" of Attributive.Minmax) = attribute()
  given min2: ("min" is Attribute on "meter" of Attributive.Decimal) = attribute()
  given minlength: ("minlength" is Attribute on "input" | "textarea" of Attributive.PositiveInt) = attribute()
  given multiple: ("multiple" is Attribute on "input" | "select" of Attributive.Presence) = attribute()
  given muted: ("muted" is Attribute on "audio" | "video" of Attributive.Presence) = attribute()
  given name: ("name" is Attribute on "button" | "fieldset" | "input" | "output" | "select" | "textarea" of Attributive.Name) = attribute()
  given name2: ("name" is Attribute on "details" of Attributive.Name) = attribute()
  given name3: ("name" is Attribute on "form" of Attributive.Name) = attribute()
  given name4: ("name" is Attribute on "iframe" | "object" of Attributive.Target) = attribute()
  given name5: ("name" is Attribute on "map" of Attributive.Name) = attribute()
  given name6: ("name" is Attribute on "meta" of Attributive.Name) = attribute()
  given name7: ("name" is Attribute on "slot" of Attributive.Name) = attribute()
  given nomodule: ("nomodule" is Attribute on "script" of Attributive.Presence) = attribute()
  given nonce: ("nonce" is Attribute of Attributive.Textual) = globalAttribute()
  given novalidate: ("novalidate" is Attribute on "form" of Attributive.Presence) = attribute()
  given open: ("open" is Attribute on "details" | "dialog" of Attributive.Presence) = attribute()
  given optimum: ("optimum" is Attribute on "meter" of Attributive.Decimal) = attribute()
  given pattern: ("pattern" is Attribute on "input" of Attributive.Regex) = attribute()
  given ping: ("ping" is Attribute on "a" | "area" of Attributive.Urls) = attribute()
  given placeholder: ("placeholder" is Attribute on "input" | "textarea" of Attributive.Textual) = attribute()
  given playsinline: ("playsinline" is Attribute on "video" of Attributive.Presence) = attribute()
  given popover: ("popover" is Attribute of Attributive.Popover) = globalAttribute()
  given popovertarget: ("popovertarget" is Attribute on "button" | "input" of Attributive.Id) = attribute()
  given popovertargetaction: ("popovertargetaction" is Attribute on "button" | "input" of Attributive.PopoverAction) = attribute()
  given poster: ("poster" is Attribute on "video" of Attributive.Url) = attribute()
  given preload: ("preload" is Attribute on "audio" | "video" of Attributive.Preload) = attribute()
  given readonly: ("readonly" is Attribute on "input" | "textarea" of Attributive.Presence) = attribute()
  given referrerpolicy: ("referrerpolicy" is Attribute on ReferrerpolicyTags of Attributive.ReferrerPolicy) = attribute()
  given rel: ("rel" is Attribute on "a" | "area" of Attributive.Tokens) = attribute()
  given rel2: ("rel" is Attribute on "link" of Attributive.Tokens) = attribute()
  given required: ("required" is Attribute on RequiredTags of Attributive.Presence) = attribute()
  given reversed: ("reversed" is Attribute on "ol" of Attributive.Presence) = attribute()
  given rows: ("rows" is Attribute on "textarea" of Attributive.PositiveInt) = attribute()
  given rowspan: ("rowspan" is Attribute on "td" | "th" of Attributive.PositiveInt) = attribute()
  given sandbox: ("sandbox" is Attribute on "iframe" of Attributive.Sandbox) = attribute()
  given scope: ("scope" is Attribute on "th" of Attributive.ThScope) = attribute()
  given selected: ("selected" is Attribute on "option" of Attributive.Presence) = attribute()
  given shadowrootclonable: ("shadowrootclonable" is Attribute on "template" of Attributive.Presence) = attribute()
  given shadowrootcustomelementregistry: ("shadowrootcustomelementregistry" is Attribute on "template" of Attributive.Presence) = attribute()
  given shadowrootdelegatesfocus: ("shadowrootdelegatesfocus" is Attribute on "template" of Attributive.Presence) = attribute()
  given shadowrootmode: ("shadowrootmode" is Attribute on "template" of Attributive.Openness) = attribute()
  given shadowrootserializable: ("shadowrootserializable" is Attribute on "template" of Attributive.Presence) = attribute()
  given shape: ("shape" is Attribute on "area" of Attributive.Shape) = attribute()
  given size: ("size" is Attribute on "input" | "select" of Attributive.PositiveInt) = attribute()
  given sizes: ("sizes" is Attribute on "img" | "source" of Attributive.SourceSizeList) = attribute()
  given sizes2: ("sizes" is Attribute on "link" of Attributive.Sizes) = attribute()
  given slot: ("slot" is Attribute of Attributive.Textual) = globalAttribute()
  given span: ("span" is Attribute on "col" | "colgroup" of Attributive.PositiveInt) = attribute()
  given spellcheck: ("spellcheck" is Attribute of Attributive.Truth) = globalAttribute()
  given src: ("src" is Attribute on SrcTags of Attributive.Url) = attribute()
  given srcdoc: ("srcdoc" is Attribute on "iframe" of Attributive.Srcdoc) = attribute()
  given srclang: ("srclang" is Attribute on "track" of Attributive.Language) = attribute()
  given srcset: ("srcset" is Attribute on "img" | "source" of Attributive.SrcSet) = attribute()
  given start: ("start" is Attribute on "ol" of Attributive.Integral) = attribute()
  given step: ("step" is Attribute on "input" of Attributive.Decimal) = attribute()
  given style: ("style" is Attribute of Attributive.Css) = globalAttribute()
  given tabindex: ("tabindex" is Attribute of Attributive.Integral) = globalAttribute()
  given target: ("target" is Attribute on "a" | "area" of Attributive.Target) = attribute()
  given target2: ("target" is Attribute on "base" of Attributive.Target) = attribute()
  given target3: ("target" is Attribute on "form" of Attributive.Target) = attribute()
  given title: ("title" is Attribute of Attributive.Textual) = globalAttribute()
  given translate: ("translate" is Attribute of Attributive.Affirmation) = globalAttribute()
  given `type`: ("type" is Attribute on "a" | "link" of Attributive.Mime) = attribute()
  given type2: ("type" is Attribute on "button" of Attributive.ButtonType) = attribute()
  given type3: ("type" is Attribute on "embed" | "object" | "source" of Attributive.Mime) = attribute()
  given type4: ("type" is Attribute on "input" of Attributive.InputType) = attribute()
  given type5: ("type" is Attribute on "ol" of Attributive.OlType) = attribute()
  given type6: ("type" is Attribute on "script" of Attributive.ScriptType) = attribute()
  given usemap: ("usemaptype" is Attribute on "img" | "input" | "object" of Attributive.HashName) = attribute()
  given value: ("value" is Attribute on "button" | "option" of Attributive.Textual) = attribute()
  given value2: ("value" is Attribute on "data" of Attributive.Textual) = attribute()
  given value3: ("value" is Attribute on "input" of Attributive.InputValue) = attribute()
  given value4: ("value" is Attribute on "li" of Attributive.Integral) = attribute()
  given value5: ("value" is Attribute on "meter" | "progress" of Attributive.Decimal) = attribute()
  given width: ("width" is Attribute on WidthTags of Attributive.PositiveInt) = attribute()
  given wrap: ("wrap" is Attribute on "textarea" of Attributive.Softness) = attribute()
  given writingsuggestions: ("writingsuggestions" is Attribute of Attributive.Truth) = globalAttribute()

class Whatwg() extends Dom:
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

  val A = Tag.transparent["a", ""]().in[Whatwg]
  val Abbr = Tag.container["abbr", Phrasing]().in[Whatwg]

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
     ().in[Whatwg]

  object Area extends Tag.Void("area", sci.Map()):
    type Topic = "area"
    type Transport = ""
    type Form = Whatwg

    val Default = Tag.void["input"](presets = sci.Map(t"shape" -> t"default")).in[Whatwg]
    val Rect = Tag.void["input"](presets = sci.Map(t"shape" -> t"rect")).in[Whatwg]
    val Circle = Tag.void["input"](presets = sci.Map(t"shape" -> t"circle")).in[Whatwg]
    val Poly = Tag.void["input"](presets = sci.Map(t"shape" -> t"poly")).in[Whatwg]

  val Article = Tag.container["article", Flow]().in[Whatwg]
  val Aside = Tag.container["aside", Flow]().in[Whatwg]

  // - audio and video are prohibited in transparent content
  // - conditions based on presence or absence of `src` attribute
  val Audio = Tag.transparent["audio", "source" | "track"]().in[Whatwg]

  val B = Tag.container["b", Phrasing]().in[Whatwg]

  // - `href` or `target` attributes are required
  val Base = Tag.void["base"]().in[Whatwg]

  val Bdi = Tag.container["bdi", Phrasing]().in[Whatwg]
  val Bdo = Tag.container["bdo", Phrasing]().in[Whatwg]
  val Blockquote = Tag.container["blockquote", Flow]().in[Whatwg]
  val Body = Tag.container["body", Flow](autoclose = true, insertable = true).in[Whatwg]
  val Br = Tag.void["br"]().in[Whatwg]

  // - constraints on content
  val Button = Tag.container["button", Phrasing]().in[Whatwg]

  // - transparent, but non-interactive
  val Canvas = Tag.transparent["canvas", ""]().in[Whatwg]

  val Caption = Tag.container["caption", Flow]().in[Whatwg]
  val Cite = Tag.container["cite", Phrasing]().in[Whatwg]
  val Code = Tag.container["code", Phrasing]().in[Whatwg]
  val Col = Tag.void["col"]().in[Whatwg]

  val Colgroup = Tag.container["colgroup", "col"]
                  (mode = Html.Mode.Whitespace, insertable = true).in[Whatwg]

  val Data = Tag.container["data", Phrasing]().in[Whatwg]
  val Datalist = Tag.container["datalist", Phrasing | "option"]().in[Whatwg]
  val Dd = Tag.container["dd", Flow](autoclose = true).in[Whatwg]
  val Del = Tag.transparent["del", ""]().in[Whatwg]
  val Details = Tag.container["details", "summary" | Flow]().in[Whatwg]
  val Dfn = Tag.container["dfn", Phrasing]().in[Whatwg]
  val Dialog = Tag.container["dialog", Flow]().in[Whatwg]
  val Div = Tag.container["div", Flow]().in[Whatwg]

  val Dl = Tag.container["dl", "div" | "dt" | ScriptSupporting]
            (autoclose = true, mode = Html.Mode.Whitespace).in[Whatwg]

  val Dt = Tag.container["dl", Flow](autoclose = true).in[Whatwg]
  val Em = Tag.container["em", Phrasing]().in[Whatwg]
  val Embed = Tag.void["embed"]().in[Whatwg]
  val Fieldset = Tag.container["fieldset", "legend" | Flow]().in[Whatwg]
  val Figcaption = Tag.container["figcaption", Flow]().in[Whatwg]
  val Figure = Tag.container["figure", "figcaption" | Flow]().in[Whatwg]
  val Footer = Tag.container["footer", Flow]().in[Whatwg]
  val Form = Tag.container["form", Flow]().in[Whatwg]
  val H1 = Tag.container["h1", Phrasing]().in[Whatwg]
  val H2 = Tag.container["h2", Phrasing]().in[Whatwg]
  val H3 = Tag.container["h3", Phrasing]().in[Whatwg]
  val H4 = Tag.container["h4", Phrasing]().in[Whatwg]
  val H5 = Tag.container["h5", Phrasing]().in[Whatwg]
  val H6 = Tag.container["h6", Phrasing]().in[Whatwg]

  val Head =
    Tag.container["head", Metadata]
     (autoclose = true, mode = Html.Mode.Whitespace, insertable = true)
    .in[Whatwg]

  val Header = Tag.container["header", Flow](autoclose = true).in[Whatwg]
  val Hgroup = Tag.container["hgroup", "p" | "h1" | "h2" | "h3" | "h4" | "h5" | "h6"]().in[Whatwg]
  val Hr = Tag.void["hr"]().in[Whatwg]
  lazy val Html = honeycomb.Html.in[Whatwg]
  val I = Tag.container["i", Phrasing]().in[Whatwg]
  val Iframe = Tag.void["iframe"]().in[Whatwg]
  val Img = Tag.void["img"]().in[Whatwg]

  object Input extends Tag.Void("input", sci.Map()):
    type Topic = "input"
    type Transport = ""
    type Form = Whatwg

    val Hidden = Tag.void["input"](presets = sci.Map(t"type" -> t"hidden")).in[Whatwg]
    val Text = Tag.void["input"](presets = sci.Map(t"type" -> t"text")).in[Whatwg]
    val Search = Tag.void["input"](presets = sci.Map(t"type" -> t"search")).in[Whatwg]
    val Tel = Tag.void["input"](presets = sci.Map(t"type" -> t"tel")).in[Whatwg]
    val Url = Tag.void["input"](presets = sci.Map(t"type" -> t"url")).in[Whatwg]
    val Email = Tag.void["input"](presets = sci.Map(t"type" -> t"email")).in[Whatwg]
    val Password = Tag.void["input"](presets = sci.Map(t"type" -> t"password")).in[Whatwg]
    val Date = Tag.void["input"](presets = sci.Map(t"type" -> t"date")).in[Whatwg]
    val Month = Tag.void["input"](presets = sci.Map(t"type" -> t"month")).in[Whatwg]
    val Week = Tag.void["input"](presets = sci.Map(t"type" -> t"week")).in[Whatwg]
    val Time = Tag.void["input"](presets = sci.Map(t"type" -> t"time")).in[Whatwg]

    val DatetimeLocal =
      Tag.void["input"](presets = sci.Map(t"type" -> t"datetime-local")).in[Whatwg]

    val Number = Tag.void["input"](presets = sci.Map(t"type" -> t"number")).in[Whatwg]
    val Range = Tag.void["input"](presets = sci.Map(t"type" -> t"range")).in[Whatwg]
    val Color = Tag.void["input"](presets = sci.Map(t"type" -> t"color")).in[Whatwg]
    val Checkbox = Tag.void["input"](presets = sci.Map(t"type" -> t"checkbox")).in[Whatwg]
    val Radio = Tag.void["input"](presets = sci.Map(t"type" -> t"radio")).in[Whatwg]
    val Submit = Tag.void["input"](presets = sci.Map(t"type" -> t"submit")).in[Whatwg]
    val Image = Tag.void["input"](presets = sci.Map(t"type" -> t"image")).in[Whatwg]
    val Reset = Tag.void["input"](presets = sci.Map(t"type" -> t"reset")).in[Whatwg]
    val Button = Tag.void["input"](presets = sci.Map(t"type" -> t"button")).in[Whatwg]

  val Ins = Tag.transparent["ins", ""]().in[Whatwg]
  val Kbd = Tag.container["kbd", Phrasing]().in[Whatwg]
  val Label = Tag.container["label", Phrasing]().in[Whatwg]

  val Legend =
    Tag.container["label", Phrasing | "h1" | "h2" | "h3" | "h4" | "h5" | "h6"]().in[Whatwg]

  val Li = Tag.container["li", Flow](autoclose = true).in[Whatwg]

  object Link extends Tag.Void("link", sci.Map()):
    type Topic = "link"
    type Transport = ""
    type Form = Whatwg

    val Alternate = Tag.void["input"](presets = sci.Map(t"rel" -> t"alternate")).in[Whatwg]
    val Canonical = Tag.void["input"](presets = sci.Map(t"rel" -> t"canonical")).in[Whatwg]
    val Author = Tag.void["input"](presets = sci.Map(t"rel" -> t"author")).in[Whatwg]
    val DnsPrefetch = Tag.void["input"](presets = sci.Map(t"rel" -> t"dns-prefetch")).in[Whatwg]
    val Expect = Tag.void["input"](presets = sci.Map(t"rel" -> t"expect")).in[Whatwg]
    val Help = Tag.void["input"](presets = sci.Map(t"rel" -> t"help")).in[Whatwg]
    val Icon = Tag.void["input"](presets = sci.Map(t"rel" -> t"icon")).in[Whatwg]
    val Manifest = Tag.void["input"](presets = sci.Map(t"rel" -> t"manifest")).in[Whatwg]
    val Modulepreload = Tag.void["input"](presets = sci.Map(t"rel" -> t"modulepreload")).in[Whatwg]
    val License = Tag.void["input"](presets = sci.Map(t"rel" -> t"license")).in[Whatwg]
    val Next = Tag.void["input"](presets = sci.Map(t"rel" -> t"next")).in[Whatwg]
    val Pingback = Tag.void["input"](presets = sci.Map(t"rel" -> t"pingback")).in[Whatwg]
    val Preconnect = Tag.void["input"](presets = sci.Map(t"rel" -> t"preconnect")).in[Whatwg]
    val Prefetch = Tag.void["input"](presets = sci.Map(t"rel" -> t"prefetch")).in[Whatwg]
    val Preload = Tag.void["input"](presets = sci.Map(t"rel" -> t"preload")).in[Whatwg]
    val Prev = Tag.void["input"](presets = sci.Map(t"rel" -> t"prev")).in[Whatwg]
    val PrivacyPolicy = Tag.void["input"](presets = sci.Map(t"rel" -> t"privacy-policy")).in[Whatwg]
    val Search = Tag.void["input"](presets = sci.Map(t"rel" -> t"search")).in[Whatwg]
    val Stylesheet = Tag.void["input"](presets = sci.Map(t"rel" -> t"stylesheet")).in[Whatwg]

    val TermsOfService =
      Tag.void["input"](presets = sci.Map(t"rel" -> t"terms-of-service")).in[Whatwg]

  val Main = Tag.container["main", Flow]().in[Whatwg]
  val Map = Tag.transparent["map", "area"]().in[Whatwg]
  val Mark = Tag.container["mark", Phrasing]().in[Whatwg]
  val Math = Tag.foreign["math"]().in[Whatwg]
  val Menu = Tag.container["menu", "li" | ScriptSupporting](mode = Html.Mode.Whitespace).in[Whatwg]
  val Meta = Tag.void["meta"]().in[Whatwg]
  val Meter = Tag.container["meter", Phrasing]().in[Whatwg]
  val Nav = Tag.container["nav", Flow]().in[Whatwg]
  val Noscript = Tag.container["noscript", "link" | "style" | "meta"]().in[Whatwg]
  val Object = Tag.transparent["object", ""]().in[Whatwg]
  val Ol = Tag.container["ol", "li" | ScriptSupporting](mode = Html.Mode.Whitespace).in[Whatwg]

  val Optgroup = Tag.container["optgroup", "option" | "legend"]
                  (autoclose = true, mode = Html.Mode.Whitespace).in[Whatwg]

  val Option = Tag.container["option", "#text"](autoclose = true).in[Whatwg]
  val Output = Tag.container["output", Phrasing]().in[Whatwg]
  val P = Tag.container["p", Phrasing](autoclose = true).in[Whatwg]

  val Picture = Tag.container["picture", "source" | "img" | ScriptSupporting]
                 (mode = Html.Mode.Whitespace).in[Whatwg]

  val Pre = Tag.container["pre", Phrasing]().in[Whatwg]
  val Progress = Tag.container["progress", Phrasing]().in[Whatwg]
  val Q = Tag.container["q", Phrasing]().in[Whatwg]
  val Rp = Tag.container["rp", "#text"](autoclose = true).in[Whatwg]
  val Rt = Tag.container["rt", Phrasing](autoclose = true).in[Whatwg]
  val Ruby = Tag.container["ruby", Phrasing | "rt" | "rp"]().in[Whatwg]
  val S = Tag.container["s", Phrasing]().in[Whatwg]
  val Samp = Tag.container["samp", Phrasing]().in[Whatwg]
  val Script = Tag.container["script", "#text"](mode = Html.Mode.Raw).in[Whatwg]
  val Search = Tag.container["search", Flow]().in[Whatwg]
  val Section = Tag.container["section", Flow]().in[Whatwg]

  val Select =
    Tag.container
     ["select", "option" | "optgroup" | "hr" | "button" | "noscript" | ScriptSupporting]
     (mode = Html.Mode.Whitespace).in[Whatwg]

  val Selectedcontent = Tag.void["selectedcontent"]().in[Whatwg]
  val Slot = Tag.transparent["slot", ""]().in[Whatwg]
  val Small = Tag.container["small", Phrasing]().in[Whatwg]
  val Source = Tag.void["source"]().in[Whatwg]
  val Span = Tag.container["span", Phrasing]().in[Whatwg]
  val Strong = Tag.container["strong", Phrasing]().in[Whatwg]
  val Style = Tag.container["style", "#text"](mode = Html.Mode.Raw).in[Whatwg]
  val Sub = Tag.container["sub", Phrasing]().in[Whatwg]
  val Summary = Tag.container["summary", Phrasing | Heading]().in[Whatwg]
  val Sup = Tag.container["sup", Phrasing]().in[Whatwg]
  val Svg = Tag.foreign["svg"]().in[Whatwg]

  val Table =
    Tag.container["table", "caption" | "colgroup" | "thead" | "tbody" | "tfoot"]
     (mode = Html.Mode.Whitespace).in[Whatwg]

  val Tbody = Tag.container["tbody", "tr"]
               (autoclose = true, mode = Html.Mode.Whitespace, insertable = true).in[Whatwg]

  val Td = Tag.container["td", Flow](autoclose = true).in[Whatwg]
  val Template = Tag.void["template"]().in[Whatwg]
  val Textarea = Tag.container["textarea", "#text"](mode = Html.Mode.Rcdata).in[Whatwg]

  val Tfoot = Tag.container["tfoot", "tr"]
               (autoclose = true, mode = Html.Mode.Whitespace).in[Whatwg]

  val Th = Tag.container["th", Flow](autoclose = true).in[Whatwg]

  val Thead = Tag.container["thead", "tr" | ScriptSupporting]
               (autoclose = true, mode = Html.Mode.Whitespace).in[Whatwg]

  val Time = Tag.container["time", Phrasing]().in[Whatwg]
  val Title = Tag.container["title", "#text"](mode = Html.Mode.Rcdata).in[Whatwg]

  val Tr = Tag.container["tr", "td" | "th" | ScriptSupporting]
            (autoclose = true, mode = Html.Mode.Whitespace, insertable = true).in[Whatwg]

  object Track extends Tag.Void("track", sci.Map()):
    type Topic = "track"
    type Transport = ""
    type Form = Whatwg

    val Captions = Tag.void["track"](presets = sci.Map(t"kind" -> t"captions"))
    val Chapters = Tag.void["track"](presets = sci.Map(t"kind" -> t"chapters"))
    val Descriptions = Tag.void["track"](presets = sci.Map(t"kind" -> t"descriptions"))
    val Metadata = Tag.void["track"](presets = sci.Map(t"kind" -> t"metadata"))
    val Subtitles = Tag.void["track"](presets = sci.Map(t"kind" -> t"subtitles"))

  val U = Tag.container["u", Phrasing]().in[Whatwg]
  val Ul = Tag.container["ul", "li" | ScriptSupporting](mode = Html.Mode.Whitespace).in[Whatwg]
  val Var = Tag.container["var", Phrasing]().in[Whatwg]
  val Video = Tag.transparent["video", "track" | "source"]().in[Whatwg]
  val Wbr = Tag.void["wbr"]().in[Whatwg]


  val elements: Dictionary[Tag] =
    Dictionary(this.membersOfType[Tag].to(Seq).bi.map(_.label -> _)*)

  val entities: Dictionary[Text] =
    val list = cp"/honeycomb/entities.tsv".read[Text].cut(t"\n").map(_.cut(t"\t")).collect:
      case List(key, value) => (key, value)

    Dictionary(list*)

  val attributes: Dictionary[Attribute] =
    val list: List[(Text, Attribute)] =
      Whatwg.membersOfType[honeycomb.Attribute]
      . foldLeft(sci.Map[Text, Attribute]()): (map, next) =>
          map.updated(next.label, map.at(next.label).let(_.merge(next)).or(next))
      . to(List)

    Dictionary(list*)
