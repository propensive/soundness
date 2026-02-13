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
  // Attribute types
  erased trait AccessKeys
  erased trait Affirmation
  erased trait Autocapitalization
  erased trait ButtonType
  erased trait Closedby
  erased trait Color
  erased trait Colorspace
  erased trait Command
  erased trait ContentEditable
  erased trait Coords
  erased trait Crossorigin
  erased trait CustomElementName
  erased trait Css
  erased trait CssClassList
  erased trait Datetime
  erased trait Decimal
  erased trait Decoding
  erased trait Dir
  erased trait Enctype
  erased trait EnterKeyHint
  erased trait FetchPriority
  erased trait HashName
  erased trait Hidden
  erased trait HttpEquiv
  erased trait Id
  erased trait Ids
  erased trait ImageSizes
  erased trait ImageSrcSet
  erased trait InputMode
  erased trait InputType
  erased trait InputValue
  erased trait Integral
  erased trait ItemProp
  erased trait Kind
  erased trait Language
  erased trait Laziness
  erased trait MediaQueryList
  erased trait Method
  erased trait Minmax
  erased trait MimeList
  erased trait Mime
  erased trait Name
  erased trait OlType
  erased trait Openness
  erased trait PermissionsPolicy
  erased trait Popover
  erased trait PopoverAction
  erased trait PositiveInt
  erased trait Presence
  erased trait Preload
  erased trait ReferrerPolicy
  erased trait Regex
  erased trait Sandbox
  erased trait ScriptType
  erased trait Shape
  erased trait Sizes
  erased trait Softness
  erased trait SourceSizeList
  erased trait Srcdoc
  erased trait SrcSet
  erased trait Switch
  erased trait Target
  erased trait Temporal
  erased trait Textual
  erased trait ThScope
  erased trait Tokens
  erased trait Truth
  erased trait Upto8
  erased trait Url
  erased trait Urls
  erased trait Utf8

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

      new Attribute(valueOf[self].tt, plane.reify.map(_.tt).to(Set), false):
        type Plane = plane
        type Topic = topic
        type Self = self
        type Form = Whatwg

  def globalAttribute[self  <: Label: ValueOf, topic](): self is Attribute of topic in Whatwg =
    new Attribute(valueOf[self].tt, Set(), true):
      type Topic = topic
      type Self = self
      type Form = Whatwg

  given abbr: ("abbr" is Attribute on "th" of Textual) = attribute()
  given accept: ("accept" is Attribute on "input" of MimeList) = attribute()
  given acceptCharset: ("acceptCharset" is Attribute on "form" of Utf8) = attribute()
  given accesskey: ("accesskey" is Attribute of AccessKeys) = globalAttribute()
  given action: ("action" is Attribute on "form" of Url) = attribute()
  given allow: ("allow" is Attribute on "iframe" of PermissionsPolicy) = attribute()
  given allowfullscreen: ("allowfullscreen" is Attribute on "iframe" of Presence) = attribute()
  given alpha: ("alpha" is Attribute on "input" of Presence) = attribute()
  given alt: ("alt" is Attribute on "area" | "img" | "input" of Textual) = attribute()
  given as: ("as" is Attribute on "link" of Textual) = attribute()
  given async: ("async" is Attribute on "script" of Presence) = attribute()
  given autocapitalize: ("autocapitalize" is Attribute of Autocapitalization) = globalAttribute()
  given autocomplete: ("autocomplete" is Attribute on "form" of Switch) = attribute()

  given autocomplete2: ("autocomplete" is Attribute on "input" | "select" | "textarea" of Switch) =
    attribute()

  given autocorrect: ("autocorrect" is Attribute of Textual) = globalAttribute()
  given autofocus: ("autofocus" is Attribute of Presence) = globalAttribute()
  given autoplay: ("autoplay" is Attribute on "audio" | "video" of Presence) = attribute()
  given blocking: ("blocking" is Attribute on "link" | "script" | "style" of Tokens) = attribute()
  given charset: ("charset" is Attribute on "meta" of Utf8) = attribute()
  given checked: ("checked" is Attribute on "input" of Presence) = attribute()
  given cite: ("cite" is Attribute on "blockquote" | "del" | "ins" | "q" of Url) = attribute()
  given `class`: ("class" is Attribute of CssClassList) = globalAttribute()
  given closedby: ("closedby" is Attribute on "dialog" of Closedby) = attribute()
  given color: ("color" is Attribute on "link" of Color) = attribute()
  given colorspace: ("colorspace" is Attribute on "input" of Colorspace) = attribute()
  given cols: ("cols" is Attribute on "textarea" of PositiveInt) = attribute()
  given colspan: ("colspan" is Attribute on "td" | "th" of PositiveInt) = attribute()
  given command: ("command" is Attribute on "button" of Command) = attribute()
  given commandfor: ("commandfor" is Attribute on "button" of Id) = attribute()
  given content: ("content" is Attribute on "meta" of Textual) = attribute()
  given contenteditable: ("contenteditable" is Attribute of ContentEditable) = globalAttribute()
  given controls: ("controls" is Attribute on "audio" | "video" of Presence) = attribute()
  given coords: ("coords" is Attribute on "area" of Coords) = attribute()
  given crossorigin: ("crossorigin" is Attribute on CrossoriginTags of Crossorigin) = attribute()
  given data: ("data" is Attribute on "object" of Url) = attribute()
  given datetime: ("datetime" is Attribute on "del" | "ins" of Datetime) = attribute()
  given datetime2: ("datetime" is Attribute on "time" of Temporal) = attribute()
  given decoding: ("decoding" is Attribute on "img" of Decoding) = attribute()
  given default: ("default" is Attribute on "track" of Presence) = attribute()
  given defer: ("defer" is Attribute on "script" of Presence) = attribute()
  given dir: ("dir" is Attribute of Dir) = globalAttribute()
  given dirname: ("dirname" is Attribute on "input" | "textarea" of Textual) = attribute()

  private type Disableable = "button" | "input" | "optgroup" | "option" | "select" | "textarea"
  given disabled: ("disabled" is Attribute on Disableable of Presence) = attribute()
  given disabled2: ("disabled" is Attribute on "fieldset" of Presence) = attribute()
  given disabled3: ("disabled" is Attribute on "link" of Presence) = attribute()
  given download4: ("download" is Attribute on "a" | "area" of Textual) = attribute()
  given draggable: ("draggable" is Attribute of Truth) = globalAttribute()
  given enctype: ("enctype" is Attribute on "form" of Enctype) = attribute()
  given enterkeyhint: ("enterkeyhint" is Attribute of EnterKeyHint) = globalAttribute()

  private type FetchPrioritizable = "img" | "link" | "script"
  given fetchpriority: ("fetchpriority" is Attribute on FetchPrioritizable of FetchPriority) =
    attribute()

  given `for`: ("for" is Attribute on "label" of Id) = attribute()
  given for2: ("for" is Attribute on "output" of Tokens) = attribute()
  given form: ("form" is Attribute on FormTags of Id) = attribute()
  given formaction: ("formaction" is Attribute on "input" | "button" of Url) = attribute()
  given formenctype: ("formenctype" is Attribute on "input" | "button" of Enctype) = attribute()
  given formmethod: ("formmethod" is Attribute on "input" | "button" of Method) = attribute()

  given formnovalidate: ("formnovalidate" is Attribute on "input" | "button" of Presence) =
    attribute()

  given formtarget: ("formtarget" is Attribute on "input" | "button" of Target) = attribute()
  given headers: ("headers" is Attribute on "td" | "th" of Tokens) = attribute()
  given headingoffset: ("headingoffset" is Attribute of Upto8) = globalAttribute()
  given headingreset: ("headingoffset" is Attribute of Presence) = globalAttribute()
  given height: ("height" is Attribute on HeightTags of PositiveInt) = attribute()
  given hidden: ("hidden" is Attribute of Hidden) = globalAttribute()
  given high: ("high" is Attribute on "meter" of Decimal) = attribute()
  given href: ("href" is Attribute on "a" | "area" of Url) = attribute()
  given href2: ("href" is Attribute on "base" of Url) = attribute()
  given href3: ("href" is Attribute on "link" of Url) = attribute()
  given hreflang: ("hreflang" is Attribute on "a" | "link" of Language) = attribute()
  given httpEquiv: ("http-equiv" is Attribute on "meta" of HttpEquiv) = attribute()
  given id: ("id" is Attribute of Id) = globalAttribute()
  given imagesizes: ("imagesizes" is Attribute on "link" of ImageSizes) = attribute()
  given imagesrcset: ("imagesrcset" is Attribute on "link" of ImageSrcSet) = attribute()
  given inert: ("inert" is Attribute of Presence) = globalAttribute()
  given inputmode: ("inputmode" is Attribute of InputMode) = globalAttribute()
  given integrity: ("integrity" is Attribute on "link" | "script" of Textual) = attribute()
  given is: ("is" is Attribute of CustomElementName) = globalAttribute()
  given ismap: ("ismap" is Attribute on "img" of Presence) = attribute()
  given itemid: ("itemid" is Attribute of Url) = globalAttribute()
  given itemprop: ("itemprop" is Attribute of ItemProp) = globalAttribute()
  given itemref: ("itemref" is Attribute of Ids) = globalAttribute()
  given itemscope: ("itemscope" is Attribute of Presence) = globalAttribute()
  given itemtype: ("itemtype" is Attribute of Urls) = globalAttribute()
  given kind: ("kind" is Attribute on "track" of Kind) = attribute()
  given label: ("label" is Attribute on "optgroup" | "option" | "track" of Textual) = attribute()
  given lang: ("lang" is Attribute of Language) = globalAttribute()
  given list: ("list" is Attribute on "input" of Id) = attribute()
  given loading: ("loading" is Attribute on "img" | "iframe" of Laziness) = attribute()
  given loop: ("loop" is Attribute on "audio" | "video" of Presence) = attribute()
  given low: ("low" is Attribute on "meter" of Decimal) = attribute()
  given max: ("max" is Attribute on "input" of Minmax) = attribute()
  given max2: ("max" is Attribute on "meter" | "progress" of Decimal) = attribute()
  given maxlength: ("maxlength" is Attribute on "input" | "textarea" of PositiveInt) = attribute()
  given media: ("media" is Attribute on MediaTags of MediaQueryList) = attribute()
  given method: ("method" is Attribute on "form" of Method) = attribute()
  given min: ("min" is Attribute on "input" of Minmax) = attribute()
  given min2: ("min" is Attribute on "meter" of Decimal) = attribute()
  given minlength: ("minlength" is Attribute on "input" | "textarea" of PositiveInt) = attribute()
  given multiple: ("multiple" is Attribute on "input" | "select" of Presence) = attribute()
  given muted: ("muted" is Attribute on "audio" | "video" of Presence) = attribute()

  private type NameElements = "button" | "fieldset" | "input" | "output" | "select" | "textarea"
  given name: ("name" is Attribute on NameElements of Name) = attribute()
  given name2: ("name" is Attribute on "details" of Name) = attribute()
  given name3: ("name" is Attribute on "form" of Name) = attribute()
  given name4: ("name" is Attribute on "iframe" | "object" of Target) = attribute()
  given name5: ("name" is Attribute on "map" of Name) = attribute()
  given name6: ("name" is Attribute on "meta" of Name) = attribute()
  given name7: ("name" is Attribute on "slot" of Name) = attribute()
  given nomodule: ("nomodule" is Attribute on "script" of Presence) = attribute()
  given nonce: ("nonce" is Attribute of Textual) = globalAttribute()
  given novalidate: ("novalidate" is Attribute on "form" of Presence) = attribute()
  given open: ("open" is Attribute on "details" | "dialog" of Presence) = attribute()
  given optimum: ("optimum" is Attribute on "meter" of Decimal) = attribute()
  given pattern: ("pattern" is Attribute on "input" of Regex) = attribute()
  given ping: ("ping" is Attribute on "a" | "area" of Urls) = attribute()
  given placeholder: ("placeholder" is Attribute on "input" | "textarea" of Textual) = attribute()
  given playsinline: ("playsinline" is Attribute on "video" of Presence) = attribute()
  given popover: ("popover" is Attribute of Popover) = globalAttribute()
  given popovertarget: ("popovertarget" is Attribute on "button" | "input" of Id) = attribute()

  given pta: ("popovertargetaction" is Attribute on "button" | "input" of PopoverAction) =
    attribute()

  given poster: ("poster" is Attribute on "video" of Url) = attribute()
  given preload: ("preload" is Attribute on "audio" | "video" of Preload) = attribute()
  given readonly: ("readonly" is Attribute on "input" | "textarea" of Presence) = attribute()

  given referrerpolicy: ("referrerpolicy" is Attribute on ReferrerpolicyTags of ReferrerPolicy) =
    attribute()

  given rel: ("rel" is Attribute on "a" | "area" of Tokens) = attribute()
  given rel2: ("rel" is Attribute on "link" of Tokens) = attribute()
  given required: ("required" is Attribute on RequiredTags of Presence) = attribute()
  given reversed: ("reversed" is Attribute on "ol" of Presence) = attribute()
  given rows: ("rows" is Attribute on "textarea" of PositiveInt) = attribute()
  given rowspan: ("rowspan" is Attribute on "td" | "th" of PositiveInt) = attribute()
  given sandbox: ("sandbox" is Attribute on "iframe" of Sandbox) = attribute()
  given scope: ("scope" is Attribute on "th" of ThScope) = attribute()
  given selected: ("selected" is Attribute on "option" of Presence) = attribute()

  given shadowrootclonable: ("shadowrootclonable" is Attribute on "template" of Presence) =
    attribute()

  given srcer: ("shadowrootcustomelementregistry" is Attribute on "template" of Presence) =
    attribute()

  given srdf: ("shadowrootdelegatesfocus" is Attribute on "template" of Presence) = attribute()
  given shadowrootmode: ("shadowrootmode" is Attribute on "template" of Openness) = attribute()

  given shadowrootserializable: ("shadowrootserializable" is Attribute on "template" of Presence) =
    attribute()

  given shape: ("shape" is Attribute on "area" of Shape) = attribute()
  given size: ("size" is Attribute on "input" | "select" of PositiveInt) = attribute()
  given sizes: ("sizes" is Attribute on "img" | "source" of SourceSizeList) = attribute()
  given sizes2: ("sizes" is Attribute on "link" of Sizes) = attribute()
  given slot: ("slot" is Attribute of Textual) = globalAttribute()
  given span: ("span" is Attribute on "col" | "colgroup" of PositiveInt) = attribute()
  given spellcheck: ("spellcheck" is Attribute of Truth) = globalAttribute()
  given src: ("src" is Attribute on SrcTags of Url) = attribute()
  given srcdoc: ("srcdoc" is Attribute on "iframe" of Srcdoc) = attribute()
  given srclang: ("srclang" is Attribute on "track" of Language) = attribute()
  given srcset: ("srcset" is Attribute on "img" | "source" of SrcSet) = attribute()
  given start: ("start" is Attribute on "ol" of Integral) = attribute()
  given step: ("step" is Attribute on "input" of Decimal) = attribute()
  given style: ("style" is Attribute of Css) = globalAttribute()
  given tabindex: ("tabindex" is Attribute of Integral) = globalAttribute()
  given target: ("target" is Attribute on "a" | "area" of Target) = attribute()
  given target2: ("target" is Attribute on "base" of Target) = attribute()
  given target3: ("target" is Attribute on "form" of Target) = attribute()
  given title: ("title" is Attribute of Textual) = globalAttribute()
  given translate: ("translate" is Attribute of Affirmation) = globalAttribute()
  given `type`: ("type" is Attribute on "a" | "link" of Mime) = attribute()
  given type2: ("type" is Attribute on "button" of ButtonType) = attribute()
  given type3: ("type" is Attribute on "embed" | "object" | "source" of Mime) = attribute()
  given type4: ("type" is Attribute on "input" of InputType) = attribute()
  given type5: ("type" is Attribute on "ol" of OlType) = attribute()
  given type6: ("type" is Attribute on "script" of ScriptType) = attribute()
  given usemap: ("usemaptype" is Attribute on "img" | "input" | "object" of HashName) = attribute()
  given value: ("value" is Attribute on "button" | "option" of Textual) = attribute()
  given value2: ("value" is Attribute on "data" of Textual) = attribute()
  given value3: ("value" is Attribute on "input" of InputValue) = attribute()
  given value4: ("value" is Attribute on "li" of Integral) = attribute()
  given value5: ("value" is Attribute on "meter" | "progress" of Decimal) = attribute()
  given width: ("width" is Attribute on WidthTags of PositiveInt) = attribute()
  given wrap: ("wrap" is Attribute on "textarea" of Softness) = attribute()
  given writingsuggestions: ("writingsuggestions" is Attribute of Truth) = globalAttribute()

class Whatwg() extends Dom:
  def doctype: Doctype = Doctype("html")

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

  val A = Tag.transparent["a", "", Whatwg]()
  val Abbr = Tag.container["abbr", Phrasing, Whatwg]()

  val Address =
    Tag.container
      [ "address",
        "a" | "abbr" | "area" | "audio" | "b" | "bdi" | "bdo" | "blockquote" | "br" | "button"
        | "canvas" | "cite" | "code" | "data" | "datalist" | "del" | "details" | "dfn" | "dialog"
        | "div" | "dl" | "em" | "embed" | "fieldset" | "figure" | "form" | "hr" | "i" | "iframe"
        | "img" | "input" | "ins" | "kbd" | "label" | "link" | "main" | "map" | "mark" | "menu"
        | "meta" | "meter" | "noscript" | "object" | "ol" | "output" | "p" | "picture" | "pre"
        | "progress" | "q" | "ruby" | "s" | "samp" | "script" | "select" | "slot" | "small" | "span"
        | "strong" | "sub" | "sup" | "table" | "template" | "textarea" | "time" | "u" | "ul" | "var"
        | "video" | "wbr", Whatwg ]
          ( )

  object Area extends Tag.Void("area", sci.Map(), false):
    type Topic = "area"
    type Transport = ""
    type Form = Whatwg

    val Default = Tag.void["input", Whatwg](presets = sci.Map(t"shape" -> t"default"))
    val Rect = Tag.void["input", Whatwg](presets = sci.Map(t"shape" -> t"rect"))
    val Circle = Tag.void["input", Whatwg](presets = sci.Map(t"shape" -> t"circle"))
    val Poly = Tag.void["input", Whatwg](presets = sci.Map(t"shape" -> t"poly"))

  val Article = Tag.container["article", Flow, Whatwg]()
  val Aside = Tag.container["aside", Flow, Whatwg]()

  // - audio and video are prohibited in transparent content
  // - conditions based on presence or absence of `src` attribute
  val Audio = Tag.transparent["audio", "source" | "track", Whatwg]()

  val B = Tag.container["b", Phrasing, Whatwg]()

  // - `href` or `target` attributes are required
  val Base = Tag.void["base", Whatwg]()

  val Bdi = Tag.container["bdi", Phrasing, Whatwg]()
  val Bdo = Tag.container["bdo", Phrasing, Whatwg]()
  val Blockquote = Tag.container["blockquote", Flow, Whatwg]()
  val Body = Tag.container["body", Flow, Whatwg](autoclose = true, insertable = true)
  val Br = Tag.void["br", Whatwg]()

  // - constraints on content
  val Button = Tag.container["button", Phrasing, Whatwg](boundary = true)

  // - transparent, but non-interactive
  val Canvas = Tag.transparent["canvas", "", Whatwg]()

  val Caption = Tag.container["caption", Flow, Whatwg](boundary = true)
  val Cite = Tag.container["cite", Phrasing, Whatwg]()
  val Code = Tag.container["code", Phrasing, Whatwg]()
  val Col = Tag.void["col", Whatwg]()

  val Colgroup = Tag.container["colgroup", "col", Whatwg]
                  (mode = Html.Mode.Whitespace, insertable = true)

  val Data = Tag.container["data", Phrasing, Whatwg]()
  val Datalist = Tag.container["datalist", Phrasing | "option", Whatwg]()
  val Dd = Tag.container["dd", Flow, Whatwg](autoclose = true)
  val Del = Tag.transparent["del", "", Whatwg]()
  val Details = Tag.container["details", "summary" | Flow, Whatwg]()
  val Dfn = Tag.container["dfn", Phrasing, Whatwg]()
  val Dialog = Tag.container["dialog", Flow, Whatwg]()
  val Div = Tag.container["div", Flow, Whatwg]()

  val Dl = Tag.container["dl", "div" | "dt" | ScriptSupporting, Whatwg]
            (autoclose = true, mode = Html.Mode.Whitespace)

  val Dt = Tag.container["dl", Flow, Whatwg](autoclose = true)
  val Em = Tag.container["em", Phrasing, Whatwg]()
  val Embed = Tag.void["embed", Whatwg]()
  val Fieldset = Tag.container["fieldset", "legend" | Flow, Whatwg]()
  val Figcaption = Tag.container["figcaption", Flow, Whatwg]()
  val Figure = Tag.container["figure", "figcaption" | Flow, Whatwg]()
  val Footer = Tag.container["footer", Flow, Whatwg]()
  val Form = Tag.container["form", Flow, Whatwg]()
  val H1 = Tag.container["h1", Phrasing, Whatwg]()
  val H2 = Tag.container["h2", Phrasing, Whatwg]()
  val H3 = Tag.container["h3", Phrasing, Whatwg]()
  val H4 = Tag.container["h4", Phrasing, Whatwg]()
  val H5 = Tag.container["h5", Phrasing, Whatwg]()
  val H6 = Tag.container["h6", Phrasing, Whatwg]()

  val Head =
    Tag.container["head", Metadata, Whatwg]
      ( autoclose = true, mode = Html.Mode.Whitespace, insertable = true )

  val Header = Tag.container["header", Flow, Whatwg](autoclose = true)
  val Hgroup = Tag.container["hgroup", "p" | "h1" | "h2" | "h3" | "h4" | "h5" | "h6", Whatwg]()
  val Hr = Tag.void["hr", Whatwg]()
  lazy val Html = honeycomb.Html
  val I = Tag.container["i", Phrasing, Whatwg]()
  val Iframe = Tag.void["iframe", Whatwg]()
  val Img = Tag.void["img", Whatwg]()

  object Input extends Tag.Void("input", sci.Map(), false):
    type Topic = "input"
    type Transport = ""
    type Form = Whatwg

    val Hidden = Tag.void["input", Whatwg](presets = sci.Map(t"type" -> t"hidden"))
    val Text = Tag.void["input", Whatwg](presets = sci.Map(t"type" -> t"text"))
    val Search = Tag.void["input", Whatwg](presets = sci.Map(t"type" -> t"search"))
    val Tel = Tag.void["input", Whatwg](presets = sci.Map(t"type" -> t"tel"))
    val Url = Tag.void["input", Whatwg](presets = sci.Map(t"type" -> t"url"))
    val Email = Tag.void["input", Whatwg](presets = sci.Map(t"type" -> t"email"))
    val Password = Tag.void["input", Whatwg](presets = sci.Map(t"type" -> t"password"))
    val Date = Tag.void["input", Whatwg](presets = sci.Map(t"type" -> t"date"))
    val Month = Tag.void["input", Whatwg](presets = sci.Map(t"type" -> t"month"))
    val Week = Tag.void["input", Whatwg](presets = sci.Map(t"type" -> t"week"))
    val Time = Tag.void["input", Whatwg](presets = sci.Map(t"type" -> t"time"))

    val DatetimeLocal =
      Tag.void["input", Whatwg](presets = sci.Map(t"type" -> t"datetime-local"))

    val Number = Tag.void["input", Whatwg](presets = sci.Map(t"type" -> t"number"))
    val Range = Tag.void["input", Whatwg](presets = sci.Map(t"type" -> t"range"))
    val Color = Tag.void["input", Whatwg](presets = sci.Map(t"type" -> t"color"))
    val Checkbox = Tag.void["input", Whatwg](presets = sci.Map(t"type" -> t"checkbox"))
    val Radio = Tag.void["input", Whatwg](presets = sci.Map(t"type" -> t"radio"))
    val Submit = Tag.void["input", Whatwg](presets = sci.Map(t"type" -> t"submit"))
    val Image = Tag.void["input", Whatwg](presets = sci.Map(t"type" -> t"image"))
    val Reset = Tag.void["input", Whatwg](presets = sci.Map(t"type" -> t"reset"))
    val Button = Tag.void["input", Whatwg](presets = sci.Map(t"type" -> t"button"))

  val Ins = Tag.transparent["ins", "", Whatwg]()
  val Kbd = Tag.container["kbd", Phrasing, Whatwg]()
  val Label = Tag.container["label", Phrasing, Whatwg]()

  val Legend =
    Tag.container["label", Phrasing | "h1" | "h2" | "h3" | "h4" | "h5" | "h6", Whatwg]()

  val Li = Tag.container["li", Flow, Whatwg](autoclose = true)

  object Link extends Tag.Void("link", sci.Map(), false):
    type Topic = "link"
    type Transport = ""
    type Form = Whatwg

    val Alternate = Tag.void["link", Whatwg](presets = sci.Map(t"rel" -> t"alternate"))
    val Canonical = Tag.void["link", Whatwg](presets = sci.Map(t"rel" -> t"canonical"))
    val Author = Tag.void["link", Whatwg](presets = sci.Map(t"rel" -> t"author"))
    val DnsPrefetch = Tag.void["link", Whatwg](presets = sci.Map(t"rel" -> t"dns-prefetch"))
    val Expect = Tag.void["link", Whatwg](presets = sci.Map(t"rel" -> t"expect"))
    val Help = Tag.void["link", Whatwg](presets = sci.Map(t"rel" -> t"help"))
    val Icon = Tag.void["link", Whatwg](presets = sci.Map(t"rel" -> t"icon"))
    val Manifest = Tag.void["link", Whatwg](presets = sci.Map(t"rel" -> t"manifest"))
    val Modulepreload = Tag.void["link", Whatwg](presets = sci.Map(t"rel" -> t"modulepreload"))
    val License = Tag.void["link", Whatwg](presets = sci.Map(t"rel" -> t"license"))
    val Next = Tag.void["link", Whatwg](presets = sci.Map(t"rel" -> t"next"))
    val Pingback = Tag.void["link", Whatwg](presets = sci.Map(t"rel" -> t"pingback"))
    val Preconnect = Tag.void["link", Whatwg](presets = sci.Map(t"rel" -> t"preconnect"))
    val Prefetch = Tag.void["link", Whatwg](presets = sci.Map(t"rel" -> t"prefetch"))
    val Preload = Tag.void["link", Whatwg](presets = sci.Map(t"rel" -> t"preload"))
    val Prev = Tag.void["link", Whatwg](presets = sci.Map(t"rel" -> t"prev"))
    val PrivacyPolicy = Tag.void["link", Whatwg](presets = sci.Map(t"rel" -> t"privacy-policy"))
    val Search = Tag.void["link", Whatwg](presets = sci.Map(t"rel" -> t"search"))
    val Stylesheet = Tag.void["link", Whatwg](presets = sci.Map(t"rel" -> t"stylesheet"))

    val TermsOfService =
      Tag.void["link", Whatwg](presets = sci.Map(t"rel" -> t"terms-of-service"))

  val Main = Tag.container["main", Flow, Whatwg]()
  val Map = Tag.transparent["map", "area", Whatwg]()
  val Mark = Tag.container["mark", Phrasing, Whatwg]()
  val Math = Tag.foreign["math", Whatwg]()
  val Menu = Tag.container["menu", "li" | ScriptSupporting, Whatwg](mode = Html.Mode.Whitespace)

  object Meta extends Tag.Void("meta", sci.Map(), false):
    val ApplicationName =
      Tag.void["meta", Whatwg](presets = sci.Map(t"name" -> t"application-name"))

    val Author = Tag.void["meta", Whatwg](presets = sci.Map(t"name" -> t"author"))
    val Description = Tag.void["meta", Whatwg](presets = sci.Map(t"name" -> t"description"))
    val Generator = Tag.void["meta", Whatwg](presets = sci.Map(t"name" -> t"generator"))
    val Keywords = Tag.void["meta", Whatwg](presets = sci.Map(t"name" -> t"keywords"))
    val Referrer = Tag.void["meta", Whatwg](presets = sci.Map(t"name" -> t"referrer"))
    val Viewport = Tag.void["meta", Whatwg](presets = sci.Map(t"name" -> t"viewport"))
    val ThemeColor = Tag.void["meta", Whatwg](presets = sci.Map(t"name" -> t"theme-color"))
    val ColorScheme = Tag.void["meta", Whatwg](presets = sci.Map(t"name" -> t"color-scheme"))

  val Meter = Tag.container["meter", Phrasing, Whatwg]()
  val Nav = Tag.container["nav", Flow, Whatwg]()
  val Noscript = Tag.container["noscript", "link" | "style" | "meta", Whatwg]()
  val Object = Tag.transparent["object", "", Whatwg](boundary = true)
  val Ol = Tag.container["ol", "li" | ScriptSupporting, Whatwg](mode = Html.Mode.Whitespace)

  val Optgroup = Tag.container["optgroup", "option" | "legend", Whatwg]
                  (autoclose = true, mode = Html.Mode.Whitespace)

  val Option = Tag.container["option", "#text", Whatwg](autoclose = true)
  val Output = Tag.container["output", Phrasing, Whatwg]()
  val P = Tag.container["p", Phrasing, Whatwg](autoclose = true)

  val Picture = Tag.container["picture", "source" | "img" | ScriptSupporting, Whatwg]
    ( mode = Html.Mode.Whitespace )

  val Pre = Tag.container["pre", Phrasing, Whatwg]()
  val Progress = Tag.container["progress", Phrasing, Whatwg]()
  val Q = Tag.container["q", Phrasing, Whatwg]()
  val Rp = Tag.container["rp", "#text", Whatwg](autoclose = true)
  val Rt = Tag.container["rt", Phrasing, Whatwg](autoclose = true)
  val Ruby = Tag.container["ruby", Phrasing | "rt" | "rp", Whatwg]()
  val S = Tag.container["s", Phrasing, Whatwg]()
  val Samp = Tag.container["samp", Phrasing, Whatwg]()
  val Script = Tag.container["script", "#text", Whatwg](mode = Html.Mode.Raw)
  val Search = Tag.container["search", Flow, Whatwg]()
  val Section = Tag.container["section", Flow, Whatwg]()

  val Select =
    Tag.container
      [ "select", "option" | "optgroup" | "hr" | "button" | "noscript" | ScriptSupporting, Whatwg ]
      ( mode = Html.Mode.Whitespace )

  val Selectedcontent = Tag.void["selectedcontent", Whatwg]()
  val Slot = Tag.transparent["slot", "", Whatwg]()
  val Small = Tag.container["small", Phrasing, Whatwg]()
  val Source = Tag.void["source", Whatwg]()
  val Span = Tag.container["span", Phrasing, Whatwg]()
  val Strong = Tag.container["strong", Phrasing, Whatwg]()
  val Style = Tag.container["style", "#text", Whatwg](mode = Html.Mode.Raw)
  val Sub = Tag.container["sub", Phrasing, Whatwg]()
  val Summary = Tag.container["summary", Phrasing | Heading, Whatwg]()
  val Sup = Tag.container["sup", Phrasing, Whatwg]()
  val Svg = Tag.foreign["svg", Whatwg]()

  val Table =
    Tag.container["table", "caption" | "colgroup" | "thead" | "tbody" | "tfoot", Whatwg]
      ( mode = Html.Mode.Whitespace, boundary = true )

  val Tbody = Tag.container["tbody", "tr", Whatwg]
    ( autoclose = true, mode = Html.Mode.Whitespace, insertable = true )

  val Td = Tag.container["td", Flow, Whatwg](autoclose = true, boundary = true)
  val Template = Tag.void["template", Whatwg](boundary = true)
  val Textarea = Tag.container["textarea", "#text", Whatwg](mode = Html.Mode.Rcdata)

  val Tfoot = Tag.container["tfoot", "tr", Whatwg]
    ( autoclose = true, mode = Html.Mode.Whitespace )

  val Th = Tag.container["th", Flow, Whatwg](autoclose = true, boundary = true)

  val Thead = Tag.container["thead", "tr" | ScriptSupporting, Whatwg]
    ( autoclose = true, mode = Html.Mode.Whitespace )

  val Time = Tag.container["time", Phrasing, Whatwg]()
  val Title = Tag.container["title", "#text", Whatwg](mode = Html.Mode.Rcdata)

  val Tr = Tag.container["tr", "td" | "th" | ScriptSupporting, Whatwg]
            (autoclose = true, mode = Html.Mode.Whitespace, insertable = true)

  object Track extends Tag.Void("track", sci.Map(), false):
    type Topic = "track"
    type Transport = ""
    type Form = Whatwg

    val Captions = Tag.void["track", Whatwg](presets = sci.Map(t"kind" -> t"captions"))
    val Chapters = Tag.void["track", Whatwg](presets = sci.Map(t"kind" -> t"chapters"))
    val Descriptions = Tag.void["track", Whatwg](presets = sci.Map(t"kind" -> t"descriptions"))
    val Metadata = Tag.void["track", Whatwg](presets = sci.Map(t"kind" -> t"metadata"))
    val Subtitles = Tag.void["track", Whatwg](presets = sci.Map(t"kind" -> t"subtitles"))

  val U = Tag.container["u", Phrasing, Whatwg]()
  val Ul = Tag.container["ul", "li" | ScriptSupporting, Whatwg](mode = Html.Mode.Whitespace)
  val Var = Tag.container["var", Phrasing, Whatwg]()
  val Video = Tag.transparent["video", "track" | "source", Whatwg]()
  val Wbr = Tag.void["wbr", Whatwg]()


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
