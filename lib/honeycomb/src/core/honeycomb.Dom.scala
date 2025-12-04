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

  erased given abbr: ("abbr" is Attribute on "th" of Attributive.Textual) = !!
  erased given accept: ("accept" is Attribute on "input" of Attributive.MimeList) = !!
  erased given acceptCharset: ("acceptCharset" is Attribute on "form" of Attributive.Utf8) = !!
  erased given accesskey: ("accesskey" is Attribute of Attributive.AccessKeys) = !!
  erased given action: ("action" is Attribute on "form" of Attributive.Url) = !!
  erased given allow: ("allow" is Attribute on "iframe" of Attributive.PermissionsPolicy) = !!
  erased given allowfullscreen: ("allowfullscreen" is Attribute on "iframe" of Attributive.Presence) = !!
  erased given alpha: ("alpha" is Attribute on "input" of Attributive.Presence) = !!
  erased given alt: ("alt" is Attribute on "area" | "img" | "input" of Attributive.Textual) = !!
  erased given as: ("as" is Attribute on "link" of Attributive.Textual) = !!
  erased given async: ("async" is Attribute on "script" of Attributive.Presence) = !!
  erased given autocapitalize: ("autocapitalize" is Attribute of Attributive.Autocapitalization) = !!
  erased given autocomplete: ("autocomplete" is Attribute on "form" of Attributive.Switch) = !!
  erased given autocomplete2: ("autocomplete" is Attribute on "input" | "select" | "textarea" of Attributive.Switch) = !!
  erased given autocorrect: ("autocorrect" is Attribute of Attributive.Textual) = !!
  erased given autofocus: ("autofocus" is Attribute of Attributive.Presence) = !!
  erased given autoplay: ("autoplay" is Attribute on "audio" | "video" of Attributive.Presence) = !!
  erased given blocking: ("blocking" is Attribute on "link" | "script" | "style" of Attributive.Tokens) = !!
  erased given charset: ("charset" is Attribute on "meta" of Attributive.Utf8) = !!
  erased given checked: ("checked" is Attribute on "input" of Attributive.Presence) = !!
  erased given cite: ("cite" is Attribute on "blockquote" | "del" | "ins" | "q" of Attributive.Url) = !!
  erased given `class`: ("class" is Attribute of Attributive.CssClasses) = !!
  erased given closedby: ("closedby" is Attribute on "dialog" of Attributive.Closedby) = !!
  erased given color: ("color" is Attribute on "link" of Attributive.Color) = !!
  erased given colorspace: ("colorspace" is Attribute on "input" of Attributive.Colorspace) = !!
  erased given cols: ("cols" is Attribute on "textarea" of Attributive.PositiveInt) = !!
  erased given colspan: ("colspan" is Attribute on "td" | "th" of Attributive.PositiveInt) = !!
  erased given command: ("command" is Attribute on "button" of Attributive.Command) = !!
  erased given commandfor: ("commandfor" is Attribute on "button" of Attributive.Id) = !!
  erased given content: ("content" is Attribute on "meta" of Attributive.Textual) = !!
  erased given contenteditable: ("contenteditable" is Attribute of Attributive.ContentEditable) = !!
  erased given controls: ("controls" is Attribute on "audio" | "video" of Attributive.Presence) = !!
  erased given coords: ("coords" is Attribute on "area" of Attributive.Coords) = !!
  erased given crossorigin: ("crossorigin" is Attribute on CrossoriginTags of Attributive.Crossorigin) = !!
  erased given data: ("data" is Attribute on "object" of Attributive.Url) = !!
  erased given datetime: ("datetime" is Attribute on "del" | "ins" of Attributive.Datetime) = !!
  erased given datetime2: ("datetime" is Attribute on "time" of Attributive.Temporal) = !!
  erased given decoding: ("decoding" is Attribute on "img" of Attributive.Decoding) = !!
  erased given default: ("default" is Attribute on "track" of Attributive.Presence) = !!
  erased given defer: ("defer" is Attribute on "script" of Attributive.Presence) = !!
  erased given dir: ("dir" is Attribute of Attributive.Dir) = !!
  erased given dirname: ("dirname" is Attribute on "input" | "textarea" of Attributive.Textual) = !!
  erased given disabled: ("disabled" is Attribute on "button" | "input" | "optgroup" | "option" | "select" | "textarea" of Attributive.Presence) = !!
  erased given disabled2: ("disabled" is Attribute on "fieldset" of Attributive.Presence) = !!
  erased given disabled3: ("disabled" is Attribute on "link" of Attributive.Presence) = !!
  erased given download4: ("download" is Attribute on "a" | "area" of Attributive.Textual) = !!
  erased given draggable: ("draggable" is Attribute of Attributive.Truth) = !!
  erased given enctype: ("enctype" is Attribute on "form" of Attributive.Enctype) = !!
  erased given enterkeyhint: ("enterkeyhint" is Attribute of Attributive.EnterKeyHint) = !!
  erased given fetchpriority: ("fetchpriority" is Attribute on "img" | "link" | "script" of Attributive.FetchPriority) = !!
  erased given `for`: ("for" is Attribute on "label" of Attributive.Id) = !!
  erased given for2: ("for" is Attribute on "output" of Attributive.Tokens) = !!
  erased given form: ("form" is Attribute on FormTags of Attributive.Id) = !!
  erased given formaction: ("formaction" is Attribute on "input" | "button" of Attributive.Url) = !!
  erased given formenctype: ("formenctype" is Attribute on "input" | "button" of Attributive.Enctype) = !!
  erased given formmethod: ("formmethod" is Attribute on "input" | "button" of Attributive.Method) = !!
  erased given formnovalidate: ("formnovalidate" is Attribute on "input" | "button" of Attributive.Presence) = !!
  erased given formtarget: ("formtarget" is Attribute on "input" | "button" of Attributive.Target) = !!
  erased given headers: ("headers" is Attribute on "td" | "th" of Attributive.Tokens) = !!
  erased given headingoffset: ("headingoffset" is Attribute of Attributive.Upto8) = !!
  erased given headingreset: ("headingoffset" is Attribute of Attributive.Presence) = !!
  erased given height: ("height" is Attribute on HeightTags of Attributive.PositiveInt) = !!
  erased given hidden: ("hidden" is Attribute of Attributive.Hidden) = !!
  erased given high: ("high" is Attribute on "meter" of Attributive.Decimal) = !!
  erased given href: ("href" is Attribute on "a" | "area" of Attributive.Url) = !!
  erased given href2: ("href" is Attribute on "base" of Attributive.Url) = !!
  erased given href3: ("href" is Attribute on "link" of Attributive.Url) = !!
  erased given hreflang: ("hreflang" is Attribute on "a" | "link" of Attributive.Language) = !!
  erased given httpEquiv: ("http-equiv" is Attribute on "meta" of Attributive.HttpEquiv) = !!
  erased given id: ("id" is Attribute of Attributive.Id) = !!
  erased given imagesizes: ("imagesizes" is Attribute on "link" of Attributive.ImageSizes) = !!
  erased given imagesrcset: ("imagesrcset" is Attribute on "link" of Attributive.ImageSrcSet) = !!
  erased given inert: ("inert" is Attribute of Attributive.Presence) = !!
  erased given inputmode: ("inputmode" is Attribute of Attributive.InputMode) = !!
  erased given integrity: ("integrity" is Attribute on "link" | "script" of Attributive.Textual) = !!
  erased given is: ("is" is Attribute of Attributive.CustomElementName) = !!
  erased given ismap: ("ismap" is Attribute on "img" of Attributive.Presence) = !!
  erased given itemid: ("itemid" is Attribute of Attributive.Url) = !!
  erased given itemprop: ("itemprop" is Attribute of Attributive.ItemProp) = !!
  erased given itemref: ("itemref" is Attribute of Attributive.Ids) = !!
  erased given itemscope: ("itemscope" is Attribute of Attributive.Presence) = !!
  erased given itemtype: ("itemtype" is Attribute of Attributive.Urls) = !!
  erased given kind: ("kind" is Attribute on "track" of Attributive.Kind) = !!
  erased given label: ("label" is Attribute on "optgroup" | "option" | "track" of Attributive.Textual) = !!
  erased given lang: ("lang" is Attribute of Attributive.Language) = !!
  erased given list: ("list" is Attribute on "input" of Attributive.Id) = !!
  erased given loading: ("loading" is Attribute on "img" | "iframe" of Attributive.Laziness) = !!
  erased given loop: ("loop" is Attribute on "audio" | "video" of Attributive.Presence) = !!
  erased given low: ("low" is Attribute on "meter" of Attributive.Decimal) = !!
  erased given max: ("max" is Attribute on "input" of Attributive.Minmax) = !!
  erased given max2: ("max" is Attribute on "meter" | "progress" of Attributive.Decimal) = !!
  erased given maxlength: ("maxlength" is Attribute on "input" | "textarea" of Attributive.PositiveInt) = !!
  erased given media: ("media" is Attribute on MediaTags of Attributive.MediaQueryList) = !!
  erased given method: ("method" is Attribute on "form" of Attributive.Method) = !!
  erased given min: ("min" is Attribute on "input" of Attributive.Minmax) = !!
  erased given min2: ("min" is Attribute on "meter" of Attributive.Decimal) = !!
  // min
  erased given minlength: ("minlength" is Attribute on "input" | "textarea" of Attributive.PositiveInt) = !!
  erased given multiple: ("multiple" is Attribute on "input" | "select" of Attributive.Presence) = !!
  erased given muted: ("muted" is Attribute on "audio" | "video" of Attributive.Presence) = !!
  erased given name: ("name" is Attribute on "button" | "fieldset" | "input" | "output" | "select" | "textarea" of Attributive.Name) = !!
  erased given name2: ("name" is Attribute on "details" of Attributive.Name) = !!
  erased given name3: ("name" is Attribute on "form" of Attributive.Name) = !!
  erased given name4: ("name" is Attribute on "iframe" | "object" of Attributive.Target) = !!
  erased given name5: ("name" is Attribute on "map" of Attributive.Name) = !!
  erased given name6: ("name" is Attribute on "meta" of Attributive.Name) = !!
  erased given name7: ("name" is Attribute on "slot" of Attributive.Name) = !!
  erased given nomodule: ("nomodule" is Attribute on "script" of Attributive.Presence) = !!
  erased given nonce: ("nonce" is Attribute of Attributive.Textual) = !!
  erased given novalidate: ("novalidate" is Attribute on "form" of Attributive.Presence) = !!
  erased given open: ("open" is Attribute on "details" | "dialog" of Attributive.Presence) = !!
  erased given optimum: ("optimum" is Attribute on "meter" of Attributive.Decimal) = !!
  erased given pattern: ("pattern" is Attribute on "input" of Attributive.Regex) = !!
  erased given ping: ("ping" is Attribute on "a" | "area" of Attributive.Urls) = !!
  erased given placeholder: ("placeholder" is Attribute on "input" | "textarea" of Attributive.Textual) = !!
  erased given playsinline: ("playsinline" is Attribute on "video" of Attributive.Presence) = !!
  erased given popover: ("popover" is Attribute of Attributive.Popover) = !!
  erased given popovertarget: ("popovertarget" is Attribute on "button" | "input" of Attributive.Id) = !!
  erased given popovertargetaction: ("popovertargetaction" is Attribute on "button" | "input" of Attributive.PopoverAction) = !!
  erased given poster: ("poster" is Attribute on "video" of Attributive.Url) = !!
  erased given preload: ("preload" is Attribute on "audio" | "video" of Attributive.Preload) = !!
  erased given readonly: ("readonly" is Attribute on "input" | "textarea" of Attributive.Presence) = !!
  erased given referrerpolicy: ("referrerpolicy" is Attribute on ReferrerpolicyTags of Attributive.ReferrerPolicy) = !!
  erased given rel: ("rel" is Attribute on "a" | "area" of Attributive.Tokens) = !!
  erased given rel2: ("rel" is Attribute on "link" of Attributive.Tokens) = !!
  erased given required: ("required" is Attribute on RequiredTags of Attributive.Presence) = !!
  erased given reversed: ("reversed" is Attribute on "ol" of Attributive.Presence) = !!
  erased given rows: ("rows" is Attribute on "textarea" of Attributive.PositiveInt) = !!
  erased given rowspan: ("rowspan" is Attribute on "td" | "th" of Attributive.PositiveInt) = !!
  erased given sandbox: ("sandbox" is Attribute on "iframe" of Attributive.Sandbox) = !!
  erased given scope: ("scope" is Attribute on "th" of Attributive.ThScope) = !!
  erased given selected: ("selected" is Attribute on "option" of Attributive.Presence) = !!
  erased given shadowrootclonable: ("shadowrootclonable" is Attribute on "template" of Attributive.Presence) = !!
  erased given shadowrootcustomelementregistry: ("shadowrootcustomelementregistry" is Attribute on "template" of Attributive.Presence) = !!
  erased given shadowrootdelegatesfocus: ("shadowrootdelegatesfocus" is Attribute on "template" of Attributive.Presence) = !!
  erased given shadowrootmode: ("shadowrootmode" is Attribute on "template" of Attributive.Openness) = !!
  erased given shadowrootserializable: ("shadowrootserializable" is Attribute on "template" of Attributive.Presence) = !!
  erased given shape: ("shape" is Attribute on "area" of Attributive.Shape) = !!
  erased given size: ("size" is Attribute on "input" | "select" of Attributive.PositiveInt) = !!
  erased given sizes: ("sizes" is Attribute on "img" | "source" of Attributive.SourceSizeList) = !!
  erased given sizes2: ("sizes" is Attribute on "link" of Attributive.Sizes) = !!
  erased given slot: ("slot" is Attribute of Attributive.Textual) = !!
  erased given span: ("span" is Attribute on "col" | "colgroup" of Attributive.PositiveInt) = !!
  erased given spellcheck: ("spellcheck" is Attribute of Attributive.Truth) = !!
  erased given src: ("src" is Attribute on SrcTags of Attributive.Url) = !!
  erased given srcdoc: ("srcdoc" is Attribute on "iframe" of Attributive.Srcdoc) = !!
  erased given srclang: ("srclang" is Attribute on "track" of Attributive.Language) = !!
  erased given srcset: ("srcset" is Attribute on "img" | "source" of Attributive.SrcSet) = !!
  erased given start: ("start" is Attribute on "ol" of Attributive.Integral) = !!
  erased given step: ("step" is Attribute on "input" of Attributive.Decimal) = !!
  erased given style: ("style" is Attribute of Attributive.Css) = !!
  erased given tabindex: ("tabindex" is Attribute of Attributive.Integral) = !!
  erased given target: ("target" is Attribute on "a" | "area" of Attributive.Target) = !!
  erased given target2: ("target" is Attribute on "base" of Attributive.Target) = !!
  erased given target3: ("target" is Attribute on "form" of Attributive.Target) = !!
  erased given title: ("title" is Attribute of Attributive.Textual) = !!
  erased given translate: ("translate" is Attribute of Attributive.Affirmation) = !!
  erased given `type`: ("type" is Attribute on "a" | "link" of Attributive.Mime) = !!
  erased given type2: ("type" is Attribute on "button" of Attributive.ButtonType) = !!
  erased given type3: ("type" is Attribute on "embed" | "object" | "source" of Attributive.Mime) = !!
  erased given type4: ("type" is Attribute on "input" of Attributive.InputType) = !!
  erased given type5: ("type" is Attribute on "ol" of Attributive.OlType) = !!
  erased given type6: ("type" is Attribute on "script" of Attributive.ScriptType) = !!
  erased given usemap: ("usemaptype" is Attribute on "img" | "input" | "object" of Attributive.HashName) = !!
  erased given value: ("value" is Attribute on "button" | "option" of Attributive.Textual) = !!
  erased given value2: ("value" is Attribute on "data" of Attributive.Textual) = !!
  erased given value3: ("value" is Attribute on "input" of Attributive.InputValue) = !!
  erased given value4: ("value" is Attribute on "li" of Attributive.Integral) = !!
  erased given value5: ("value" is Attribute on "meter" | "progress" of Attributive.Decimal) = !!
  erased given width: ("width" is Attribute on WidthTags of Attributive.PositiveInt) = !!
  erased given wrap: ("wrap" is Attribute on "textarea" of Attributive.Softness) = !!
  erased given writingsuggestions: ("writingsuggestions" is Attribute of Attributive.Truth) = !!

class Whatwg() extends Dom:
  import Html.Issue.*

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


  val elements: Dictionary[Tag] =
    Dictionary(this.membersOfType[Tag].to(Seq).bi.map(_.label -> _)*)

  val entities: Dictionary[Text] =
    val list = cp"/honeycomb/entities.tsv".read[Text].cut(t"\n").map(_.cut(t"\t")).collect:
      case List(key, value) => (key, value)

    Dictionary(list*)
