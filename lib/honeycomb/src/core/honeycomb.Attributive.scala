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

import anticipation.*
import gesticulate.*
import gossamer.*
import hieroglyph.*
import kaleidoscope.*
import prepositional.*
import proscenium.*
import rudiments.*
import serpentine.*
import spectacular.*
import urticose.*
import vacuous.*

object AttributeConversion:
  given textTtextual: Text is AttributeConversion to Attribute.Textual = (key, value) => Attribute(key, value)
  given stringTextual: String is AttributeConversion to Attribute.Textual = (key, value) => Attribute(key, value)

  given boolean: Boolean is AttributeConversion to Attribute.Textual = (key, value) =>
    if value then Attribute(key, Unset) else Unset

trait AttributeConversion extends Typeclass, Resultant:
  def attribute(key: Text, value: Self): Optional[Attribute]

erased trait Attributable:
  type Self <: Label
  type Operand
  type Plane <: Label

object Attributable:
  import Attribute.*

  erased given accesskey: ("accesskey" is Attributable on Label by Textual) = !!
  erased given anchor: ("anchor" is Attributable on Label by Textual) = !!
  erased given autocapitalize: ("autocapitalize" is Attributable on Label by Textual) = !!
  erased given autocorrect: ("autocorrect" is Attributable on Label by Textual) = !!
  erased given autofocus: ("autofocus" is Attributable on Label by Textual) = !!
  erased given `class`: ("class" is Attributable on Label by Textual) = !!
  erased given contenteditable: ("contenteditable" is Attributable on Label by Textual) = !!
  erased given dir: ("dir" is Attributable on Label by Textual) = !!
  erased given draggable: ("draggable" is Attributable on Label by Textual) = !!
  erased given enterkeyhint: ("enterkeyhint" is Attributable on Label by Textual) = !!
  erased given exportparts: ("exportparts" is Attributable on Label by Textual) = !!

  private type HeightTags =
    "canvas" | "embed" | "iframe" | "img" | "input" | "object" | "video" | "svg"

  erased given height: ("height" is Attributable on HeightTags by Textual) = !!

  erased given hidden: ("hidden" is Attributable on Label by Textual) = !!
  erased given id: ("id" is Attributable on Label by Textual) = !!
  erased given inert: ("inert" is Attributable on Label by Textual) = !!
  erased given inputmode: ("inputmode" is Attributable on Label by Textual) = !!
  erased given is: ("is" is Attributable on Label by Textual) = !!
  erased given itemid: ("itemid" is Attributable on Label by Textual) = !!
  erased given itemprop: ("itemprop" is Attributable on Label by Textual) = !!
  erased given itemref: ("itemref" is Attributable on Label by Textual) = !!
  erased given itemscope: ("itemscope" is Attributable on Label by Textual) = !!
  erased given itemtype: ("itemtype" is Attributable on Label by Textual) = !!
  erased given lang: ("lang" is Attributable on Label by Textual) = !!
  erased given nonce: ("nonce" is Attributable on Label by Textual) = !!
  erased given part: ("part" is Attributable on Label by Textual) = !!
  erased given popover: ("popover" is Attributable on Label by Textual) = !!
  erased given role: ("role" is Attributable on Label by Textual) = !!
  erased given slot: ("slot" is Attributable on Label by Textual) = !!
  erased given spellcheck: ("spellcheck" is Attributable on Label by Textual) = !!
  erased given style: ("style" is Attributable on Label by Textual) = !!
  erased given tabindex: ("tabindex" is Attributable on Label by Textual) = !!
  erased given title: ("title" is Attributable on Label by Textual) = !!
  erased given translate: ("translate" is Attributable on Label by Textual) = !!
  erased given vkp: ("virtualkeyboardpolicy" is Attributable on Label by Textual) = !!
  erased given writingsuggestions: ("writingsuggestions" is Attributable on Label by Textual) = !!
  erased given accept: ("accept" is Attributable on "form" | "input" by Textual) = !!
  erased given acceptCharset: ("acceptCharset" is Attributable on "form" by Textual) = !!
  erased given action: ("action" is Attributable on "form" by Textual) = !!
  erased given allow: ("allow" is Attributable on "iframe" by Textual) = !!
  erased given alpha: ("alpha" is Attributable on "input" by Textual) = !!
  erased given alt: ("alt" is Attributable on "area" | "img" | "input" by Textual) = !!
  erased given as: ("as" is Attributable on "link" by Textual) = !!
  erased given async: ("async" is Attributable on "script" by Textual) = !!
  erased given autoplay: ("autoplay" is Attributable on "audio" | "video" by Textual) = !!
  erased given capture: ("capture" is Attributable on "input" by Textual) = !!
  erased given charset: ("charset" is Attributable on "meta" by Textual) = !!
  erased given checked: ("checked" is Attributable on "input" by Textual) = !!
  erased given cite: ("cite" is Attributable on "blockquote" | "del" | "ins" | "q" by Textual) = !!
  erased given colorspace: ("colorspace" is Attributable on "input" by Textual) = !!
  erased given cols: ("cols" is Attributable on "textarea" by Textual) = !!
  erased given colspan: ("colspan" is Attributable on "td" | "th" by Textual) = !!
  erased given controls: ("controls" is Attributable on "audio" | "video" by Textual) = !!
  erased given coords: ("coords" is Attributable on "area" by Textual) = !!

  private type CrossoriginTags = "audio" | "img" | "link" | "script" | "video"
  erased given crossorigin: ("crossorigin" is Attributable on CrossoriginTags by Textual) = !!

  erased given csp: ("csp" is Attributable on "iframe" by Textual) = !!
  erased given data: ("data" is Attributable on "object" by Textual) = !!
  erased given datetime: ("datetime" is Attributable on "del" | "ins" | "time" by Textual) = !!
  erased given decoding: ("decoding" is Attributable on "img" by Textual) = !!
  erased given default: ("default" is Attributable on "track" by Textual) = !!
  erased given defer: ("defer" is Attributable on "script" by Textual) = !!
  erased given dirname: ("dirname" is Attributable on "input" | "textarea" by Textual) = !!

  private type DisabledTags =
    "button" | "fieldset" | "input" | "optgroup" | "option" | "select" | "textarea"

  erased given disabled: ("disabled" is Attributable on DisabledTags by Textual) = !!
  erased given display: ("display" is Attributable on "math" by Textual) = !!
  erased given download: ("download" is Attributable on "a" | "area" by Textual) = !!
  erased given enctype: ("enctype" is Attributable on "form" by Textual) = !!
  erased given elementtiming: ("elementtiming" is Attributable on "img" | "video" by Textual) = !!

  private type FetchpriorityTags = "img" | "link" | "script"
  erased given fetchpriority: ("fetchpriority" is Attributable on FetchpriorityTags by Textual) = !!

  erased given `for`: ("for" is Attributable on "label" | "output" by Textual) = !!

  private type FormTags =
    "button" | "fieldset" | "input" | "object" | "output" | "select" | "textarea"

  erased given form: ("form" is Attributable on FormTags by Textual) = !!
  erased given formaction: ("formaction" is Attributable on "input" | "button" by Textual) = !!
  erased given formenctype: ("formenctype" is Attributable on "input" | "button" by Textual) = !!
  erased given formmethod: ("formmethod" is Attributable on "input" | "button" by Textual) = !!

  erased given formnovalidate: ("formnovalidate" is Attributable on "input" | "button" by Textual) =
    !!

  erased given formtarget: ("formtarget" is Attributable on "input" | "button" by Textual) = !!
  erased given headers: ("headers" is Attributable on "td" | "th" by Textual) = !!
  erased given high: ("high" is Attributable on "meter" by Textual) = !!
  erased given href: ("href" is Attributable on "a" | "area" | "base" | "link" by Textual) = !!
  erased given hreflang: ("hreflang" is Attributable on "a" | "link" by Textual) = !!
  erased given httpEquiv: ("httpEquiv" is Attributable on "meta" by Textual) = !!
  erased given integrity: ("integrity" is Attributable on "link" | "script" by Textual) = !!
  erased given ismap: ("ismap" is Attributable on "img" by Textual) = !!
  erased given kind: ("kind" is Attributable on "track" by Textual) = !!
  erased given label: ("label" is Attributable on "optgroup" | "option" | "track" by Textual) = !!
  erased given loading: ("loading" is Attributable on "img" | "iframe" by Textual) = !!
  erased given list: ("list" is Attributable on "input" by Textual) = !!
  erased given loop: ("loop" is Attributable on "audio" | "video" by Textual) = !!
  erased given low: ("low" is Attributable on "meter" by Textual) = !!
  erased given max: ("max" is Attributable on "input" | "meter" | "progress" by Textual) = !!
  erased given maxlength: ("maxlength" is Attributable on "input" | "textarea" by Textual) = !!
  erased given minlength: ("minlength" is Attributable on "input" | "textarea" by Textual) = !!

  private type MediaTags = "a" | "area" | "link" | "source" | "style"
  erased given media: ("media" is Attributable on MediaTags by Textual) = !!

  erased given method: ("method" is Attributable on "form" by Textual) = !!
  erased given min: ("min" is Attributable on "input" | "meter" by Textual) = !!
  erased given multiple: ("multiple" is Attributable on "input" | "select" by Textual) = !!
  erased given muted: ("muted" is Attributable on "audio" | "video" by Textual) = !!

  private type NameTags =
    "button" | "form" | "fieldset" | "iframe" | "input" | "object" | "output" | "select"
    | "textarea" | "map" | "meta" | "param"

  erased given name: ("name" is Attributable on NameTags by Textual) = !!
  erased given novalidate: ("novalidate" is Attributable on "form" by Textual) = !!
  erased given open: ("open" is Attributable on "details" | "dialog" by Textual) = !!
  erased given optimum: ("optimum" is Attributable on "meter" by Textual) = !!
  erased given pattern: ("pattern" is Attributable on "input" by Textual) = !!
  erased given ping: ("ping" is Attributable on "a" | "area" by Textual) = !!
  erased given placeholder: ("placeholder" is Attributable on "input" | "textarea" by Textual) = !!
  erased given playsinline: ("playsinline" is Attributable on "video" by Textual) = !!
  erased given poster: ("poster" is Attributable on "video" by Textual) = !!
  erased given preload: ("preload" is Attributable on "audio" | "video" by Textual) = !!
  erased given readonly: ("readonly" is Attributable on "input" | "textarea" by Textual) = !!

  private type ReferrerpolicyTags = "a" | "area" | "iframe" | "img" | "link" | "script"
  erased given referrerpolicy: ("referrerpolicy" is Attributable on ReferrerpolicyTags by Textual) =
    !!

  erased given rel: ("rel" is Attributable on "a" | "area" | "link" by Textual) = !!

  private type RequiredTags = "input" | "select" | "textarea"
  erased given required: ("required" is Attributable on RequiredTags by Textual) = !!

  erased given reversed: ("reversed" is Attributable on "ol" by Textual) = !!
  erased given rows: ("rows" is Attributable on "textarea" by Textual) = !!
  erased given rowspan: ("rowspan" is Attributable on "td" | "th" by Textual) = !!
  erased given sandbox: ("sandbox" is Attributable on "iframe" by Textual) = !!
  erased given scope: ("scope" is Attributable on "th" by Textual) = !!
  erased given selected: ("selected" is Attributable on "option" by Textual) = !!
  erased given shape: ("shape" is Attributable on "a" | "area" by Textual) = !!
  erased given size: ("size" is Attributable on "input" | "select" by Textual) = !!
  erased given sizes: ("sizes" is Attributable on "link" | "img" | "source" by Textual) = !!
  erased given span: ("span" is Attributable on "col" | "colgroup" by Textual) = !!

  private type SrcTags =
    "audio" | "embed" | "iframe" | "img" | "input" | "script" | "source" | "track" | "video"

  erased given src: ("src" is Attributable on SrcTags by Textual) = !!

  erased given srcdoc: ("srcdoc" is Attributable on "iframe" by Textual) = !!
  erased given srclang: ("srclang" is Attributable on "track" by Textual) = !!
  erased given srcset: ("srcset" is Attributable on "img" | "source" by Textual) = !!
  erased given start: ("start" is Attributable on "ol" by Textual) = !!
  erased given step: ("step" is Attributable on "input" by Textual) = !!
  erased given summary: ("summary" is Attributable on "table" by Textual) = !!
  erased given target: ("target" is Attributable on "a" | "area" | "base" | "form" by Textual) = !!

  private type TypeTags =
    "button" | "input" | "embed" | "object" | "ol" | "script" | "source" | "style" | "menu" | "link"

  erased given `type`: ("type" is Attributable on TypeTags by Textual) = !!
  erased given usemap: ("usemaptype" is Attributable on "img" | "input" | "object" by Textual) = !!

  private type ValueTags =
    "button" | "data" | "input" | "li" | "meter" | "option" | "progress" | "param"

  erased given value: ("value" is Attributable on ValueTags by Textual) = !!

  private type WidthTags =
    "canvas" | "embed" | "iframe" | "img" | "input" | "object" | "video" | "svg"

  erased given width: ("width" is Attributable on WidthTags by Textual) = !!

  erased given wrap: ("wrap" is Attributable on "textarea" by Textual) = !!


trait Attributive extends Targetable:
  type Self <: Label
  type Topic
  def convert(value: Topic): Optional[Text | Attributive.NotShown.type]
  def rename: Optional[Text] = Unset

object Attributive:
  object NotShown

  given generic: [label <: Label: GenericHtmlAttribute2[value], value] =>  label is Attributive:
    type Topic = value
    def convert(value: value): Optional[Text] = label.serialize(value).show
    override def rename: Optional[Text] = label.name.show

  given accept: ("accept" is Attributive of List[Text]) = _.join(t",")

  given acceptCharset: ("acceptCharset" is Attributive):
    type Topic = Encoding
    override def rename: Optional[Text] = t"accept-charset"
    def convert(value: Encoding): Text = value.name

  given accesskey: ("accesskey" is Attributive of Char) = _.show
  given action: ("action" is Attributive of Text) = identity(_)

  given action2: [url: Abstractable across Urls to Text] => ("action" is Attributive of url) =
    _.generic

  given allowfullscreen: ("allowfullscreen" is Attributive of Boolean) = _ => Unset
  given allowpaymentrequest: ("allowpaymentrequest" is Attributive of Boolean) = _ => Unset
  given alt: ("alt" is Attributive of Text) = identity(_)
  given async: ("async" is Attributive of Boolean) = _ => Unset
  given autocomplete: ("autocomplete" is Attributive of Autocomplete) = _.show
  given autocomplete2: ("autocomplete" is Attributive of Boolean) = t"off".unless(_)
  given autoplay: ("autoplay" is Attributive of Boolean) = _ => Unset
  given autofocus: ("autofocus" is Attributive of Boolean) = _ => Unset
  given border: ("border" is Attributive of Boolean) = if _ then t"1" else t""
  given charset: ("charset" is Attributive of Encoding) = _.name
  given checkedBoolean: ("checked" is Attributive of Boolean) = if _ then Unset else NotShown
  given cite: ("cite" is Attributive of Text) = identity(_)

  given cite2: [url: Abstractable across Urls to Text] => ("cite" is Attributive of url) =
    _.generic

  given `class`: ("class" is Attributive):
    type Topic = List[CssClass]
    override def rename: Optional[Text] = t"class"
    def convert(value: List[CssClass]): Text = value.map(_.name).join(t" ")

  given class2: Attributive of CssClass:
    override def rename: Optional[Text] = t"class"
    def convert(value: CssClass): Text = value.name

  given code: ("code" is Attributive of Text) = identity(_) // MediaError
  given codebase: ("codebase" is Attributive of Text) = identity(_)

  given codebase2: [url: Abstractable across Urls to Text] => ("codebase" is Attributive of url) =
    _.generic

  given cols: ("cols" is Attributive of Int) = _.show
  given colspan: ("colspan" is Attributive of Int) = _.show
  given content: ("content" is Attributive of Text) = identity(_)

  given contenteditable: ("contenteditable" is Attributive of Boolean) =
    if _ then t"true" else t"false"

  given controls: ("controls" is Attributive of Boolean) = if _ then Unset else NotShown
  given coords: ("coords" is Attributive of Seq[Double]) = _.map(_.toString.show).join(t",")
  given crossorigin: ("crossorigin" is Attributive of Crossorigin) = _.show
  given data: ("data" is Attributive of Text) = identity(_)

  given data2: [url: Abstractable across Urls to Text] => ("data" is Attributive of url) =
    _.generic

  given datetime: ("datetime" is Attributive of Text) = identity(_) // To be provided by Aviation
  given default: ("default" is Attributive of Boolean) = if _ then Unset else NotShown
  given defer: ("defer" is Attributive of Boolean) = if _ then Unset else NotShown
  given dir: ("dir" is Attributive of HDir) = _.show

  // Should be the name of an input in a form, followed by `.dir`
  given dirname: ("dirname" is Attributive of Text) = identity(_)

  given disabled: ("disabled" is Attributive of Boolean) = if _ then Unset else NotShown

  // should be a filename, but probably best as `Text`
  given download: ("download" is Attributive of Text) = identity(_)

  given draggable: ("draggable" is Attributive of Boolean) = if _ then Unset else NotShown
  given enctype: ("enctype" is Attributive of MediaType) = _.show

  given hfor: ("hfor" is Attributive):
    type Topic = DomId
    override def rename: Optional[Text] = t"for"
    def convert(value: DomId): Text = value.name

  given hfors: ("hfor" is Attributive):
    type Topic = Seq[DomId]
    override def rename: Optional[Text] = t"for"
    def convert(value: Seq[DomId]): Text = value.map(_.name).join(t" ")

  given `for`: ("for" is Attributive):
    type Topic = DomId
    def convert(value: DomId): Text = value.name

  given fors: ("for" is Attributive):
    type Topic = Seq[DomId]
    def convert(value: Seq[DomId]): Text = value.map(_.name).join(t" ")

  given form: ("form" is Attributive of DomId) = _.name
  given formaction: ("formaction" is Attributive of Text) = identity(_) // Provided by Scintillate

  given formaction2: [url: Abstractable across Urls to Text]
        => ("formaction" is Attributive of url) =
    _.generic

  given formenctype: ("formenctype" is Attributive of Text) = identity(_)
  given formmethod: ("formmethod" is Attributive of Method) = _.show
  given formnovalidate: ("formnovalidate" is Attributive of Boolean) = if _ then Unset else NotShown
  given formtarget: ("formtarget" is Attributive of Target) = _.show
  given headers: ("headers" is Attributive of DomId) = _.name
  given headers2: ("headers" is Attributive of Set[DomId]) = _.map(_.name).join(t" ")
  given height: ("height" is Attributive of Int) = _.show
  given hidden: ("hidden" is Attributive of Boolean) = if _ then Unset else NotShown
  given high: ("high" is Attributive of Double) = _.toString.show

  given href: ("href" is Attributive of Text) = identity(_)

  given href2: [url: Abstractable across Urls to Text] => ("href" is Attributive of url) =
    _.generic

  inline given href3: [topic, path <: Path of topic under %.type] => Www is Filesystem
         => ("href" is Attributive of path) =
    _.on[Www].encode

  inline given href4: [topic, path <: Path of topic] => Www is Filesystem
         => ("href" is Attributive of path) =
    _.on[Www].encode

  inline given href4: [path <: Path on Www] => ("href" is Attributive of path) = _.encode

  given href5: [path: Abstractable across Paths to Text] => ("href" is Attributive of path) =
    _.generic

  given href6: [relative <: Relative on Www] => ("href" is Attributive of relative) =
    _.encode

  inline given href7: [topic, relative <: Relative of topic]
               => ("href" is Attributive of relative) =
    _.on[Www].encode

  // Needs to be provided by Cosmopolite
  given hreflang: ("hreflang" is Attributive of Text) = identity(_)

  given httpEquiv: ("httpEquiv" is Attributive):
    type Topic = HttpEquiv
    override def rename: Optional[Text] = t"http-equiv"
    def convert(value: HttpEquiv): Text = value.show

  given id: ("id" is Attributive of DomId) = _.name
  given ismap: ("ismap" is Attributive of Boolean) = if _ then Unset else NotShown
  given kind: ("kind" is Attributive of Kind) = _.show
  given label: ("label" is Attributive of Text) = identity(_)
  given lang: ("lang" is Attributive of Text) = identity(_) // Should be provided by Cosmopolite
  given list: ("list" is Attributive of Seq[DomId]) = _.map(_.name).join(t" ")
  given list2: ("list" is Attributive of DomId) = _.name
  given loop: ("loop" is Attributive of Boolean) = if _ then Unset else NotShown
  given low: ("low" is Attributive of Double) = _.toString.tt
  given manifest: ("manifest" is Attributive of Text) = identity(_) // Provided by Scintillate

  given manifest2: [url: Abstractable across Urls to Text]
        => ("manifest" is Attributive of url) =
    _.generic

  given max: ("max" is Attributive of Double | Int) = _.toString.show
  given maxlength: ("maxlength" is Attributive of Int) = _.show
  given minlength: ("minlength" is Attributive of Int) = _.show
  given media: ("media" is Attributive of Text) = identity(_) // Should be provided by Cataclysm
  given method: ("method" is Attributive of Method) = _.show
  given min: ("min" is Attributive of Double | Int) = _.toString.show
  given multiple: ("multiple" is Attributive of Boolean) = if _ then Unset else NotShown
  given muted: ("muted" is Attributive of Boolean) = if _ then Unset else NotShown

  // Should provide special `name` identifiers
  given name: ("name" is Attributive of Text) = identity(_)

  given name2: ("name" is Attributive of Target onto "object") = _.show
  given name3: ("name" is Attributive of Target onto "iframe") = _.show
  given nonce: ("nonce" is Attributive of Text) = identity(_) // Should be provided by Gastronomy
  given novalidate: ("novalidate" is Attributive of Boolean) = if _ then Unset else NotShown
  given open: ("open" is Attributive of Boolean) = if _ then Unset else NotShown
  given optimum: ("optimum" is Attributive of Double) = _.toString.show
  given pattern: ("pattern" is Attributive of Regex) = _.pattern
  given placeholder: ("placeholder" is Attributive of Text) = identity(_)
  given playsinline: ("playsinline" is Attributive of Boolean) = if _ then Unset else NotShown
  given poster: ("poster" is Attributive of Text) = identity(_)

  given poster2: [url: Abstractable across Urls to Text] => ("poster" is Attributive of url) =
    _.generic

  given preload: ("preload" is Attributive of Preload) = _.show
  given readonly: ("readonly" is Attributive of Boolean) = if _ then Unset else NotShown
  given referrerpolicy: ("referrerpolicy" is Attributive of Text) = identity(_)
  given rel: ("rel" is Attributive of Rel) = _.show
  given rel2: ("rel" is Attributive of Seq[Rel]) = _.map(_.show).join(t" ")
  given required: ("required" is Attributive of Boolean) = if _ then Unset else NotShown
  given rev: ("rev" is Attributive of Rev) = _.show
  given rows: ("rows" is Attributive of Int) = _.show
  given rowspan: ("rowspan" is Attributive of Int) = _.show
  given sandbox: ("sandbox" is Attributive of Sandbox) = _.show
  given scope: ("scope" is Attributive of Scope) = _.show
  given selected: ("selected" is Attributive of Boolean) = if _ then Unset else NotShown
  given shape: ("shape" is Attributive of Shape) = _.show
  given size: ("size" is Attributive of Int) = _.show
  given sizes: ("sizes" is Attributive of Text) = identity(_) // This should perhaps be a Map
  given slot: ("slot" is Attributive of Text) = identity(_)
  given span: ("span" is Attributive of Int) = _.show
  given spellcheck: ("spellcheck" is Attributive of Boolean) = if _ then Unset else NotShown
  given src: ("src" is Attributive of Text) = identity(_)

  given src2: [path: Abstractable across Paths to Text] => ("src" is Attributive of path) =
    _.generic

  given src3: [url: Abstractable across Urls to Text] => ("src" is Attributive of url) = _.generic

  inline given src4: [topic, path <: Path of topic under %.type] => Www is Filesystem
         => ("src" is Attributive):
    type Topic = path
    def convert(value: path): Text = value.on[Www].encode

  inline given src5: [topic, path <: Path of topic] => Www is Filesystem
         => ("src" is Attributive of path) =
    _.on[Www].encode

  given src5: [relative <: Relative on Www] => ("src" is Attributive of relative) = _.encode

  inline given src6: [topic, relative <: Relative of topic] => ("src" is Attributive):
    type Topic = relative
    def convert(value: relative): Text = value.on[Www].encode

  given srcdoc: ("srcdoc" is Attributive of OldHtml[?]) = _.show
  given srclang: ("srclang" is Attributive of Text) = identity(_)

  // This should be provided by Cataclysm
  given srcset: ("srcset" is Attributive of Text) = identity(_)

  given start: ("start" is Attributive of Int) = _.show
  given step: ("step" is Attributive of Double) = _.toString.show
  given style: ("style" is Attributive of Text) = identity(_) // Should be provided by Cataclysm
  given tabindex: ("tabindex" is Attributive of Int) = _.show
  given target: ("target" is Attributive of Target) = _.show
  given target2: ("target" is Attributive of DomId) = _.name
  given title: ("title" is Attributive of Text) = identity(_)
  given translate: ("translate" is Attributive of Boolean) = if _ then Unset else NotShown
  given linkType: ("type" is Attributive of MediaType onto "link") = _.show
  given buttonType: ("type" is Attributive of Text onto "button") = _.show
  given capture: ("capture" is Attributive of Capture) = _.show

  // This needs a representation of HTML names
  given usemap: ("usemap" is Attributive of Text) = identity(_)

  given value: ("value" is Attributive of Double) = _.toString.show
  given valueInt: ("value" is Attributive of Int) = _.show
  given valueText: ("value" is Attributive of Text) = identity(_)
  given width: ("width" is Attributive of Int) = _.show
  given wrap: ("wrap" is Attributive of Wrap) = _.show
