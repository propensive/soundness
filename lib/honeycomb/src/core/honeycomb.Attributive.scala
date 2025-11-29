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
import serpentine.*
import spectacular.*
import urticose.*
import vacuous.*

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
