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
┃    Soundness, version 0.43.0.                                                                    ┃
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

trait HtmlAttribute[-value] extends Targetable:
  type Self <: Label
  def convert(value: value): Optional[Text | HtmlAttribute.NotShown.type]
  def rename: Optional[Text] = Unset

object HtmlAttribute:
  object NotShown

  given generic: [label <: Label: GenericHtmlAttribute[value], value]
        =>  label is HtmlAttribute[value]:

    def convert(value: value): Optional[Text] = label.serialize(value).show
    override def rename: Optional[Text] = label.name.show

  given accept: ("accept" is HtmlAttribute[List[Text]]) = _.join(t",")

  given acceptCharset: ("acceptCharset" is HtmlAttribute[Encoding]):
    override def rename: Optional[Text] = t"accept-charset"
    def convert(value: Encoding): Text = value.name

  given accesskey: ("accesskey" is HtmlAttribute[Char]) = _.show
  given action: ("action" is HtmlAttribute[Text]) = identity(_)

  given action2: [url: Abstractable across Urls to Text] => ("action" is HtmlAttribute[url]) =
    _.generic

  given allowfullscreen: ("allowfullscreen" is HtmlAttribute[Boolean]) = _ => Unset
  given allowpaymentrequest: ("allowpaymentrequest" is HtmlAttribute[Boolean]) = _ => Unset
  given alt: ("alt" is HtmlAttribute[Text]) = identity(_)
  given async: ("async" is HtmlAttribute[Boolean]) = _ => Unset
  given autocomplete: ("autocomplete" is HtmlAttribute[Autocomplete]) = _.show
  given autocomplete2: ("autocomplete" is HtmlAttribute[Boolean]) = t"off".unless(_)
  given autoplay: ("autoplay" is HtmlAttribute[Boolean]) = _ => Unset
  given autofocus: ("autofocus" is HtmlAttribute[Boolean]) = _ => Unset
  given border: ("border" is HtmlAttribute[Boolean]) = if _ then t"1" else t""
  given charset: ("charset" is HtmlAttribute[Encoding]) = _.name
  given checkedBoolean: ("checked" is HtmlAttribute[Boolean]) = if _ then Unset else NotShown
  given cite: ("cite" is HtmlAttribute[Text]) = identity(_)

  given cite2: [url: Abstractable across Urls to Text] => ("cite" is HtmlAttribute[url]) =
    _.generic

  given `class`: ("class" is HtmlAttribute[List[CssClass]]):
    override def rename: Optional[Text] = t"class"
    def convert(value: List[CssClass]): Text = value.map(_.name).join(t" ")

  given class2: HtmlAttribute[CssClass]:
    override def rename: Optional[Text] = t"class"
    def convert(value: CssClass): Text = value.name

  given code: ("code" is HtmlAttribute[Text]) = identity(_) // MediaError
  given codebase: ("codebase" is HtmlAttribute[Text]) = identity(_)

  given codebase2: [url: Abstractable across Urls to Text] => ("codebase" is HtmlAttribute[url]) =
    _.generic

  given cols: ("cols" is HtmlAttribute[Int]) = _.show
  given colspan: ("colspan" is HtmlAttribute[Int]) = _.show
  given content: ("content" is HtmlAttribute[Text]) = identity(_)

  given contenteditable: ("contenteditable" is HtmlAttribute[Boolean]) =
    if _ then t"true" else t"false"

  given controls: ("controls" is HtmlAttribute[Boolean]) = if _ then Unset else NotShown
  given coords: ("coords" is HtmlAttribute[Seq[Double]]) = _.map(_.toString.show).join(t",")
  given crossorigin: ("crossorigin" is HtmlAttribute[Crossorigin]) = _.show
  given data: ("data" is HtmlAttribute[Text]) = identity(_)

  given data2: [url: Abstractable across Urls to Text] => ("data" is HtmlAttribute[url]) =
    _.generic

  given datetime: ("datetime" is HtmlAttribute[Text]) = identity(_) // To be provided by Aviation
  given default: ("default" is HtmlAttribute[Boolean]) = if _ then Unset else NotShown
  given defer: ("defer" is HtmlAttribute[Boolean]) = if _ then Unset else NotShown
  given dir: ("dir" is HtmlAttribute[HDir]) = _.show

  // Should be the name of an input in a form, followed by `.dir`
  given dirname: ("dirname" is HtmlAttribute[Text]) = identity(_)

  given disabled: ("disabled" is HtmlAttribute[Boolean]) = if _ then Unset else NotShown

  // should be a filename, but probably best as `Text`
  given download: ("download" is HtmlAttribute[Text]) = identity(_)

  given draggable: ("draggable" is HtmlAttribute[Boolean]) = if _ then Unset else NotShown
  given enctype: ("enctype" is HtmlAttribute[MediaType]) = _.show

  given hfor: ("hfor" is HtmlAttribute[DomId]):
    override def rename: Optional[Text] = t"for"
    def convert(value: DomId): Text = value.name

  given hfors: ("hfor" is HtmlAttribute[Seq[DomId]]):
    override def rename: Optional[Text] = t"for"
    def convert(value: Seq[DomId]): Text = value.map(_.name).join(t" ")

  given `for`: ("for" is HtmlAttribute[DomId]):
    def convert(value: DomId): Text = value.name

  given fors: ("for" is HtmlAttribute[Seq[DomId]]):
    def convert(value: Seq[DomId]): Text = value.map(_.name).join(t" ")

  given form: ("form" is HtmlAttribute[DomId]) = _.name
  given formaction: ("formaction" is HtmlAttribute[Text]) = identity(_) // Provided by Scintillate

  given formaction2: [url: Abstractable across Urls to Text]
        => ("formaction" is HtmlAttribute[url]) =
    _.generic

  given formenctype: ("formenctype" is HtmlAttribute[Text]) = identity(_)
  given formmethod: ("formmethod" is HtmlAttribute[Method]) = _.show
  given formnovalidate: ("formnovalidate" is HtmlAttribute[Boolean]) = if _ then Unset else NotShown
  given formtarget: ("formtarget" is HtmlAttribute[Target]) = _.show
  given headers: ("headers" is HtmlAttribute[DomId]) = _.name
  given headers2: ("headers" is HtmlAttribute[Set[DomId]]) = _.map(_.name).join(t" ")
  given height: ("height" is HtmlAttribute[Int]) = _.show
  given hidden: ("hidden" is HtmlAttribute[Boolean]) = if _ then Unset else NotShown
  given high: ("high" is HtmlAttribute[Double]) = _.toString.show
  given href: ("href" is HtmlAttribute[Text]) = identity(_)

  given href2: [url: Abstractable across Urls to Text] => ("href" is HtmlAttribute[url]) =
    _.generic

  inline given href3: [topic, path <: Path of topic under %.type] => UrlSpace is System
         => ("href" is HtmlAttribute[path]) =
    _.on[UrlSpace].encode

  inline given href4: [topic, path <: Path of topic] => UrlSpace is System
         => ("href" is HtmlAttribute[path]) =
    _.on[UrlSpace].encode

  inline given href4: [path <: Path on UrlSpace] => ("href" is HtmlAttribute[path]) = _.encode

  inline given href5: [relative <: Relative on UrlSpace] => ("href" is HtmlAttribute[relative]) =
    _.encode

  given href6: [path: Abstractable across Paths to Text] => ("href" is HtmlAttribute[path]) =
    _.generic

  // Needs to be provided by Cosmopolite
  given hreflang: ("hreflang" is HtmlAttribute[Text]) = identity(_)

  given httpEquiv: ("httpEquiv" is HtmlAttribute[HttpEquiv]):
    override def rename: Optional[Text] = t"http-equiv"
    def convert(value: HttpEquiv): Text = value.show

  given id: ("id" is HtmlAttribute[DomId]) = _.name
  given ismap: ("ismap" is HtmlAttribute[Boolean]) = if _ then Unset else NotShown
  given kind: ("kind" is HtmlAttribute[Kind]) = _.show
  given label: ("label" is HtmlAttribute[Text]) = identity(_)
  given lang: ("lang" is HtmlAttribute[Text]) = identity(_) // Should be provided by Cosmopolite
  given list: ("list" is HtmlAttribute[Seq[DomId]]) = _.map(_.name).join(t" ")
  given list2: ("list" is HtmlAttribute[DomId]) = _.name
  given loop: ("loop" is HtmlAttribute[Boolean]) = if _ then Unset else NotShown
  given low: ("low" is HtmlAttribute[Double]) = _.toString.tt
  given manifest: ("manifest" is HtmlAttribute[Text]) = identity(_) // Provided by Scintillate

  given manifest2: [url: Abstractable across Urls to Text]
        => ("manifest" is HtmlAttribute[url]) =
    _.generic

  given max: ("max" is HtmlAttribute[Double | Int]) = _.toString.show
  given maxlength: ("maxlength" is HtmlAttribute[Int]) = _.show
  given minlength: ("minlength" is HtmlAttribute[Int]) = _.show
  given media: ("media" is HtmlAttribute[Text]) = identity(_) // Should be provided by Cataclysm
  given method: ("method" is HtmlAttribute[Method]) = _.show
  given min: ("min" is HtmlAttribute[Double | Int]) = _.toString.show
  given multiple: ("multiple" is HtmlAttribute[Boolean]) = if _ then Unset else NotShown
  given muted: ("muted" is HtmlAttribute[Boolean]) = if _ then Unset else NotShown

  // Should provide special `name` identifiers
  given name: ("name" is HtmlAttribute[Text]) = identity(_)

  given name2: ("name" is HtmlAttribute[Target] onto "object") = _.show
  given name3: ("name" is HtmlAttribute[Target] onto "iframe") = _.show
  given nonce: ("nonce" is HtmlAttribute[Text]) = identity(_) // Should be provided by Gastronomy
  given novalidate: ("novalidate" is HtmlAttribute[Boolean]) = if _ then Unset else NotShown
  given open: ("open" is HtmlAttribute[Boolean]) = if _ then Unset else NotShown
  given optimum: ("optimum" is HtmlAttribute[Double]) = _.toString.show
  given pattern: ("pattern" is HtmlAttribute[Regex]) = _.pattern
  given placeholder: ("placeholder" is HtmlAttribute[Text]) = identity(_)
  given playsinline: ("playsinline" is HtmlAttribute[Boolean]) = if _ then Unset else NotShown
  given poster: ("poster" is HtmlAttribute[Text]) = identity(_)

  given poster2: [url: Abstractable across Urls to Text] => ("poster" is HtmlAttribute[url]) =
    _.generic

  given preload: ("preload" is HtmlAttribute[Preload]) = _.show
  given readonly: ("readonly" is HtmlAttribute[Boolean]) = if _ then Unset else NotShown
  given referrerpolicy: ("referrerpolicy" is HtmlAttribute[Text]) = identity(_)
  given rel: ("rel" is HtmlAttribute[Rel]) = _.show
  given rel2: ("rel" is HtmlAttribute[Seq[Rel]]) = _.map(_.show).join(t" ")
  given required: ("required" is HtmlAttribute[Boolean]) = if _ then Unset else NotShown
  given rev: ("rev" is HtmlAttribute[Rev]) = _.show
  given rows: ("rows" is HtmlAttribute[Int]) = _.show
  given rowspan: ("rowspan" is HtmlAttribute[Int]) = _.show
  given sandbox: ("sandbox" is HtmlAttribute[Sandbox]) = _.show
  given scope: ("scope" is HtmlAttribute[Scope]) = _.show
  given selected: ("selected" is HtmlAttribute[Boolean]) = if _ then Unset else NotShown
  given shape: ("shape" is HtmlAttribute[Shape]) = _.show
  given size: ("size" is HtmlAttribute[Int]) = _.show
  given sizes: ("sizes" is HtmlAttribute[Text]) = identity(_) // This should perhaps be a Map
  given slot: ("slot" is HtmlAttribute[Text]) = identity(_)
  given span: ("span" is HtmlAttribute[Int]) = _.show
  given spellcheck: ("spellcheck" is HtmlAttribute[Boolean]) = if _ then Unset else NotShown
  given src: ("src" is HtmlAttribute[Text]) = identity(_)

  given src2: [path: Abstractable across Paths to Text] => ("src" is HtmlAttribute[path]) =
    _.generic

  given src3: [url: Abstractable across Urls to Text] => ("src" is HtmlAttribute[url]) = _.generic

  inline given src4: [topic, path <: Path of topic under %.type] => UrlSpace is System
         => ("src" is HtmlAttribute[path]) =
    _.on[UrlSpace].encode

  inline given src5: [topic, path <: Path of topic] => UrlSpace is System
         => ("src" is HtmlAttribute[path]) =
    _.on[UrlSpace].encode

  inline given src5: [relative <: Relative on UrlSpace] => ("src" is HtmlAttribute[relative]) =
    _.encode

  given srcdoc: ("srcdoc" is HtmlAttribute[Html[?]]) = _.show
  given srclang: ("srclang" is HtmlAttribute[Text]) = identity(_)

  // This should be provided by Cataclysm
  given srcset: ("srcset" is HtmlAttribute[Text]) = identity(_)

  given start: ("start" is HtmlAttribute[Int]) = _.show
  given step: ("step" is HtmlAttribute[Double]) = _.toString.show
  given style: ("style" is HtmlAttribute[Text]) = identity(_) // Should be provided by Cataclysm
  given tabindex: ("tabindex" is HtmlAttribute[Int]) = _.show
  given target: ("target" is HtmlAttribute[Target]) = _.show
  given target2: ("target" is HtmlAttribute[DomId]) = _.name
  given title: ("title" is HtmlAttribute[Text]) = identity(_)
  given translate: ("translate" is HtmlAttribute[Boolean]) = if _ then Unset else NotShown
  given linkType: ("type" is HtmlAttribute[MediaType] onto "link") = _.show
  given buttonType: ("type" is HtmlAttribute[Text] onto "button") = _.show
  given capture: ("capture" is HtmlAttribute[Capture]) = _.show

  // This needs a representation of HTML names
  given usemap: ("usemap" is HtmlAttribute[Text]) = identity(_)

  given value: ("value" is HtmlAttribute[Double]) = _.toString.show
  given valueInt: ("value" is HtmlAttribute[Int]) = _.show
  given valueText: ("value" is HtmlAttribute[Text]) = identity(_)
  given width: ("width" is HtmlAttribute[Int]) = _.show
  given wrap: ("wrap" is HtmlAttribute[Wrap]) = _.show
