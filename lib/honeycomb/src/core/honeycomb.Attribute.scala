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
┃    Soundness, version 0.45.0.                                                                    ┃
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

trait Attribute extends Targetable:
  type Self <: Label
  type Topic
  def convert(value: Topic): Optional[Text | Attribute.NotShown.type]
  def rename: Optional[Text] = Unset

object Attribute:
  object NotShown

  def apply[label <: Label: ValueOf, topic](name: Text)(lambda: topic => Text)
  : label is Attribute of topic =

      new Attribute:
        type Self = label
        type Topic = topic
        def convert(value: topic): Text = lambda(value)
        override def rename: Optional[Text] = label.value

  given generic: [label <: Label: GenericHtmlAttribute[value], value] => label is Attribute:
    type Topic = value
    def convert(value: value): Optional[Text] = label.serialize(value).show
    override def rename: Optional[Text] = label.name.show

  given accept: ("accept" is Attribute of List[Text]) = _.join(t",")
  given acceptCharset: ("acceptCharset" is Attribute of Encoding) = apply("accept-charset")(_.name)
  given accesskey: ("accesskey" is Attribute of Char) = _.show
  given action: ("action" is Attribute of Text) = identity(_)

  given action2: [url: Abstractable across Urls to Text] => ("action" is Attribute of url) =
    _.generic

  given allowfullscreen: ("allowfullscreen" is Attribute of Boolean) = _ => Unset
  given allowpaymentrequest: ("allowpaymentrequest" is Attribute of Boolean) = _ => Unset
  given alt: ("alt" is Attribute of Text) = identity(_)
  given async: ("async" is Attribute of Boolean) = _ => Unset
  given autocomplete: ("autocomplete" is Attribute of Autocomplete) = _.show
  given autocomplete2: ("autocomplete" is Attribute of Boolean) = t"off".unless(_)
  given autoplay: ("autoplay" is Attribute of Boolean) = _ => Unset
  given autofocus: ("autofocus" is Attribute of Boolean) = _ => Unset
  given border: ("border" is Attribute of Boolean) = if _ then t"1" else t""
  given charset: ("charset" is Attribute of Encoding) = _.name
  given checkedBoolean: ("checked" is Attribute of Boolean) = if _ then Unset else NotShown
  given cite: ("cite" is Attribute of Text) = identity(_)
  given cite2: [url: Abstractable across Urls to Text] => ("cite" is Attribute of url) = _.generic
  given `class`: ("class" is Attribute of List[CssClass]) = apply("class")(_.map(_.name).join(t" "))
  given class2: ("class" is Attribute of CssClass) = apply("class")(_.name)
  given code: ("code" is Attribute of Text) = identity(_) // MediaError
  given codebase: ("codebase" is Attribute of Text) = identity(_)

  given codebase2: [url: Abstractable across Urls to Text] => ("codebase" is Attribute of url) =
    _.generic

  given cols: ("cols" is Attribute of Int) = _.show
  given colspan: ("colspan" is Attribute of Int) = _.show
  given content: ("content" is Attribute of Text) = identity(_)
  given contenteditable: ("contenteditable" is Attribute of Boolean) = if _ then "true" else "false"
  given controls: ("controls" is Attribute of Boolean) = if _ then Unset else NotShown
  given coords: ("coords" is Attribute of Seq[Double]) = _.map(_.toString.show).join(t",")
  given crossorigin: ("crossorigin" is Attribute of Crossorigin) = _.show
  given data: ("data" is Attribute of Text) = identity(_)
  given data2: [url: Abstractable across Urls to Text] => ("data" is Attribute of url) = _.generic
  given datetime: ("datetime" is Attribute of Text) = identity(_) // To be provided by Aviation
  given default: ("default" is Attribute of Boolean) = if _ then Unset else NotShown
  given defer: ("defer" is Attribute of Boolean) = if _ then Unset else NotShown
  given dir: ("dir" is Attribute of HDir) = _.show
  given dirname: ("dirname" is Attribute of Text) = identity(_)
  given disabled: ("disabled" is Attribute of Boolean) = if _ then Unset else NotShown
  given download: ("download" is Attribute of Text) = identity(_)
  given draggable: ("draggable" is Attribute of Boolean) = if _ then Unset else NotShown
  given enctype: ("enctype" is Attribute of MediaType) = _.show
  given hfor: ("hfor" is Attribute of DomId) = apply("for")(_.name)
  given hfors: ("hfor" is Attribute of Seq[DomId]) = apply("for")(_.map(_.name).join(t" "))
  given `for`: ("for" is Attribute of DomId) = _.name
  given fors: ("for" is Attribute of Seq[DomId]) = _.map(_.name).join(t" ")
  given form: ("form" is Attribute of DomId) = _.name
  given formaction: ("formaction" is Attribute of Text) = identity(_)

  given formaction2: [url: Abstractable across Urls to Text] => ("formaction" is Attribute of url) =
    _.generic

  given formenctype: ("formenctype" is Attribute of Text) = identity(_)
  given formmethod: ("formmethod" is Attribute of Method) = _.show
  given formnovalidate: ("formnovalidate" is Attribute of Boolean) = if _ then Unset else NotShown
  given formtarget: ("formtarget" is Attribute of Target) = _.show
  given headers: ("headers" is Attribute of DomId) = _.name
  given headers2: ("headers" is Attribute of Set[DomId]) = _.map(_.name).join(t" ")
  given height: ("height" is Attribute of Int) = _.show
  given hidden: ("hidden" is Attribute of Boolean) = if _ then Unset else NotShown
  given high: ("high" is Attribute of Double) = _.toString.show
  given href: ("href" is Attribute of Text) = identity(_)
  given href2: [url: Abstractable across Urls to Text] => ("href" is Attribute of url) = _.generic

  inline given href3: [topic, path <: Path of topic under %.type] => Www is System
         => ("href" is Attribute):
    type Topic = path
    def convert(path: path): Text = path.on[Www].encode

  inline given href4: [topic, path <: Path of topic] => Www is System
         => ("href" is Attribute of path) =
    _.on[Www].encode

  inline given href4: [path <: Path on Www] => ("href" is Attribute of path) = _.encode

  given href5: [path: Abstractable across Paths to Text] => ("href" is Attribute of path) =
    _.generic

  given href6: [relative <: Relative on Www] => ("href" is Attribute of relative) = _.encode

  inline given href7: [topic, relative <: Relative of topic] => ("href" is Attribute):
    type Topic = relative
    def convert(value: relative): Text = value.on[Www].encode

  // Needs to be provided by Cosmopolite
  given hreflang: ("hreflang" is Attribute of Text) = identity(_)

  given httpEquiv: ("httpEquiv" is Attribute of HttpEquiv) = apply("http-equiv")(_.show)
  given id: ("id" is Attribute of DomId) = _.name
  given ismap: ("ismap" is Attribute of Boolean) = if _ then Unset else NotShown
  given kind: ("kind" is Attribute of Kind) = _.show
  given label: ("label" is Attribute of Text) = identity(_)
  given lang: ("lang" is Attribute of Text) = identity(_) // Should be provided by Cosmopolite
  given list: ("list" is Attribute of Seq[DomId]) = _.map(_.name).join(t" ")
  given list2: ("list" is Attribute of DomId) = _.name
  given loop: ("loop" is Attribute of Boolean) = if _ then Unset else NotShown
  given low: ("low" is Attribute of Double) = _.toString.tt
  given manifest: ("manifest" is Attribute of Text) = identity(_) // Provided by Scintillate

  given manifest2: [url: Abstractable across Urls to Text]
        => ("manifest" is Attribute of url) =
    _.generic

  given max: ("max" is Attribute of Double | Int) = _.toString.show
  given maxlength: ("maxlength" is Attribute of Int) = _.show
  given minlength: ("minlength" is Attribute of Int) = _.show
  given media: ("media" is Attribute of Text) = identity(_)
  given method: ("method" is Attribute of Method) = _.show
  given min: ("min" is Attribute of Double | Int) = _.toString.show
  given multiple: ("multiple" is Attribute of Boolean) = if _ then Unset else NotShown
  given muted: ("muted" is Attribute of Boolean) = if _ then Unset else NotShown
  given name: ("name" is Attribute of Text) = identity(_)
  given name2: ("name" is Attribute of Target onto "object") = _.show
  given name3: ("name" is Attribute of Target onto "iframe") = _.show
  given nonce: ("nonce" is Attribute of Text) = identity(_)
  given novalidate: ("novalidate" is Attribute of Boolean) = if _ then Unset else NotShown
  given open: ("open" is Attribute of Boolean) = if _ then Unset else NotShown
  given optimum: ("optimum" is Attribute of Double) = _.toString.show
  given pattern: ("pattern" is Attribute of Regex) = _.pattern
  given placeholder: ("placeholder" is Attribute of Text) = identity(_)
  given playsinline: ("playsinline" is Attribute of Boolean) = if _ then Unset else NotShown
  given poster: ("poster" is Attribute of Text) = identity(_)

  given poster2: [url: Abstractable across Urls to Text] => ("poster" is Attribute of url) =
    _.generic

  given preload: ("preload" is Attribute of Preload) = _.show
  given readonly: ("readonly" is Attribute of Boolean) = if _ then Unset else NotShown
  given referrerpolicy: ("referrerpolicy" is Attribute of Text) = identity(_)
  given rel: ("rel" is Attribute of Rel) = _.show
  given rel2: ("rel" is Attribute of Seq[Rel]) = _.map(_.show).join(t" ")
  given required: ("required" is Attribute of Boolean) = if _ then Unset else NotShown
  given rev: ("rev" is Attribute of Rev) = _.show
  given rows: ("rows" is Attribute of Int) = _.show
  given rowspan: ("rowspan" is Attribute of Int) = _.show
  given sandbox: ("sandbox" is Attribute of Sandbox) = _.show
  given scope: ("scope" is Attribute of Scope) = _.show
  given selected: ("selected" is Attribute of Boolean) = if _ then Unset else NotShown
  given shape: ("shape" is Attribute of Shape) = _.show
  given size: ("size" is Attribute of Int) = _.show
  given sizes: ("sizes" is Attribute of Text) = identity(_)
  given slot: ("slot" is Attribute of Text) = identity(_)
  given span: ("span" is Attribute of Int) = _.show
  given spellcheck: ("spellcheck" is Attribute of Boolean) = if _ then Unset else NotShown
  given src: ("src" is Attribute of Text) = identity(_)
  given src2: [path: Abstractable across Paths to Text] => ("src" is Attribute of path) = _.generic
  given src3: [url: Abstractable across Urls to Text] => ("src" is Attribute of url) = _.generic

  inline given src4: [topic, path <: Path of topic under %.type] => Www is System
         => ("src" is Attribute):
    type Topic = path
    def convert(value: path): Text = value.on[Www].encode

  inline given src5: [topic, path <: Path of topic] => Www is System
         => ("src" is Attribute of path) =
    _.on[Www].encode

  given src5: [relative <: Relative on Www] => ("src" is Attribute of relative) = _.encode

  inline given src6: [topic, relative <: Relative of topic] => ("src" is Attribute):
    type Topic = relative
    def convert(value: relative): Text = value.on[Www].encode

  given srcdoc: ("srcdoc" is Attribute of Html[?]) = _.show
  given srclang: ("srclang" is Attribute of Text) = identity(_)
  given srcset: ("srcset" is Attribute of Text) = identity(_)
  given start: ("start" is Attribute of Int) = _.show
  given step: ("step" is Attribute of Double) = _.toString.show
  given style: ("style" is Attribute of Text) = identity(_) // Should be provided by Cataclysm
  given tabindex: ("tabindex" is Attribute of Int) = _.show
  given target: ("target" is Attribute of Target) = _.show
  given target2: ("target" is Attribute of DomId) = _.name
  given title: ("title" is Attribute of Text) = identity(_)
  given translate: ("translate" is Attribute of Boolean) = if _ then Unset else NotShown
  given linkType: ("type" is Attribute of MediaType onto "link") = _.show
  given buttonType: ("type" is Attribute of Text onto "button") = _.show
  given capture: ("capture" is Attribute of Capture) = _.show
  given usemap: ("usemap" is Attribute of Text) = identity(_)
  given value: ("value" is Attribute of Double) = _.toString.show
  given valueInt: ("value" is Attribute of Int) = _.show
  given valueText: ("value" is Attribute of Text) = identity(_)
  given width: ("width" is Attribute of Int) = _.show
  given wrap: ("wrap" is Attribute of Wrap) = _.show
